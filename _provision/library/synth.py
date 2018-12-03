#!/usr/bin/env python
import sys
import collections

from ansible.module_utils.basic import AnsibleModule


SynthResult = collections.namedtuple("SynthResult", ["failed", "changed", "message"])


if sys.version_info[0] == 2:

    def iteritems(d):
        return d.iteritems()


else:

    def iteritems(d):
        return d.items()


class SynthException(Exception):
    pass


class Synth(object):
    def __init__(self, **kwargs):
        for key, value in iteritems(kwargs):
            setattr(self, key, value)

    def run(self):
        try:
            if self.upgrade_system:
                return self._command_upgrade_system()
            elif self.packages is not None:
                return getattr(self, "_state_%s" % self.state)()
        except SynthException as e:
            message = str(e)
            return self._failed(message)
        return self._message("no action was performed.")

    #
    # Helpers
    #

    def _synth(self, *args):
        return self.module.run_command(["synth"] + list(args))

    def _pkg(self, *args):
        return self.module.run_command(["pkg"] + list(args))

    def _check_package(self, pkg):
        rc, out, err = self._pkg("query", "%t", pkg)
        return rc == 0 and out != ""

    def _install(self, pkgs):
        cmd = "install"
        if self.force:
            cmd = "force"
        rc, out, err = self._synth(cmd, *pkgs)
        if (
            out.find("Installing") == -1
            and out.find("Upgrading") == -1
            and out.find("already installed") == -1
        ):
            raise SynthException(out.strip())
        return out

    def _delete(self, pkgs):
        rc, out, err = self._pkg("delete", "-y", *pkgs)
        if rc != 0:
            raise SynthException(err)
        return out

    #
    # Result
    #

    def _failed(self, message):
        return SynthResult(failed=True, changed=None, message=message)

    def _changed(self, message):
        return SynthResult(failed=None, changed=True, message=message)

    def _message(self, message):
        return SynthResult(failed=None, changed=None, message=message)

    #
    # Commands
    #

    def _command_upgrade_system(self):
        rc, out, err = self._synth("upgrade-system")
        if out.find("Your packages are up to date") != -1:
            return self._message("package(s) are up-to-date")
        return self._changed("package(s) has been upgraded")

    #
    # Package states
    #

    def _state_present(self):
        pkgs = []
        for pkg in self.packages:
            if self._check_package(pkg):
                continue
            pkgs.append(pkg)
        if pkgs:
            out = self._install(pkgs)
            if (
                out.find("The most recent version of packages are already installed")
                == -1
            ):
                return self._changed("installed %s package(s)" % len(pkgs))
        return self._message("no package(s) was installed")

    def _state_absent(self):
        pkgs = []
        for pkg in self.packages:
            if not self._check_package(pkg):
                continue
            pkgs.append(pkg)
        if pkgs:
            self._delete(pkgs)
            return self._changed("uninstalled %s package(s)" % len(pkgs))
        return self._message("package(s) is already uninstalled")

    def _state_latest(self):
        out = self._install(self.packages)
        if out.find("The most recent version of packages are already installed") != -1:
            return self._message("package(s) are up-to-date")
        return self._changed("package(s) has been upgraded")


def main():
    module = AnsibleModule(
        supports_check_mode=True,
        argument_spec=dict(
            name=dict(aliases=["pkg", "name"], required=False, type="list"),
            state=dict(default="present", choices=["present", "absent", "latest"]),
            force=dict(required=False, type="bool"),
            upgrade_system=dict(required=False, type="bool"),
        ),
    )

    module.run_command_environ_update = dict(
        LANG="C", LC_ALL="C", LC_MESSAGES="C", LC_CTYPE="C"
    )

    packages = None
    if module.params["name"]:
        packages = module.params["name"]

    synth = Synth(
        module=module,
        packages=packages,
        state=module.params["state"],
        force=module.params["force"],
        upgrade_system=module.params["upgrade_system"],
    )

    result = synth.run()
    if result.failed:
        module.fail_json(msg=result.message)
    else:
        module.exit_json(changed=result.changed, msg=result.message)


if __name__ == "__main__":
    main()
