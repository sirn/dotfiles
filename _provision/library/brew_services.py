#!/usr/bin/env python
import sys
import collections

from ansible.module_utils.basic import AnsibleModule


BrewServicesResult = collections.namedtuple(
    "BrewServices", ["failed", "changed", "message"]
)


if sys.version_info[0] == 2:

    def iteritems(d):
        return d.iteritems()


else:

    def iteritems(d):
        return d.items()


class BrewServicesException(Exception):
    pass


class BrewServices(object):
    def __init__(self, **kwargs):
        for key, value in iteritems(kwargs):
            setattr(self, key, value)

    def run(self):
        try:
            return getattr(self, "_state_%s" % self.state)()
        except BrewServicesException as e:
            message = str(e)
            return self._failed(message)
        return self._message("no action was performed.")

    #
    # Helpers
    #

    def _brew_services(self, *args):
        return self.module.run_command(["brew", "services"] + list(args))

    def _get_state(self, name):
        rc, out, err = self._brew_services("list")
        for l in out.splitlines()[1:]:
            s_name, s_state, *s_args = l.split(None, 3)
            if s_name != name:
                continue
            return s_name, s_state

    #
    # Result
    #

    def _failed(self, message):
        return BrewServicesResult(failed=True, changed=None, message=message)

    def _changed(self, message):
        return BrewServicesResult(failed=None, changed=True, message=message)

    def _message(self, message):
        return BrewServicesResult(failed=None, changed=None, message=message)

    #
    # Service states
    #

    def _state_started(self):
        retval = self._get_state(self.name)
        if retval is None:
            raise BrewServicesException("%s is not a valid service name" % (self.name,))
        name, state = retval
        if state == "started":
            return self._message("%s is already running" % (self.name,))
        rc, out, err = self._brew_services("start", self.name)
        return self._changed("%s has been successfully started" % (self.name,))

    def _state_restarted(self):
        rc, out, err = self._brew_services("restart", self.name)
        if rc != 0:
            raise BrewServicesException("%s is not a valid service name" % (self.name,))
        rc, out, err = self._brew_services("restart", self.name)
        return self._changed("%s has been successfully restarted" % (self.name,))

    def _state_stopped(self):
        retval = self._get_state(self.name)
        if retval is None:
            raise BrewServicesException("%s is not a valid service name" % (self.name,))
        name, state = retval
        if state == "stopped":
            return self._message("%s is not already running" % (self.name,))
        rc, out, err = self._brew_services("stop", self.name)
        return self._changed("%s has been successfully stopped" % (self.name,))


def main():
    module = AnsibleModule(
        supports_check_mode=True,
        argument_spec=dict(
            name=dict(required=True, type="str"),
            state=dict(default="started", choices=["started", "stopped", "restarted"]),
        ),
    )

    module.run_command_environ_update = dict(
        LANG="C", LC_ALL="C", LC_MESSAGES="C", LC_CTYPE="C"
    )

    brew_services = BrewServices(
        module=module, name=module.params["name"], state=module.params["state"]
    )

    result = brew_services.run()
    if result.failed:
        module.fail_json(msg=result.message)
    else:
        module.exit_json(changed=result.changed, msg=result.message)


if __name__ == "__main__":
    main()
