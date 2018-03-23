#!/usr/bin/env python3
import sys
from ansible.module_utils.basic import *
from collections import namedtuple


PacAurResult = namedtuple('PacAurResult', [
    'failed',
    'changed',
    'message',
])


class PacAurException(Exception):
    pass


class PacAur(object):

    def __init__(self, **kwargs):
        for key, value in iter(kwargs):
            setattr(self, key, value)

    def run(self):
        try:
            if self.update_cache:
                return self._command_update_cache()
            elif self.upgrade:
                return self._command_upgrade()
            elif self.packages is not None:
                return getattr(self, '_state_%s' % self.state)()
        except PacAurException as e:
            message = str(e)
            return self._failed(message)
        return self._message('no action was performed')

    #
    # Helpers
    #

    def _pacaur(self, *args):
        cmd = [self.pacaur_path]
        cmd += list(args)
        return self.module.run_command(cmd)

    def _is_installed(self, pkg):
        status, out, err = self._pacaur('-Q', pkg)
        return not err

    def _install(self, pkg):
        status, out, err = self._pacaur(
            '-S',
            '--needed',
            '--noconfirm',
            '--noedit', pkg)
        if status != 0:
            raise PacAurException(
                'an error occurred while installing %s: %s' % (pkg, err))
        return status, out, err

    def _uninstall(self, pkg):
        status, out, err = self._pacaur('-Rns', '--noconfirm', pkg)
        if status != 0:
            raise PacAurException(
                'an error occurred while uninstalling %s: %s' % (pkg, err))
        return status, out, err

    #
    # Results
    #

    def _failed(self, message):
        return PacAurResult(failed=True, changed=None, message=message)

    def _changed(self, message):
        return PacAurResult(failed=None, changed=True, message=message)

    def _message(self, message):
        return PacAurResult(failed=None, changed=None, message=message)

    #
    # Commands
    #

    def _command_update_cache(self):
        status, out, err = self._pacaur('-k')
        if status == 0 and out != '':
            return self._changed('cache has been successfully updated')
        return self._message('cache are up-to-date')

    def _command_upgrade(self):
        status, out, err = self._pacaur('-kuq', '--noconfirm')
        if status == 0 and not 'there is nothing to do' in out:
            return self._changed('packages has been succesfully upgraded')
        return self._message('all packages are up-to-date')

    #
    # States
    #

    def _state_present(self):
        i = 0
        for pkg in self.packages:
            if self._is_installed(pkg):
                continue
            status, out, err = self._install(pkg)
            i += 1
        if i:
            return self._changed('installed %s packages' % i)
        return self._message('no package was installed')

    def _state_latest(self):
        i = 0
        u = 0
        for pkg in self.packages:
            if not self._is_installed(pkg):
                status, out, err = self._install(pkg)
                i += 1
            else:
                status, out, err = self._install(pkg)
                if not 'there is nothing to do' in out:
                    u += 1

        if u and not i:
            return self._changed('upgraded %s package(s)' % u)
        elif u and i:
            return self._changed(
                'installed %s and upgraded %s package(s)' % (i, u))
        elif i and not u:
            return self._changed('installed %s package(s)' % u)
        return self._message('package is already up-to-date')

    def _state_absent(self):
        a = 0
        for pkg in self.packages:
            if self._is_installed(pkg):
                self._uninstall(pkg)
                a += 1
        if a:
            return self._changed('uninstalled %s package(s)' % a)
        return self._message('package is already uninstalled')


def main():
    module = AnsibleModule(
        supports_check_mode=False,
        argument_spec=dict(
            name=dict(aliases=['pkg', 'name'], required=False, type='list'),
            update_cache=dict(required=False, type='bool'),
            upgrade=dict(required=False, type='bool'),
            state=dict(default='present', choices=[
                'present',
                'latest',
                'absent',
            ])
        )
    )

    packages = None
    if module.params['name']:
        packages = module.params['name']

    pacaur_path = module.get_bin_path('pacaur', True)
    pacaur = PacAur(
        module=module,
        packages=packages,
        pacaur_path=pacaur_path,
        state=module.params['state'],
        update_cache=module.params['update_cache'],
        upgrade=module.params['upgrade'],
    )

    result = pacaur.run()
    if result.failed:
        module.fail_json(msg=result.message)
    else:
        module.exit_json(msg=result.message, changed=result.changed)


if __name__ == '__main__':
    main()
