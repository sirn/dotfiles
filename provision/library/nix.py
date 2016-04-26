#!/usr/bin/env python
from ansible.module_utils.basic import *
from collections import namedtuple


NixPkgResult = namedtuple('NixPkgResult', [
    'failed',
    'changed',
    'message',
])


class NixPkgException(Exception):
    pass


class NixPkg(object):
    def __init__(self, **kwargs):
        for key, value in kwargs.iteritems():
            setattr(self, key, value)

    def run(self):
        try:
            if self.upgrade_packages:
                return self._command_upgrade_packages()
            elif self.update_channels:
                return self._command_update_channels()
            elif self.packages is not None:
                return getattr(self, '_state_%s' % self.state)()
        except NixPkgException as e:
            message = str(e)
            return self._failed(message)
        return self._message('no action was performed.')

    #
    # Helpers
    #

    def _nix_env(self, *args):
        cmd = ['nix-env']
        if self.expr is not None:
            cmd += ['--file', self.expr]
        cmd += list(args)
        return self.module.run_command(cmd)

    def _nix_channel(self, *args):
        cmd = ['nix-channel'] + list(args)
        return self.module.run_command(cmd)

    def _check_package(self, pkg):
        if self.attr:
            status, out, err = self._nix_env('-qasA', pkg)
            return status == 0 and out[0] == "I"
        else:
            status, out, err = self._nix_env('--query', pkg)
            return status == 0
        return False

    def _install(self, pkg):
        cmd = ['--install']
        if self.attr:
            cmd += ['--attr']
        status, out, err = self._nix_env(*(cmd + [pkg]))
        if status != 0:
            raise NixPkgException(
                'an error occurred while installing %s: %s' % (pkg, err))
        return out

    def _upgrade(self, pkg):
        cmd = ['--upgrade']
        if self.attr:
            cmd += ['--attr']
        status, out, err = self._nix_env(*(cmd + [pkg]))
        if status != 0:
            msg = 'an error occurred while upgrading %s: %s' % (pkg, err)
            raise NixPkgException(msg)
        return out

    def _uninstall(self, pkg):
        status, out, err = self._nix_env('--uninstall', pkg)
        if status != 0:
            msg = 'an error occurred while uninstalling %s: %s' % (pkg, err)
            raise NixPkgException(msg)
        return out

    #
    # Result
    #

    def _failed(self, message):
        return NixPkgResult(failed=True, changed=None, message=message)

    def _changed(self, message):
        return NixPkgResult(failed=None, changed=True, message=message)

    def _message(self, message):
        return NixPkgResult(failed=None, changed=None, message=message)

    #
    # Commands
    #

    def _command_upgrade_packages(self):
        status, out, err = self._nix_env('--upgrade')
        if status != 0:
            raise NixPkgException(
                'an error occurred while upgrading packages: %s' % (err,))
        if out != '':
            return self._changed('packages has been succesfully upgraded')
        return self._message('all packages are up-to-date')

    def _command_update_channels(self):
        status, out, err = self._nix_channel('--update')
        if status != 0:
            raise NixPkgException(
                'an error occurred while upgrading packages: %s' % (err,))
        if out != '':
            return self._changed('channels has been succesfully updated')
        return self._message('channels are up-to-date')

    #
    # Package states
    #

    def _state_present(self):
        i = 0
        for pkg in self.packages:
            if self._check_package(pkg):
                continue
            self._install(pkg)
            i += 1
        if i:
            return self._changed('installed %s package(s)' % i)
        return self._message('no package was installed')

    def _state_latest(self):
        u = 0
        i = 0
        for pkg in self.packages:
            if not self._check_package(pkg):
                self._install(pkg)
                i += 1
            else:
                out = self._upgrade(pkg)
                if out != '':
                    u += 1
        if u and not i:
            return self._changed('upgraded %s package(s)' % u)
        elif u and i:
            msg = 'installed %s and upgraded %s package(s)' % (i, u)
            return self._changed(msg)
        elif i and not u:
            return self._changed('installed %s package(s)' % u)
        return self._message('package is already up-to-date')

    def _state_absent(self):
        a = 0
        for pkg in self.packages:
            if not self._check_package(pkg):
                continue
            self._uninstall(pkg)
            a += 1
        if a:
            return self._changed('uninstalled %s package(s)' % a)
        return self._message('package is already uninstalled')


def main():
    module = AnsibleModule(
        supports_check_mode=True,
        argument_spec=dict(
            name=dict(aliases=['pkg', 'name'], required=False, type='list'),
            expr=dict(require=False, default=None),
            attr=dict(require=False, default=None, type='bool'),
            update_channels=dict(required=False, type='bool'),
            upgrade_packages=dict(required=False, type='bool'),
            state=dict(default='present', choices=[
                'present',
                'latest',
                'absent',
            ]),
        ),
    )

    module.run_command_environ_update = dict(
        LANG='C',
        LC_ALL='C',
        LC_MESSAGES='C',
        LC_CTYPE='C',
    )

    packages = None
    if module.params['name']:
        packages = module.params['name']

    nixpkg = NixPkg(
        module=module,
        packages=packages,
        state=module.params['state'],
        update_channels=module.params['update_channels'],
        upgrade_packages=module.params['upgrade_packages'],
        expr=module.params['expr'],
        attr=module.params['attr'],
    )

    result = nixpkg.run()
    if result.failed:
        module.fail_json(msg=result.message)
    else:
        module.exit_json(changed=result.changed, msg=result.message)


if __name__ == '__main__':
    main()
