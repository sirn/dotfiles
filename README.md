# Dotfiles

This repository hosts my personal dotfiles as well as machine provisioning and workflow scripts. This repository come with no warranty. It may wipe your boot disk, eat your pet, etc. I like to experiment with my setup and this repository reflect that. Use it at your own risk.

## Usage

```shell
$ mkdir -p $HOME/.dotfiles
$ cd $HOME/.dotfiles
$ curl -sSL https://git.sr.ht/~sirn/dotfiles/archive/master.tar.gz | tar -xvzf - --strip-components=1
$ ./bootstrap.sh
```

## How it works

The primary endpoint of this repository is `bootstrap.sh`, which is a shell script written in [POSIX sh](https://askubuntu.com/questions/1059265/). The script is used to setup packages, configuring current user environment, and configuring the system.

The `bootstrap.sh` script has few configurations available, namely:

-   Profile: a script entrypoint
-   Flavor: a feature flag used in the profile
-   Lookup path: a path to discover a profile

The `bootstrap.sh` script itself does nothing but handling arguments and dispatching to an appropriate entrypoint script located in `libexec/bootstrap` at each lookup path location. The script to be executed will depends on the name of the profile and the operating it is running.

For example, running a `pkg` profile (`-p pkg`) on macOS that has lookup path of `/foo` and `/bar` (`-l /foo -l /bar`) will result in `/foo/libexec/bootstrap/pkg_darwin.sh` and `/bar/libexec/bootstrap/pkg_darwin.sh` getting called.

When the entrypoint script get called, `bootstrap.sh` will also pass flavors as script arguments. The entrypoint script may use the given flavor to selectively install packages according to the flavor. For scripts that use external data sources (such as `var/bootstrap/pkglist.txt`), they will lookup such source in the following order:

-   `file/path/source.txt`
-   `file/path/source.flavor.txt`
-   `file/path/osname/source.txt`
-   `file/path/osname/source.flavor.txt`

This allow me to have separate sets of packages to be installed for GUI desktop-enabled machine (`pkglist.desktop.txt`) and for terminal use (`pkglist.txt`), and in other situations, without having to install unneeded packages. `var/bootstrap/darwin` is a good example for this.

This repository and bootstrap scripts are designed for personal needs, but if you have any questions about the setup, feel free to contact me at [sirn@ogsite.net](mailto:sirn@ogsite.net).

## FAQs

### Why?

I like to make my system reproducible. I mostly work on iPad Pro, by using [Blink.sh](http://www.blink.sh) to remote into a workspace running FreeBSD or OpenBSD depending on the phase of the moon. I also work on macOS sometimes, and I like to be able to run a single command to get everything in sync.

### Why not [Ansible](https://www.ansible.com)?

The `bootstrap.sh` script is in fact evolved from Ansible! Ansible has serve this repository well for several years, however to use Ansible to bootstrap the current system from scratch require some sort of shell script to bootstrap Ansible and the packaging system itself.

This led to a problem: I need to have half of the bootstrap process in shell script and another half in Ansible. For example, in case of Darwin (macOS), I need to install SDK, install Homebrew, install Python 3 and install Ansible from a shell script to be able to provision a local machine.

One day, I thought, why don't I try to do everything in shell script? POSIX sh, even, to ensure compatibility. The result is the current incarnation of this repository.

### What is `.dotpriv`?

`.dotpriv` is a private dotfiles repository that supplement this repository using lookup path mechanism described in _How it works_. It contained configurations not published here due to privacy and security reasons.

### Congratulations on building a [Rube Goldberg machine](https://en.wikipedia.org/wiki/Rube_Goldberg_machine)

That's not a question.

## License

Public domain
