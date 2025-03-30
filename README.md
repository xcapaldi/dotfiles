# dotfiles

My dotfiles leverage [chezmoi](https://www.chezmoi.io/) to manage:

* MacOS work machine
* WSL2 Ubuntu system under Windows 11
* Ubuntu hacktop when I can't be at the desktop

I used to use Linux as my primary machine and under those circumstances I rolled with a minimal https://fedoraproject.org/[Fedora] setup.
However since building a Windows PC, WSL only officially supports Ubuntu and OpenSUSE.
It's easiest to keep the configuration consistent between machines so now I have Ubuntu both in WSL and on my hacktop.
In all cases I've slowly steered away from super complex configurations in favor of using default functionality.

## MacOS work machine
Not much to say here... chezmoi takes care of installing [Homebrew](https://brew.sh/) packages and global [asdf](https://asdf-vm.com/) tool versions.
It also manages my `zshrc`, `psqlrc` git and ssh configs.
There is some additional corporate configuration that must be done manually on a fresh install.

## Windows Host and WSL2
I don't manage the host Windows 11 system with chezmoi because configuration is minimal and I only install a few packages using Winget.

> [!NOTE]
> You may need to update the Windows Store before Winget works properly.


``` powershell
winget install Bitwarden.Bitwarden
winget install Discord.Discord
winget install Mozilla.Firefox
winget install Valve.Steam
winget install Syncthing.Syncthing
```

Then setup WSL2 which defaults to Ubuntu.
A few other linux distros are supported but Canonical works closely with Microsoft for WSL so it is probably the most stable.

``` powershell
wsl --install
```

After setting up a username and password, you'll need to restart before using your Linux subsystem.
Once inside, you can install Chezmoi and initialize with this repository directly:

``` shell
sudo snap refresh
sudo snap install chezmoi --classic
chezmoi init https://github.com/xcapaldi/dotfiles.git
```

## Ubuntu
Since my main desktop is running Ubuntu under WSL, Windows is taking care of window management.
The only GUI application running from Ubuntu is Emacs anyway.
I use my hacktop exclusively for personal project development so I keep the system as minimal as possible.
I don't even transport it much since the battery is ancient.
This means I could omit audio control, media codecs, wifi, display manager and utilities for external drives; all things I would use in my daily driver.
In the interest of time though, [xubuntu](https://xubuntu.org/) is probably sufficient.

### Installing a minimal system
Use [Ubuntu server](https://ubuntu.com/download/server) installer to get a super minimal system.
Once inside, install chezmoi and initialize:

``` shell
sudo snap refresh
sudo snap install chezmoi --classic
chezmoi init https://github.com/xcapaldi/dotfiles.git
```

This will take care of installing a simple window manager; [jwm](https://joewing.net/projects/jwm/).
Then I can simply login via tty and run `startx` to get into a graphical session.
Since I'm exclusively using this machine to work on personal projects I usually just have Emacs, Vivaldi and a terminal running.

### Use posix-compliant shell (unused currently)
Alias `sh` to `dash` instead of `bash`.
This should speed up posix-compliant scripts.

``` shell
sudo apt-get install dash
sudo rm /bin/sh
sudo ln -s /bin/dash /bin/sh
```

### Email (unused currently)
Fetching email locally can be the most complex configuration managed via Chezmoi.
I am trying Gnus inside Emacs as my email client (instead of `isync` + `notmuch` + `msmtp`).
Gnus relies on the `.authinfo` file to contain mail server information and credentials.
Gnus' own configuration includes some personal information as well.
While GPG is standard for encryption, I prefer to use [age](https://github.com/FiloSottile/age) because it is far easier use for this purpose.
I generate two keyfiles; one for Chezmoi's native encrypted configuration support and one for general use.
The Gnus configuration (`gnus.el`) is encrypted by Chezmoi using that key: `encrypted_gnus.el.age`.
This means inside the repo, it is encrypted.
However on my local machine, it lives in plain text.
This would work for the `.authinfo` as well but it is best practice to keep it encrypted even locally so the passwords are not stored in plain text.
So `.authinfo` is encrypted with the general key (`.authinfo.age`) even locally.
These keys are the only important pieces that are not maintained with the rest of my configuration.

