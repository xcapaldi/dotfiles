# My dotfiles using [Chezmoi](https://www.chezmoi.io/)
Note that this is currently used to configure my personal openSUSE Tumbleweed box, my work MacOS box and my WSL2 Ubuntu system under Windows 11.
In all cases I've slowly steered away from super complex configurations in favor of using default functionality.
I would prefer to spend time potentially lost in a rabbit hole of configuration on real projects.

## WSL2 Ubuntu
I don't manage the host Windows 11 system with Chezmoi because configuration is minimal and I only install a few packages using Winget.
Note that you may need to update the Windows Store before Winget works properly.

```Powershell
winget install Bitwarden.Bitwarden
winget install Discord.Discord
winget install Mozilla.Firefox
winget install Valve.Steam
```

Then setup WSL2 which defaults to Ubuntu.
A few other linux distros are supported but Canonical works closely with Microsoft for WSL so it is probably the most stable.

```Powershell
wsl --install
```

After setting up a username and password, you'll need to restart before using your Linux subsystem.
Once inside, you can install Chezmoi and initialize with this repository directly:

```Shell
sh -c "$(curl -fsLS get.chezmoi.io)" -- init --apply xcapaldi
```

## Personal linux box
I prefer to use [openSUSE Tumbleweed Linux](https://www.opensuse.org/#Tumbleweed) as it is a well-behaved modern Linux distribution that is dedicated to open-source.
I previously used Fedora linux but I grew tired of the quick release schedule.
Now that I am working, I no longer spend my entire day on my personal machine so I am favoring a stable rolling-release distribution that requires less-frequent upkeep. 

### Installing a minimal system
I cut my teeth on minimal Linux systems running tiling window managers like [DWM](https://dwm.suckless.org/) and [Ratpoison](https://www.nongnu.org/ratpoison/).
While I still appreciate the minimalism, I've decided to try and focus more on building things rather than configuring my system to be more efficient.
I am inspired by xkcd's [Is It Worth The Time?](https://xkcd.com/1205/).
Conveniently if you install [openSUSE tumbleweed](https://get.opensuse.org/tumbleweed/) you can install a minimal graphical system.
This actually installs [IceWM](https://ice-wm.org/) which is a fairly minimal floating window manager with a Windows-ish UX.
It's ugly but works well with next to no configuration.
I'm seeing how efficient my workflow is with this kind of setup.
As a plus, a non-tech person could probably navigate this setup without guidance.

#### Wifi
```Shell
sudo zypper install NetworkManager-applet
```

#### Audio
```Shell
sudo zypper install alsa-firmware pulseaudio pavucontrol pasystray
```

#### Manage display
```Shell
sudo zypper install arandr xbacklight
```

#### Mount and manage external drives
```Shell
sudo zypper install udisks2 udiskie
```

#### [OBS Package Installer](https://github.com/openSUSE/opi)
This enables installing things outside the main open-source package repository.
We need it specifically to install media codecs.

```Shell
sudo zypper install opi
opi codecs
```

#### Chezmoi
Of course we need to install and initialize [Chezmoi](https://www.chezmoi.io/):

```Shell
sudo zypper install chezmoi
chezmoi init xcapaldi
chezmoi apply
```

#### Use posix-compliant shell
Alias `sh` to `dash` instead of `bash`.
This should speed up posix-compliant scripts.

```Shell
sudo zypper install dash
sudo rm /bin/sh
sudo ln -s /bin/dash /bin/sh
```

#### Email
Fetching email locally can be the most complex configuration managed via Chezmoi.
I am trying Gnus inside Emacs as my email client (instead of `isync` + `notmuch` + `msmtp`).
Gnus relies on the =.authinfo= file to contain mail server information and credentials.
Gnus' own configuration includes some personal information as well.
While GPG is standard for encryption, I prefer to use [age](https://github.com/FiloSottile/age) because it is far easier use for this purpose.
I generate two keyfiles; one for Chezmoi's native encrypted configuration support and one for general use.
The Gnus configuration (`gnus.el`) is encrypted by Chezmoi using that key: `encrypted_gnus.el.age`.
This means inside the repo, it is encrypted.
However on my local machine, it lives in plain text.
This would work for the `.authinfo` as well but it is best practice to keep it encrypted even locally so the passwords are not stored in plain text.
So `.authinfo` is encrypted with the general key (`.authinfo.age`) even locally.
These keys are the only important pieces that are not maintained with the rest of my configuration.
