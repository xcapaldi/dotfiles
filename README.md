# dotfiles

My dotfiles leverage [chezmoi](https://www.chezmoi.io/) to manage:

* MacOS work machine
* WSL2 Ubuntu system under Windows 11
* Superficially Fedora homelab server

I previously use Linux as my primary machine and under those circumstances I rolled with a minimal [Fedora](https://fedoraproject.org/) setup.
I now use [Homebrew](https://brew.sh/) as the package manager on MacOS and WSL.
This is purely for the convenience of having identical Emacs versions since I'm often using features from the most recent release.
In all cases I've slowly steered away from super complex configurations in favor of using default functionality.
Low-config, composable tools have become increasingly valuable as life pressures restrict time to fiddle.

## MacOS work machine
Not much to say here... chezmoi takes care of installing [Homebrew](https://brew.sh/) packages and global [mise](https://mise.jdx.dev/) tool versions.
It also manages my `zshrc`, `psqlrc` git and ssh configs.
There is some additional corporate configuration that must be done manually on a fresh install.

## Windows Host and WSL2
I don't manage the host Windows 11 system with chezmoi because configuration is minimal and I only install a few packages using Winget.

> [!NOTE]
> You may need to update the Windows Store before Winget works properly.


``` powershell
winget install Bitwarden.Bitwarden
winget install Anthropic.Claude
winget install Google.Chrome
winget install Discord.Discord
winget install Valve.Steam
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

## Homelab

### OS and tools
Install [Fedora Server (44)](https://fedoraproject.org/server/) with no `root` user AND with encrypted harddrive.
This will automatically run [Cockpit](https://cockpit-project.org) which is very nice for remote management.

Can access Cockpit UI at `https://ip-address-of-machine:9090`.

Install [Docker](https://docs.docker.com/engine/install/fedora/#set-up-the-repository)
- Set up the repository: `sudo dnf config-manager addrepo --from-repofile https://download.docker.com/linux/fedora/docker-ce.repo`
- Install latest version: `sudo dnf install docker-ce docker-ce-cli containerd.io docker-buildx-plugin docker-compose-plugin`
- Start docker engine automatically: `sudo systemctl enable --now docker`

### Immich
Install [Immich](https://docs.immich.app/overview/quick-start/).
Follow the [post-install guide](https://docs.immich.app/install/post-install):

- Navigate to `http://<machine-ip-address>:2283` and create an admin user.

### Backups

I want to backup Immich's database and the raw images to my desktop.
In the future I may want a 2-way synchronization for my recipe store.
I tried Syncthing but found it far to complex.
Since this is a local network I'll just rely on shared drives

#### Samba

```sh
sudo dnf install samba
sudo systemctl enable --now smb
```

#### Define the share

`sudo vi /etc/samba/smb.conf` and append at the bottom:

```
[immich]
    path = /home/admin/immich-app/library
    browseable = yes
    read only = yes
    valid users = admin
```

#### Allow SELinux to serve it

```sh
sudo setsebool -P samba_export_all_ro on
```

#### Set a Samba password

```sh
sudo smbpasswd -a admin
```

#### Open the firewall

```sh
sudo firewall-cmd --permanent --add-service=samba
sudo firewall-cmd --reload
```

#### Test and restart

```sh
sudo testparm
sudo systemctl restart smb
```

#### Map it on Windows

On the desktop go to Explorer -> This PC -> Map network drive -> `\\server-ip\immich` -> tick "Connect using different credentials" and "Reconnect at sign-in".
Enter the Samba username and password from step 5.
Confirm you can see `library`, `upload` and `backups`.

#### Create the copy script

Save the following as `immich-backup.bat`:

```
robocopy \\server-ip\immich\library C:\Users\xavie\immich-backup\library /E /XO /R:2 /W:5 /LOG+:C:\Users\xavie\immich-backup\log.txt
robocopy \\server-ip\immich\upload C:\Users\xavie\immich-backup\upload /E /XO /R:2 /W:5 /LOG+:C:\Users\xavie\immich-backup\log.txt
robocopy \\server-ip\immich\backups C:\Users\xavie\immich-backup\backups /E /XO /R:2 /W:5 /LOG+:C:\Users\xavie\immich-backup\log.txt
```

#### Run it once by hand for the bulk backup

#### Schedule it

In Task Scheduler -> Create Basic Task -> Daily -> Start a program -> point to `immich-backup.bat`.
Leave it on "Run only when user is logged in".
Once the task is created, edit it and under Properties -> Settings, enable "Run task as soon as possible after a scheduled start is missed".

