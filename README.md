# dotfiles

My dotfiles leverage [chezmoi](https://www.chezmoi.io/) to manage:

* MacOS work machine
* WSL2 Ubuntu system under Windows 11
* Ubuntu hacktop when I can't be at the desktop

I used to use Linux as my primary machine and under those circumstances I rolled with a minimal [Fedora](https://fedoraproject.org/) setup.
However since building a Windows PC, WSL only officially supports Ubuntu and OpenSUSE.
It's easiest to keep the configuration consistent between machines so now I have Ubuntu both in WSL and on my hacktop.
I even use [Homebrew](https://brew.sh/) as the package manager on all the unix systems.
In all cases I've slowly steered away from super complex configurations in favor of using default functionality.
Low-config, composable tools have become increasingly valuable as life pressures restrict time to fiddle.

## A Shift in Philosophy: tmux + terminal apps over Emacs

Emacs is incredible. It doesn't conform to the status quo and that allows you to develop some independent thinking in terms of **how** to interact with the machine.
However AI coding tools (Claude Code) are built around terminal-first ([agent-teams](https://code.claude.com/docs/en/agent-teams)).
The tmux + terminal apps approach has a structural advantage here: spin up a pane for an AI agent, another for your editor, another for logs, and they coexist naturally.
I don't really want to depend on Emacs + vterm (already rougher than a native terminal) + tmux in some cases.
For now, I've migrated to a terminal workflow.
If the AI ecosystem stabilizes, I'm sure Emacs will adapt and be a very powerful option; I simply don't have the time to hack it until then.

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

## Ubuntu
Since my main desktop is running Ubuntu under WSL, Windows is taking care of window management.
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
Since I'm exclusively using this machine to work on personal projects I usually just have a browser and a terminal (with tmux) running.
