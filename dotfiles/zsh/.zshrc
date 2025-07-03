# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi
if [ -f ${HOME}/.zplug/init.zsh ]; then
    source ${HOME}/.zplug/init.zsh
fi

export PATH=$PATH:~/.local/bin/

#scripts
export PATH=$PATH:~/.config/scripts
export PATH=$PATH:~/.config/scripts/Automations
export PATH=$PATH:~/.config/scripts/Development
export PATH=$PATH:~/.config/scripts/Misc
export PATH=$PATH:~/.config/scripts/Media
export PATH=$PATH:~/.config/scripts/Accounting
export PATH=$PATH:~/.config/scripts/Dotfiles

# Path to your oh-my-zsh installation.
export ZSH="/home/joshua/.oh-my-zsh"
export EDITOR="nvim"
export PATH="$HOME/.tmux/plugins/tmuxifier/bin:$PATH"

plugins=(
  git
  fzf-tab
  zsh-autosuggestions
  zsh-syntax-highlighting
  npm
  zsh-interactive-cd
  z
  yarn
  ufw
  systemadmin
  pass
  minikube
  kubectl
  jsontools
  docker
  copybuffer
  copypath
  command-not-found
  transfer
)

ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='fg=4'

source $ZSH/oh-my-zsh.sh

# Setup fzf
if [[ ! "$PATH" == */usr/share/fzf/bin* ]]; then
  export PATH="${PATH:+${PATH}:}/usr/share/fzf/bin"
fi

# Auto-completion
[[ $- == *i* ]] && source "/usr/share/fzf/shell/completion.zsh" 2> /dev/null

# Key bindings
source "/usr/share/fzf/shell/key-bindings.zsh"

bindkey '^R' fzf-history-widget

# Quick cd using fzf
fcd() {
  cd "$(find -type d | fzf --preview 'tree -C {} | head -200' --preview-window 'up:60%')"
}

# Find and edit using fzf
fe() {
  nvim "$(find -type f | fzf --preview 'cat {}' --preview-window 'up:60%')"
}

frg() {
	RG_PREFIX="rga --files-with-matches"
	local file
	file="$(
		FZF_DEFAULT_COMMAND="$RG_PREFIX '$1'" \
			fzf --sort --preview="[[ ! -z {} ]] && rga --pretty --context 5 {q} {}" \
				--phony -q "$1" \
				--bind "change:reload:$RG_PREFIX {q}" \
				--preview-window="70%:wrap"
	)" &&
	echo "opening $file" &&
	xdg-open "$file"
}

pe() {
  local file
  file=$(find ~/.password-store -type f -name '*.gpg' | sed "s|^$HOME/.password-store/||;s|\.gpg$||" | fzf)
  if [ -n "$file" ]; then
    pass edit "$file"
  fi
}

# Find and remove files with fzf
frm() {
  # Use `find` to list files and directories, and pipe them to `fzf` for selection
  selected=$(find . -type f -o -type d 2>/dev/null | fzf -m)
  
  # Check if any selection was made
  if [[ -n "$selected" ]]; then
    # Echo the files or directories that will be deleted
    echo "Deleting the following files or directories:"
    echo "$selected"
    
    # Use `xargs` to safely pass selected files/directories to `rm -rf`
    echo "$selected" | xargs -d '\n' rm -rf
  else
    echo "No files or directories selected."
  fi
}

ssh_fzf() {
    local host=$(grep "Host " ~/.ssh/config | cut -d " " -f 2 | fzf)
    if [[ -n $host ]]; then
        ssh "$host"
    else
        echo "No host selected"
    fi
}

# path for GO
export PATH="$PATH:$HOME/go/bin"

# Programs
alias cat="bat"
alias cl="clear"
alias ls="eza -l --icons"
alias la="eza -TL 2 --icons"
alias lg="lazygit"
alias ld="lazydocker"
alias py="python"
alias td="termdown"

# Aliases 2024
alias dnf="sudo dnf"
alias p="sudo pacman"
alias ytd='yt-dlp'
alias src="source ~/.zshrc"
alias sshl="ssh_fzf"

# Navigation
alias godir="cd ~/go/src/github.com/jblais493/"
alias photos="cd ~/Photos"
alias f="thunar . &"
alias revere="cd ~/Revere"
alias revereb="cd ~/Revere/Revere\ LATEST/Brokerage"
alias commer="cd ~/Revere/Revere\ LATEST/Brokerage/Alberta/Edmonton/Deals/2025/Commercial"
alias sellers="cd ~/Revere/Revere\ LATEST/Brokerage/Alberta/Edmonton/Deals/2025/Sellers"
alias buyers="cd ~/Revere/Revere\ LATEST/Brokerage/Alberta/Edmonton/Deals/2025/Buyers"
alias revsys="cd ~/Revere/Revere\ LATEST/Systems"
alias nas="cd /mnt/nomad/TrueNAS"
alias media="cd /mnt/nomad/TrueNAS/media"
alias music="cd ~/MusicOrganized"
alias movies="cd /mnt/nomad/TrueNAS/media/Movies"
alias library="cd /mnt/nomad/TrueNAS/Library"
alias mpray="nvim ~/Vaults/Personal/Prayers/Morning\ Prayers.md"
alias epray="nvim ~/Vaults/Personal/Prayers/Evening\ Prayers.md"

# Scripts
alias w2pdf="wkhtmltopdf"
alias devwork="~/.config/scripts/devwork.sh"
alias f2p="~/.config/scripts/file_2_phone.sh"
alias dlp="nvim ~/.config/scripts/dlphone"
alias eopn="~/.config/scripts/manage_encrypted_drives eopn"
alias ecls="~/.config/scripts/manage_encrypted_drives ecls"
alias rs="~/.config/scripts/gammastep.sh"
alias gst="python ~/.config/scripts/Accounting/GST.py"
alias remit="python ~/.config/scripts/Accounting/GST_Remit.py"
alias rsx="killall gammastep"
alias pg="pass generate"
alias v="nvim"
alias sd="spotdl"
alias nm="neomutt"
alias syncvault="rsync -avz --delete /mnt/TrueNAS/ /mnt/Vault/TrueNAS"
alias syncnas="rsync -avz --delete /mnt/nomad/TrueNAS/ /mnt/External4TB/TrueNAS"
alias syncrev="rsync -avz --delete '/home/joshua/Revere/Revere LATEST/' '/mnt/nomad/TrueNAS/Revere/Revere LATEST/'"
alias mntnas="sshfs joshua@172.18.250.13:/mnt/Vault /mnt/Logos"
alias umountnas="fusermount -u /mnt/Logos"
alias mntexternal="sudo mount /dev/sdb1 /mnt/External4TB"
alias mntvault="sudo mount /dev/sda /mnt/Logos"
alias reverecalc="cd ~/Revere/Systems/Programs/Calculators && python ConveyancingOutput.py"
alias buyercalc="cd ~/Revere/Systems/Programs/Calculators && python BuyerCommissionCalc.py"
alias record="arecord -f cd output.wav"
alias anime="~/.config/scripts/ani-cli/ani-cli"
alias brodirs="mkdir 'Brokerage Documents' 'Offer' 'Conveyancing' 'Payout' 'Posts'"
alias twit="gimp /mnt/Logos/TrueNAS/Personal/Twitter.xcf"
alias strip="mogrify -strip"

# Emacs
alias emacs="GDK_BACKEND=x11 emacs"
alias doomsync="~/.config/emacs/bin/doom sync"
alias doomdoc="~/.config/emacs/bin/doom doctor"
alias doompurge="~/.config/emacs/bin/doom purge"
alias doomupgrade="~/.config/emacs/bin/doom upgrade"

# NixOS
alias nrs="~/dotfiles/scripts/nix-config.sh"
alias enix="nvim ~/dotfiles/nixos/configuration.nix"

# Networking
alias nmconnect="nmcli device wifi connect"
alias nmdown="nmcli c delete"
alias nmlist="nmcli device wifi list"
alias nmdelete="nmcli device delete"
alias startvpn="sudo systemctl start wg-quick@wg0"
alias stopvpn="sudo systemctl stop wg-quick@wg0"

# Payouts
alias letterhead="cd /mnt/nomad/TrueNAS/Revere/Revere\ LATEST/Logos\ and\ Assets/Letterhead/2022/"
alias eftinfo="gimp ~Revere/Revere\ LATEST/Logos\ and\ Assets/Letterhead/2022/Paid\ by\ EFT.xcf"
alias invoice="gimp ~/Revere/Logos\ and\ Assets/Letterhead/2022/Invoice\ Template.xcf"
alias cinst="gimp ~/Revere/Systems/Conveyancing/Templates/Conveyancing\ Instructions\ Template.xcf"
alias cominv="gimp ~/Revere/Logos\ and\ Assets/Letterhead/2022/Commission\ Invoice\ Template.xcf"
alias cashback="gimp ~/Revere/Logos\ and\ Assets/Letterhead/2022/Cashback\ Template.xcf"
alias payout="gimp ~/Revere/Systems/Templates/Invoicing\ Templates/Paystub\ Template.xcf"
alias payoutCody="gimp ~/Revere/Brokerage/Alberta/Edmonton/Agents/Paystubs/Paystub\ -\ Cody\ Serediak.xcf"
alias payoutSeth="gimp ~/Revere/Brokerage/Alberta/Edmonton/Agents/Paystubs/Paystub\ -\ Seth\ Macdonald.xcf"

# Tmux commands
alias kat="tmux kill-server"
alias t="TERM=screen-256color-bce tmux"
alias tat="tmux attach -t"
alias tsf="tmux source-file ~/.tmux.conf"
alias tk="tmux kill-session -a"

# config files
alias scripts="cd ~/.config/scripts"
alias kmon="kmonad ~/.config/kmonad/config.kbd &"

# Development
alias work="arttime --nolearn -a eye -t 'For I consider that the sufferings of this present time are not worth comparing with the glory that is going to be revealed to us - Romans 8:18' -g 4h"
alias search-email='~/.config/scripts/email_search'
alias blogdeploy="~/.config/scripts/Development/blogdeploy.sh"

# Docker commands migrated to podman
alias docker compose="podman compose"
alias dcd="podman compose down"
alias dcu="podman compose up -d"
alias dcb='podman build -t forge.labrynth.org/josh/$(basename $PWD):latest .'
alias dc="podman compose"
alias dps="podman ps -a"
alias dl="podman logs"

# Generate API Key
alias apigen="go run ~/.config/scripts/Development/apikeygen.go -app"

# Tasks for running htmx app
alias tr="go-task run"
alias tm="go-task migrate"
alias tb="go-task templ"
alias ttw="go-task tailwindcss"

## Go documentation
alias gdoc="stdsym | fzf --preview 'go doc {}' | xargs go doc"

# Management
alias ff="fastfetch"
alias ytmp3="~/.config/scripts/Media/ytmp3.sh"
alias aa="python ~/.config/scripts/Media/albumartwork.py"
alias mpdupdate="~/.config/scripts/Media/mpdupdate.sh"

bindkey -v
bindkey -M viins 'kj' vi-cmd-mode
# this will cd and ls at the same time.
function cd {
    builtin cd "$@" && ls -F
    }

eval $(thefuck --alias)
SAVEHIST=1000  # Save most-recent 1000 lines
HISTFILE=~/.zsh_history

# Edit line in vim with ctrl-e:
autoload edit-command-line; zle -N edit-command-line
bindkey '^e' edit-command-line


function r() {
	local tmp="$(mktemp -t "yazi-cwd.XXXXXX")"
	yazi "$@" --cwd-file="$tmp"
	if cwd="$(cat -- "$tmp")" && [ -n "$cwd" ] && [ "$cwd" != "$PWD" ]; then
		cd -- "$cwd"
	fi
	rm -f -- "$tmp"
}

eval "$(starship init zsh)"
eval "$(tmuxifier init -)"
eval "$(zoxide init zsh)"

# pnpm
export PNPM_HOME="/home/joshua/.local/share/pnpm"
case ":$PATH:" in
  *":$PNPM_HOME:"*) ;;
  *) export PATH="$PNPM_HOME:$PATH" ;;
esac
# pnpm end

# Hugo Version Manager: override path to the hugo executable.
hugo() {
  hvm_show_status=true
  if hugo_bin=$(hvm status --printExecPathCached); then
    if [ "${hvm_show_status}" = "true" ]; then
      >&2 printf "Hugo version management is enabled in this directory.\\n"
      >&2 printf "Run 'hvm status' for details, or 'hvm disable' to disable.\\n\\n"
    fi
  else
    if hugo_bin=$(hvm status --printExecPath); then
      if ! hvm use --useVersionInDotFile; then
        return 1
      fi
    else
      if ! hugo_bin=$(whence -p hugo); then
        >&2 printf "Command not found.\\n"
        return 1
      fi
    fi
  fi
  "${hugo_bin}" "$@"
}

# Add in direnv hook
eval "$(direnv hook zsh)"

# Use keychain to manage ssh-agent
eval $(keychain --eval --agents ssh empire.key 2>/dev/null)
# Add key manually since keychain has trouble with it
if ! ssh-add -l 2>/dev/null | grep -q "empire beginning ssh key"; then
  ssh-add ~/.ssh/empire.key
fi
