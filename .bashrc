#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

source ~/.local/share/icons-in-terminal/icons_bash.sh

PS1='[\u@\h \W]\$ '

shopt -s autocd

export DICTIONARY=en_US
export DICPATH=~/.emacs.d/hunspell
export EDITOR='emacsclient -t -a emacs -nw'
export VISUAL='emacsclient -c -a emacs'

alias update='sudo pacman -Syu'
alias install='sudo pacman -S'
alias scr='sudo systemctl restart'
alias sce='sudo systemctl enable'
alias sct='sudo systemctl stop'
alias scs='sudo systemctl start'
alias scur='systemctl --user restart'
alias scut='systemctl --user stop'
alias scus='systemctl --user start'
alias off='shutdown now'
alias rbt='reboot now'
alias www='firefox-developer-edition'
alias ffx='firefox-developer-edition'
alias exiftool='/usr/bin/vendor_perl/exiftool'
alias chbg='feh --randomize --bg-fill /home/arthur/wallpapers/*'
alias xrld='pkill loginscript'
alias decor='emerald --replace&'
alias undecor='pkill emerald'
alias spm='sudo pacman'
alias psa='ps -aux | grep -v "grep" | grep'
alias config='/usr/bin/git --git-dir=$HOME/.config/dotfiles/ --work-tree=$HOME'

#export LC_ALL=C

function reload {
    source ~/.bashrc
}

# If not running interactively, don't do anything
case $- in
    *i*) ;;
    *) return;;
esac

# Path to the bash it configuration
export BASH_IT="/home/arthur/repos/bash-it"

# Lock and Load a custom theme file.
# Leave empty to disable theming.
# location /.bash_it/themes/
export BASH_IT_THEME='bobby'

# (Advanced): Change this to the name of your remote repo if you
# cloned bash-it with a remote other than origin such as `bash-it`.
# export BASH_IT_REMOTE='bash-it'

# Your place for hosting Git repos. I use this for private repos.
export GIT_HOSTING='git@git.domain.com'

# Don't check mail when opening terminal.
unset MAILCHECK

# Change this to your console based IRC client of choice.
export IRC_CLIENT='irssi'

# Set this to the command you use for todo.txt-cli
export TODO="t"

# Set this to false to turn off version control status checking within the prompt for all themes
export SCM_CHECK=true

# Set Xterm/screen/Tmux title with only a short hostname.
# Uncomment this (or set SHORT_HOSTNAME to something else),
# Will otherwise fall back on $HOSTNAME.
#export SHORT_HOSTNAME=$(hostname -s)

# Set Xterm/screen/Tmux title with only a short username.
# Uncomment this (or set SHORT_USER to something else),
# Will otherwise fall back on $USER.
#export SHORT_USER=${USER:0:8}

# Set Xterm/screen/Tmux title with shortened command and directory.
# Uncomment this to set.
#export SHORT_TERM_LINE=true

# Set vcprompt executable path for scm advance info in prompt (demula theme)
# https://github.com/djl/vcprompt
#export VCPROMPT_EXECUTABLE=~/.vcprompt/bin/vcprompt

# (Advanced): Uncomment this to make Bash-it reload itself automatically
# after enabling or disabling aliases, plugins, and completions.
# export BASH_IT_AUTOMATIC_RELOAD_AFTER_CONFIG_CHANGE=1

# Uncomment this to make Bash-it create alias reload.
# export BASH_IT_RELOAD_LEGACY=1

# Load Bash It
source "$BASH_IT"/bash_it.sh
