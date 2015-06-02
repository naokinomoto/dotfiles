export ZSH=$HOME/.oh-my-zsh

ZSH_THEME="robbyrussell"

plugins=(git aws brew brew-cask atom bower bundler capistrano cp cpanm docker emoji-clock gem git-extras git-flow git-hubflow git-remote-branch github gitignore go grails gradle heroku history knife knife-ssh npm nvm nyan osx perl pip pod rails rake ruby rbenv python rsync sbt scala ssh-agent sudo vagrant zsh_reload)

source $ZSH/oh-my-zsh.sh

export LANGUAGE=ja_JP.utf8
export LANG=ja_JP.utf8
export LC_ALL=ja_JP.utf8

export PATH="/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/bin:$HOME/bin"

## history
HISTFILE=~/.zsh_history
HISTSIZE=100000
SAVEHIST=100000

setopt append_history
setopt share_history
setopt extended_history
setopt hist_ignore_dups
setopt hist_ignore_space
setopt hist_reduce_blanks
setopt hist_save_no_dups
setopt hist_no_store

setopt correct
setopt auto_cd
setopt pushd_ignore_dups
setopt magic_equal_subst
setopt complete_in_word
setopt complete_aliases
setopt no_flow_control
setopt no_clobber
setopt nonomatch

setopt prompt_subst
setopt no_prompt_cr
setopt transient_rprompt

# alias
alias ll="ls -alG"
alias diff="colordiff"
alias less="less -R"
alias e="/Applications/Emacs.app/Contents/MacOS/bin/emacsclient"
alias gcc="gcc-4.8"

# cask
export PATH="$HOME/.cask/bin:$PATH"

# rbenv
#export PATH="$HOME/.rbenv/bin:$PATH"
eval "$(rbenv init -)"

# node.js
[ -s "/usr/local/nvm/nvm.sh" ] && . "/usr/local/nvm/nvm.sh"

# plenv
#export PATH="$HOME/.plenv/bin:$PATH"
#eval "$(plenv init -)"

# python
#export WORKON_HOME=$HOME/.virtualenvs
#. /usr/local/bin/virtualenvwrapper.sh

# cdr
autoload -Uz chpwd_recent_dirs cdr add-zsh-hook
add-zsh-hook chpwd chpwd_recent_dirs
zstyle ':chpwd:*' recent-dirs-max 5000
zstyle ':chpwd:*' recent-dirs-default yes
zstyle ':completion:*' recent-dirs-insert both

# zaw
source $HOME/.zaw/zaw.zsh
zstyle ':filter-select' case-insensitive yes
bindkey '^xf' zaw-cdr
bindkey '^x^b' zaw-git-recent-branches
bindkey '^x^f' zaw-git-files
bindkey '^x^r' zaw-history

source ~/.zsh-git-prompt/zshrc.sh
PROMPT='%B%m%~%b$(git_super_status) %# '

export GOPATH=$HOME
