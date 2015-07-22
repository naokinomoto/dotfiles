export ZSH=$HOME/.oh-my-zsh

ZSH_THEME="powerline"

plugins=(aws brew brew-cask atom bower bundler capistrano cp cpanm docker emoji-clock gem go grails gradle heroku history knife knife-ssh npm nvm nyan osx perl pip pod rails rake ruby rbenv python rsync sbt scala ssh-agent sudo vagrant zsh_reload)

source $ZSH/oh-my-zsh.sh

export LANGUAGE=ja_JP.UTF-8
export LANG=ja_JP.UTF-8
export LC_ALL=ja_JP.UTF-8

export PATH="/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:$HOME/bin"

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
alias e="/usr/local/bin/emacsclient"
alias emacs="/usr/local/bin/emacs"
alias gcc="gcc-4.8"
alias bi="bundle install --path=vendor/bundle"
alias be="bundle exec"

export EMACS="/usr/local/bin/emacs"
export EDITOR="/usr/local/bin/emacsclient"

# cask
export PATH="$HOME/.cask/bin:$PATH"

# rbenv
#export PATH="$HOME/.rbenv/bin:$PATH"
eval "$(rbenv init -)"

# node.js
[ -s "~/.nvm/nvm.sh" ] && . "~/.nvm/nvm.sh"

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
#source $HOME/.zaw/zaw.zsh
#zstyle ':filter-select' case-insensitive yes
#bindkey '^xf' zaw-cdr
#bindkey '^x^b' zaw-git-recent-branches
#bindkey '^x^f' zaw-git-files
#bindkey '^x^r' zaw-history

#source ~/.zsh-git-prompt/zshrc.sh
#PROMPT='%B%m%~%b$(git_super_status) %# '

export GOPATH=$HOME

# powerline
POWERLINE_HIDE_HOST_NAME="true"
POWERLINE_HIDE_GIT_PROMPT_STATUS="true"
POWERLINE_SHOW_GIT_ON_RIGHT="true"

export PATH=$PATH:~/Library/Python/2.7/bin
powerline-daemon -q
. ~/Library/Python/2.7/lib/python/site-packages/powerline/bindings/zsh/powerline.zsh


function peco-src() {
  local selected_dir=$(ghq list --full-path | peco --query "$LBUFFER")
  if [ -n "$selected_dir" ];then
    BUFFER="cd ${selected_dir}"
    zle accept-line
  fi
  zle clear-screen
}

zle -N peco-src
bindkey '^]' peco-src

export DOCKER_TLS_VERIFY=1
export DOCKER_HOST=tcp://192.168.59.103:2376
export DOCKER_CERT_PATH=~/.boot2docker/certs/boot2docker-vm
nvm use iojs
