# zplug
source ~/.zplug/init.zsh

zplug "b4b4r07/zplug"
zplug "b4b4r07/enhancd"

zplug "zsh-users/zsh-syntax-highlighting"
zplug "zsh-users/zsh-history-substring-search"
zplug "zsh-users/zsh-completions"

zplug "yous/vanilli.sh"

zplug "Tarrasch/zsh-functional"

zplug "plugins/git", from:oh-my-zsh

zplug 'dracula/zsh', as:theme

export LIME_DIR_DISPLAY_COMPONENTS=2

if ! zplug check --verbose; then
    printf "Install? [y/N]: "
    if read -q; then
        echo; zplug install
    fi
fi

zplug load --verbose

#export LANGUAGE=ja_JP.UTF-8
#export LANG=ja_JP.UTF-8
#export LC_ALL=ja_JP.UTF-8

export PATH="$HOME/bin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin"

## history
HISTFILE=~/.zsh_history
HISTSIZE=1000000
SAVEHIST=1000000

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

#autoload predict-on
#predict-on


# shell
alias ll="ls -alG"
alias less="less -R"
alias history="history 1"

alias g='cd $(ghq root)/$(ghq list | peco)'
alias fig='docker-compose'

# emacs
## emacsclient
alias emacs="emacs -nw"
alias e="emacsclient -n"

## cask
export PATH="$HOME/.cask/bin:$PATH"

# chromium
alias chromium="chromium --force-device-scale-factor=1.2"

# ruby
## rbenv
export PATH="$HOME/.rbenv/bin:$PATH"
eval "$(rbenv init -)"

## bundler
alias bi="bundle install --path=vendor/bundle"
alias be="bundle exec"

# nodejs
export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

## yarn
export PATH=$PATH:`yarn global bin`

# golang
export GOPATH=$HOME
export GOENVGOROOT=$HOME/.goenvs
export GOENVTARGET=$HOME/bin
export GOENVHOME=$HOME/workspace

# stack
export PATH="$HOME/.local/bin:$PATH"

# tfenv
export PATH="$HOME/.tfenv/bin:$PATH"

# direnv
eval "$(direnv hook zsh)"

# The next line updates PATH for the Google Cloud SDK.
if [ -f "$HOME/google-cloud-sdk/path.zsh.inc" ]; then source "$HOME/google-cloud-sdk/path.zsh.inc"; fi

# The next line enables shell command completion for gcloud.
if [ -f "$HOME/google-cloud-sdk/completion.zsh.inc" ]; then source "$HOME/google-cloud-sdk/completion.zsh.inc"; fi


PATH=$PATH:$HOME/google-cloud-sdk/platform/google_appengine/

# kubectl
source <(kubectl completion zsh)
autoload -U colors; colors
# source $HOME/.zsh/zsh-kubectl-prompt/kubectl.zsh
# PROMPT=$PROMPT'%{$fg[magenta]%}($ZSH_KUBECTL_PROMPT)%{$reset_color%} '

# kubebuilder
PATH=$PATH:$HOME/local/kubebuilder/bin

source "/usr/local/opt/kube-ps1/share/kube-ps1.sh"
PS1='$(kube_ps1)'$PS1

export PYENV_ROOT="$HOME/.pyenv"
export PATH="$PYENV_ROOT/bin:$PATH"
eval "$(pyenv init -)"

source ~/.zprofile
export GOENV_ROOT="$HOME/.goenv"
export PATH="$GOENV_ROOT/bin:$PATH"
eval "$(goenv init -)"

export VISUAL="emacs -nw -q"
export EDITOR="emacs -nw -q"

source ~/.zsh/http_status

fpath+=~/.zfunc

autoload -U compinit
compinit

export RUST_SRC_PATH="$(rustc --print sysroot)/lib/rustlib/src/rust/src"
