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

zplug "yous/lime"

#zplug 'dracula/zsh', as:theme

export LIME_DIR_DISPLAY_COMPONENTS=2

#ZSH_THEME="lime"

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

export PATH="$HOME/bin:$HOME/usr/local/bin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin"

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
alias ll="ls -al --color=auto"
alias less="less -R"
alias history="history 1"

alias g='cd $(ghq root)/$(ghq list | peco)'

# emacs
## emacsclient
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
# export NVM_DIR="$HOME/.nvm"
# [ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
# [ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

## yarn
# export PATH=$PATH:`yarn global bin`

# golang
export GOPATH=$HOME
export GOENV_ROOT="$HOME/.goenv"
export PATH="$GOENV_ROOT/bin:$PATH"
eval "$(goenv init -)"

export GOPATH=$HOME
export PATH="$HOME:$PATH"
export PATH="$GOROOT/bin:$PATH"
export PATH="$GOPATH/bin:$PATH"

# stack
export PATH="$HOME/.local/bin:$PATH"

# The next line updates PATH for the Google Cloud SDK.
if [ -f '$HOME/google-cloud-sdk/path.zsh.inc' ]; then source '$HOME/google-cloud-sdk/path.zsh.inc'; fi

# The next line enables shell command completion for gcloud.
if [ -f '$HOME/google-cloud-sdk/completion.zsh.inc' ]; then source '$HOME/google-cloud-sdk/completion.zsh.inc'; fi

# kubectl
# source <(kubectl completion zsh)

export PYENV_ROOT="$HOME/.pyenv"
export PATH="$PYENV_ROOT/bin:$PATH"
eval "$(pyenv init -)"

export VISUAL="emacs -nw -q"
export EDITOR="emacs -nw -q"

fpath+=~/.zfunc

autoload -U compinit
compinit

export PATH="$HOME/.cargo/bin:$PATH"
export RUST_SRC_PATH="$(rustc --print sysroot)/lib/rustlib/src/rust/src"
[[ -s "$HOME/.avn/bin/avn.sh" ]] && source "$HOME/.avn/bin/avn.sh" # load avn

. $HOME/.zsh.d/moderc

export N_PREFIX="$HOME/n"; [[ :$PATH: == *":$N_PREFIX/bin:"* ]] || PATH+=":$N_PREFIX/bin"  # Added by n-install (see http://git.io/n-install-repo).
export PATH="$HOME/.tfenv/bin:$PATH"
# source /home/nomoto/.pyenv/shims/aws_zsh_completer.sh
