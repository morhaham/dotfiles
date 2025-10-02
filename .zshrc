export ZSH="$HOME/.oh-my-zsh"
# ZSH_THEME="my-theme"
ZSH_THEME="robbyrussell"

# plugins
# plugins end

# Plugins config
# zsh-vi-mode
ZVM_VI_INSERT_ESCAPE_BINDKEY=jj
source $(brew --prefix)/opt/zsh-vi-mode/share/zsh-vi-mode/zsh-vi-mode.plugin.zsh
# plugins config end

source $ZSH/oh-my-zsh.sh

# aliases
# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"
alias vim="nvim"
alias python="python3"
alias pip="pip3"
alias k="kubectl"
# aliases end

# enable vim modal editing 
bindkey -v

# hombrew
HOMEBREW_NO_AUTO_UPDATE=1
# hombrew end

# NVM
export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
# # lazy load NVM
# lazy_load_nvm() {
#   unset -f node npm npx
#   export NVM_DIR="$HOME/.nvm"
#   [ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"
# }
# node() { lazy_load_nvm; node "$@"; }
# npm() { lazy_load_nvm; npm "$@"; }
# npx() { lazy_load_nvm; npx "$@"; }
# [ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion
# NVM end

test -e "${HOME}/.iterm2_shell_integration.zsh" && source "${HOME}/.iterm2_shell_integration.zsh"

export PATH="/Users/morh/.local/bin:$PATH"

# pnpm
export PNPM_HOME="/Users/morh/Library/pnpm"
export PATH="$PNPM_HOME:$PATH"
# pnpm end

export PATH="$HOME/.local/bin:$PATH"

# java related
# export JAVA_HOME=/Library/Java/JavaVirtualMachines/zulu-11.jdk/Contents/Home
# jenv java env manager
export PATH="$HOME/.jenv/bin:$PATH"
# eval "$(jenv init -)"
# jenv enable-plugin export
# java related end


export GOPATH=$HOME/go
export PATH=$PATH:$GOPATH/bin
export XDG_CONFIG_HOME=$HOME/.config

# vterm
vterm_printf() {
    if [ -n "$TMUX" ] && ([ "${TERM%%-*}" = "tmux" ] || [ "${TERM%%-*}" = "screen" ]); then
        # Tell tmux to pass the escape sequences through
        printf "\ePtmux;\e\e]%s\007\e\\" "$1"
    elif [ "${TERM%%-*}" = "screen" ]; then
        # GNU screen (screen, screen-256color, screen-256color-bce)
        printf "\eP\e]%s\007\e\\" "$1"
    else
        printf "\e]%s\e\\" "$1"
    fi
}
# vterm end

# Local apps config
# Greenlight
export GREENLIGHT_DB_DSN='postgres://postgres:morh123@localhost:5432/greenlight?sslmode=disable'
export MAILTRAP_USERNAME='06fa34b5a5be95'
export MAILTRAP_PASSWORD='e49042661e65df'
# Greenlight end
# Local apps config end

# Conda
export PATH=/usr/local/anaconda3/bin:$PATH
export PATH=/opt/homebrew/anaconda3/bin:$PATH

# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
# __conda_setup="$('/Users/morhaham/miniconda3/bin/conda' 'shell.zsh' 'hook' 2> /dev/null)"
# if [ $? -eq 0 ]; then
#     eval "$__conda_setup"
# else
#     if [ -f "/Users/morhaham/miniconda3/etc/profile.d/conda.sh" ]; then
#         . "/Users/morhaham/miniconda3/etc/profile.d/conda.sh"
#     else
#         export PATH="/Users/morhaham/miniconda3/bin:$PATH"
#     fi
# fi
# unset __conda_setup
# <<< conda initialize <<<
# Conda end

# Gemini
export GEMINI_API_KEY=AIzaSyDxKnkJVNNBeYC1F6lTjROPpayl6Pn0vHg
# end Gemini

# Aider
export OLLAMA_API_BASE=http://127.0.0.1:11434
# Aider end

# Cargo
export CARGO_HOME="$XDG_CONFIG_HOME/cargo"
# Cargo end

# Source local config file with sensitive information
if [ -f "$HOME/.zshrc.local" ]; then
  source "$HOME/.zshrc.local"
fi

# zoxide
eval "$(zoxide init zsh)"
# zoxide end
