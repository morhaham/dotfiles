# If you come from bash you might have to change your $PATH.
# export PATH=$HOME/bin:/usr/local/bin:$PATH
export ZSH="$HOME/.oh-my-zsh"
# ZSH_THEME="my-theme"
ZSH_THEME="robbyrussell"

# plugins
plugins=(git autojump)
# plugins+=(zsh-vi-mode)

# Plugins config
ZVM_CURSOR_STYLE_ENABLED=false
# plugins end

source $ZSH/oh-my-zsh.sh

# aliases
# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"
alias vim="nvim"
# aliases end

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

test -e "${HOME}/.iterm2_shell_integration.zsh" && source "${HOME}/.iterm2_shell_integration.zsh"

# pnpm
export PNPM_HOME="/Users/morh/Library/pnpm"
export PATH="$PNPM_HOME:$PATH"
# pnpm end

export PATH="$PATH:/Users/morh/.local/bin"
export PATH="/opt/homebrew/opt/openjdk/bin:$PATH"
export JAVA_HOME=/Library/Java/JavaVirtualMachines/zulu-11.jdk/Contents/Home
[ -f /opt/homebrew/etc/profile.d/autojump.sh ] && . /opt/homebrew/etc/profile.d/autojump.sh
export LANG="en_US.UTF-8"
export LC_CTYPE="en_US.UTF-8"

autoload -Uz add-zsh-hook
add-zsh-hook -Uz chpwd osc7_cwd
##############################################

PATH=~/.console-ninja/.bin:$PATH

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

# some apps config
export GREENLIGHT_DB_DSN='postgres://greenlight:qwerty123@localhost/greenlight?sslmode=disable'
export PATH=/usr/local/anaconda3/bin:$PATH
export PATH=/opt/homebrew/anaconda3/bin:$PATH

# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
__conda_setup="$('/opt/homebrew/anaconda3/bin/conda' 'shell.zsh' 'hook' 2> /dev/null)"
if [ $? -eq 0 ]; then
    eval "$__conda_setup"
else
    if [ -f "/opt/homebrew/anaconda3/etc/profile.d/conda.sh" ]; then
        . "/opt/homebrew/anaconda3/etc/profile.d/conda.sh"
    else
        export PATH="/opt/homebrew/anaconda3/bin:$PATH"
    fi
fi
unset __conda_setup
# <<< conda initialize <<<

