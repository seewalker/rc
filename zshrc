#--------NOTE--------"
# DO ALL CUSTOMIZATION AT BOTTOM OF FILE. OTHERWISE, OH-MY-ZSH MAY
# OVERWRITE IT.


# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh
ec=$HOME/Documents/EarlhamCollege

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
ZSH_THEME="philips"


# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"

# Set to this to use case-sensitive completion
# CASE_SENSITIVE="true"

# Uncomment this to disable bi-weekly auto-update checks
# DISABLE_AUTO_UPDATE="true"

# Uncomment to change how often before auto-updates occur? (in days)
# export UPDATE_ZSH_DAYS=13

# Uncomment following line if you want to disable colors in ls
# DISABLE_LS_COLORS="true"

# Uncomment following line if you want to disable autosetting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment following line if you want to disable command autocorrection
# DISABLE_CORRECTION="true"

# Uncomment following line if you want red dots to be displayed while waiting for completion
# COMPLETION_WAITING_DOTS="true"

# Uncomment following line if you want to disable marking untracked files under
# VCS as dirty. This makes repository status check for large repositories much,
# much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment following line if you want to  shown in the command execution time stamp 
# in the history command output. The optional three formats: "mm/dd/yyyy"|"dd.mm.yyyy"|
# yyyy-mm-dd
# HIST_STAMPS="mm/dd/yyyy"

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(git osx web-search vi-mode battery)

source $ZSH/oh-my-zsh.sh

# User configuration

export PATH=/sbin:/usr/sbin:/usr/local/bin:${PATH}
# export MANPATH="/usr/local/man:$MANPATH"

# # Preferred editor for local and remote sessions
# if [[ -n $SSH_CONNECTION ]]; then
#   export EDITOR='vim'
# else
#   export EDITOR='mvim'
# fi

# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# ssh
# export SSH_KEY_PATH="~/.ssh/dsa_id"

function flipPager {
    if [[ $PAGER -eq $VIMPAGER ]]; then 
        PAGER=less
    else 
        PAGER=VIMPAGER
    fi
}

PS2="|%_> "
export EDITOR="vim"
export PAGER=less
#export VIMPAGER="/bin/sh -c "unset PAGER; col -b -x |    vim -R -c 'set ft=man nomod nolist' -c 'map q : q<CR>'    -c 'map <SPACE> <C-D>' -c 'map b <C-U>'    -c 'nmap K :Man <C-R>=expand(\"<cword>\")<CR><CR>' -""
export LANG="en_Us"
export KEYTIMEOUT=0.4
unsetopt autopushd #overriding oh-my-zsh
setopt HIST_IGNORE_SPACE
setopt HIST_IGNORE_DUPS
setopt HIST_VERIFY
setopt EXTENDED_HISTORY #adds timestamp to $HISTFILE entries.
setopt INC_APPEND_HISTORY #gradually write to $HISTFILE rather than all at once
                          #when shell exits.
setopt EXTENDED_GLOB
setopt PUSHD_IGNORE_DUPS
setopt PUSHD_MINUS
setopt PUSHD_TO_HOME
setopt AUTO_CD #this shadows oh-my-zsh but I make it explicit.
setopt LIST_PACKED #makes autocomplete lists have multiple columns
setopt RC_QUOTES #allows single quotes to be nested in strings
setopt MUltios #implicit 'tee' for multiple `/ ><words>/' expressions.
setopt VI

bindkey "^[[A" history-beginning-search-backward  #up arrow
bindkey "^[[B" history-beginning-search-forward   #down arrow
# Search based on what you typed in already
