PS1='%F{green}%1~%f %(?..%F{red})%)%f '

HISTSIZE=10000
SAVEHIST="$HISTSIZE"
HISTFILE="$HOME"/.zhistory

alias ls='\ls -FhvX --color=auto --group-directories-first'
alias ll='ls -g'
alias la='ll -A'

setopt HIST_IGNORE_ALL_DUPS HIST_IGNORE_SPACE HIST_NO_STORE
setopt HIST_LEX_WORDS HIST_REDUCE_BLANKS

autoload -U compinit
compinit

bindkey -e
