PS1='%F{green}%1~%f %(?..%F{red})%)%f '

HISTSIZE=10000
SAVEHIST="$HISTSIZE"
HISTFILE="$XDG_DATA_HOME"/zhistory

alias diff='\diff --color=auto'
alias grep='\grep --color=auto'

alias fd='\fd --hidden'
alias rg='\rg --hidden'

alias ls='\ls -hvFX --color=auto --group-directories-first'
alias ll='ls -g'
alias la='ls -gA'

setopt HIST_IGNORE_ALL_DUPS HIST_IGNORE_SPACE HIST_NO_STORE
setopt HIST_LEX_WORDS HIST_REDUCE_BLANKS

autoload -U compinit
compinit -d "${XDG_CACHE_HOME}"/zcompdump
