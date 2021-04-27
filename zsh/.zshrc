PS1='%F{green}%1~%f ) '

SAVEHIST=10000
HISTFILE="${XDG_DATA_HOME}"/zhistory

setopt APPEND_HISTORY INC_APPEND_HISTORY
setopt HIST_IGNORE_ALL_DUPS HIST_SAVE_NO_DUPS HIST_IGNORE_SPACE HIST_NO_STORE
setopt HIST_LEX_WORDS
setopt HIST_REDUCE_BLANKS

alias diff='\diff --color=auto'
alias grep='\grep --color=auto'

alias fd='\fd --hidden'
alias rg='\rg --hidden'

alias ls='\ls -hvxCFX --color=auto --group-directories-first'
alias ll='ls -g'
alias la='ls -gA'

autoload -U compinit
compinit -d "${XDG_CACHE_HOME}"/zcompdump
