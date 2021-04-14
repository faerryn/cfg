PS1="$(echo -n '\e[0;32m\W\e[0;39m ) ')"

HISTFILE="${XDG_DATA_HOME:-${HOME}/.local/share}"/bash_history

alias diff='\diff --color=auto'
alias grep='\grep ---color=auto'

alias fd='\fd --hidden'
alias rg='\rg --hidden'

alias ls='\ls -hvxCFX --color=auto --group-directories-first'
alias ll='ls -g'
alias la='ls -gA'
