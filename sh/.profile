# XDG base directories
export XDG_CACHE_HOME="$XDG_RUNTIME_DIR"/cache
export XDG_CONFIG_DIRS=/etc/xdg
export XDG_CONFIG_HOME="$HOME"/.config
export XDG_DATA_HOME="$HOME"/.local/share
export XDG_DATA_DIRS=/usr/local/share:/usr/share:"$XDG_DATA_DIRS"

# PATH
export PATH="$HOME"/.local/bin:"$PATH"

# less
export LESSHISTFILE=-
export PAGER='less --mouse'

# inputrc
export INPUTRC="$XDG_CONFIG_HOME"/readline/inputrc

# Rust
export CARGO_HOME="$XDG_DATA_HOME"/cargo

# NodeJS
export NODE_REPL_HISTORY="$XDG_DATA_HOME"/node_repl_history

# Python
export PYTHONSTARTUP="$XDG_CONFIG_HOME"/pythonstartup.py

# terminfo
export TERMINFO="$XDG_DATA_HOME"/terminfo
export TERMINFO_DIRS="$TERMINFO":/usr/share/terminfo
