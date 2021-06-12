export XDG_CACHE_HOME="$XDG_RUNTIME_DIR"/cache
export XDG_CONFIG_DIRS=/etc/xdg
export XDG_CONFIG_HOME="$HOME"/.config
export XDG_DATA_HOME="$HOME"/.local/share
export XDG_DATA_DIRS=/usr/local/share:/usr/share:"$XDG_DATA_DIRS"

export CARGO_HOME="$XDG_DATA_HOME"/cargo
export LESSHISTFILE="$XDG_DATA_HOME"/lesshst
export NODE_REPL_HISTORY="$XDG_DATA_HOME"/node_repl_history
export PYTHONSTARTUP="$XDG_CONFIG_HOME"/pythonstartup.py

export PATH="$HOME"/local/bin:"$PATH"
