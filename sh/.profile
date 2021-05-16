# XDG base directories
export XDG_CACHE_HOME="$XDG_RUNTIME_DIR"/cache
export XDG_CONFIG_DIRS=/etc/xdg
export XDG_CONFIG_HOME="$HOME"/.config
export XDG_DATA_HOME="$HOME"/.local/share
export XDG_DATA_DIRS=/usr/local/share:/usr/share:"$XDG_DATA_DIRS"

# PATH
prependpath () {
	case ":$PATH:" in
		*:"$1":*)
			;;
		*)
			PATH="$1${PATH:+:$PATH}"
	esac
}
prependpath "$HOME"/.local/bin
unset prependpath
export PATH

# less
export LESSHISTFILE=-
export PAGER='less --mouse'

# inputrc
export INPUTRC="$XDG_CONFIG_HOME"/readline/inputrc

# X11
export XAUTHORITY="$XDG_RUNTIME_DIR"/Xauthority

# Rust
export CARGO_HOME="$XDG_DATA_HOME"/cargo

# NodeJS
export NODE_REPL_HISTORY="$XDG_DATA_HOME"/node_repl_history

# Python
export PYTHONSTARTUP="$XDG_CONFIG_HOME"/python_history

# terminfo
export TERMINFO="$XDG_DATA_HOME"/terminfo
export TERMINFO_DIRS="$TERMINFO":/usr/share/terminfo
