# XDG base directories
export XDG_RUNTIME_DIR="${XDG_RUNTIME_DIR:-/run/user/${UID}}"
export XDG_CACHE_HOME="${XDG_RUNTIME_DIR}"/cache
export XDG_CONFIG_DIRS=/etc/xdg
export XDG_CONFIG_HOME="${HOME}"/.config
export XDG_DATA_HOME="${HOME}"/.local/share
export XDG_DATA_DIRS=/usr/local/share:/usr/share:"${XDG_DATA_DIRS}"

# Neovim
export EDITOR=nvim
export VISUAL="${EDITOR}"

# Rust
export RUSTUP_HOME="${XDG_DATA_HOME}"/rustup
export CARGO_HOME="${XDG_DATA_HOME}"/cargo

# Misc
export GRADLE_USER_HOME="${XDG_DATA_HOME}"/gradle
export INPUTRC="${XDG_CONFIG_HOME}"/readline/inputrc
export LESSHISTFILE=-
export NODE_REPL_HISTORY="${XDG_CACHE_HOME}"/node_repl_history
export XAUTHORITY="${XDG_RUNTIME_DIR}"/Xauthority

# Path
prependpath () {
	case ":${PATH}:" in
		*:"${1}":*)
			;;
		*)
			PATH="${1}${PATH:+:${PATH}}"
	esac
}
prependpath "${HOME}"/.local/bin
prependpath "${CARGO_HOME}"/bin
unset prependpath
export PATH
