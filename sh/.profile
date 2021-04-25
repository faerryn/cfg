# XDG base directories
export XDG_RUNTIME_DIR="${XDG_RUNTIME_DIR:-/run/user/${UID}}"
export XDG_CACHE_HOME="${XDG_RUNTIME_DIR}"/cache
export XDG_CONFIG_DIRS=/etc/xdg
export XDG_CONFIG_HOME="${HOME}"/.config
export XDG_DATA_HOME="${HOME}"/.local/share
export XDG_DATA_DIRS=/usr/local/share:/usr/share:"${XDG_DATA_DIRS}"

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
unset prependpath
export PATH

# Manpage
prependmanpath () {
	case ":${MANPATH}:" in
		*:"${1}":*)
			;;
		*)
			MANPATH="${1}:${MANPATH:-}"
	esac
}
prependmanpath "${HOME}"/.local/share/man
unset prependmanpath
export MANPATH

# less
export LESSHISTFILE=-
export PAGER='less --mouse'

# inputrc
export INPUTRC="${XDG_CONFIG_HOME}"/readline/inputrc

# X11
export XAUTHORITY="${XDG_RUNTIME_DIR}"/Xauthority

# SXHKD
export SXHKD_SHELL=/bin/sh
