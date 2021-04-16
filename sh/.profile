# XDG base directories
export XDG_RUNTIME_DIR="${XDG_RUNTIME_DIR:-/run/user/${UID}}"
export XDG_CACHE_HOME="${XDG_RUNTIME_DIR}"/cache
export XDG_CONFIG_DIRS=/etc/xdg
export XDG_CONFIG_HOME="${HOME}"/.config
export XDG_DATA_HOME="${HOME}"/.local/share
export XDG_DATA_DIRS=/usr/local/share:/usr/share:"${XDG_DATA_DIRS}"

# Path
export PATH="${HOME}/.local/bin:${PATH}"

# less
export LESSHISTFILE=-
export PAGER="less --mouse"

# inputrc
export INPUTRC="${XDG_CONFIG_HOME}"/readline/inputrc

# X11
export XAUTHORITY="${XDG_RUNTIME_DIR}"/Xauthority
