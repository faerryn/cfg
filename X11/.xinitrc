#!/bin/sh

set -euf

XRESOURCES="${XDG_CONFIG_HOME}"/Xresources
[ -f "${XRESOURCES}" ] && xrdb -load "${XRESOURCES}"

pgrep -x pipewire || pipewire &
pgrep -x pipewire-pulse || pipewire-pulse &
pgrep -x unclutter || unclutter &

exec dwm
