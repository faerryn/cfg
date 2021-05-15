#!/bin/sh

set -euf

XRESOURCES="${XDG_CONFIG_HOME}"/Xresources
[ -f "$XRESOURCES" ] && xrdb -load "$XRESOURCES"

exec dwm
