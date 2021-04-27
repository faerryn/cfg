#!/bin/sh

set -euf

export _JAVA_AWT_WM_NONREPARENTING=1
export MOZ_X11_EGL=1

XRESOURCES="${XDG_CONFIG_HOME}"/Xresources
[ -f "${XRESOURCES}" ] && xrdb -load "${XRESOURCES}"

exec dwm
