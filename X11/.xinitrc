#!/bin/sh

set -euf

export _JAVA_AWT_WM_NONREPARENTING=1
export MOZ_X11_EGL=1

XRESOURCES="${XDG_CONFIG_HOME}"/Xresources
[ -f "${XRESOURCES}" ] && xrdb -merge "${XRESOURCES}"

WALLRC="${XDG_CONFIG_HOME}"/wallrc
[ -f "${WALLRC}" ] && "${WALLRC}" &

xsetroot -cursor_name left_ptr &

pgrep -x picom || picom &
pgrep -x pulseaudio || pulseaudio --exit-idle-time -1 &
pgrep -x redshift || redshift &

XSESSION="${XDG_CONFIG_HOME}"/xsession
"${XSESSION}"

pkill -x picom
pkill -x pulseaudio
pkill -x redshift
