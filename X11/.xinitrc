#!/bin/sh

set -euf

export MOZ_X11_EGL=1
export SDL_VIDEODRIVER=x11
export XDG_TERMINAL=xfce4-terminal

xrdb -merge "${XDG_CONFIG_HOME}"/Xresources

exec xfce4-session
