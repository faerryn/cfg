#!/bin/sh

set -euf

export _JAVA_AWT_WM_NONREPARENTING=1
export MOZ_X11_EGL=1

exec emacs --exwm
