#!/bin/sh

set -euf

picom &
pgrep redshift || redshift &

exec bspwm
