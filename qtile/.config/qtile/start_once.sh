#!/bin/sh
xrandr -s 1360x768 
#xrandr -s 1920x1080 
$HOME/.local/bin/tray_items &
#picom --experimental-backends &
# xcompmgr &
lxsession & 
emacs --daemon &
