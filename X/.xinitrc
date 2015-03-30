#!/bin/sh
#
# ~/.xinitrc
#
# Executed by startx (run your window manager from here)

# Test if $1 is available
isavailable() {
    type "$1" &>/dev/null
}

# WTF does this do?
if [ -d /etc/X11/xinit/xinitrc.d ]; then
    for f in /etc/X11/xinit/xinitrc.d/*; do
        [ -x "$f" ] && . "$f"
    done
    unset f
fi

# Arch wiki says this helps Java programs
export _JAVA_AWT_WM_NONREPARENTING=1

xrdb -merge .Xresources

# Set the background image and the mouse pointer
feh --bg-fill /home/anthony/Pictures/beach.png
xsetroot -cursor_name left_ptr -bg "#bae4ee"

# Display a systemtray in the top-right
trayer --edge top --align right --SetDockType true --SetPartialStrut true \
       --expand true --width 10 --height 30 --transparent true --alpha 255 &

eval $(/usr/bin/gnome-keyring-daemon --start --components=pkcs11,secrets,ssh)
export SSH_AUTH_SOCK
#export GNOME_KEYRING_SOCKET
#export GNOME_KEYRING_PID

## Autostart
# Start the screen locker and the program that makes it turn on after suspend
#isavailable xscreensaver && xscreensaver -no-splash &
#isavailable xss-lock && xss-lock -- xscreensaver-command -lock &

# Start redshift
#isavailable redshift && redshift -l 34.11:-118.41 &

# Start urxvt daemon
exec ./bin/urxvtcd

# Start the window manager
exec xmonad
