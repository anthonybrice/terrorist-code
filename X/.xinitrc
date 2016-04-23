#!/bin/zsh
#
# ~/.xinitrc
#
# Executed by startx (run your window manager from here)

# Test if $1 is available
isavailable() {
    type "$1" &>/dev/null
}

# Sources scripts in xinitrc.d
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
xsetroot -cursor_name left_ptr -bg "#BAE4EE"

#############
# Autostart #
#############

eval $(/usr/bin/gnome-keyring-daemon --start --components=pkcs11,secrets,ssh)
export SSH_AUTH_SOCK

isavailable ./bin/urxvtcd && ./bin/urxvtcd &

isavailable compton && compton -cb --backend glx --vsync opengl-swc --paint-on-overlay

isavailable dunst && dunst -config .dunstrc &

isavailable syndaemon && syndaemon -t -k -d -i 2

#isavailable xss-lock && isavailable xscreensaver \
#    && xss-lock -- xscreensaver-command --lock &

isavailable synclient && synclient PalmDetect=1 TapButton2=3 TapButton3=2 \
    FingerHigh=105 FingerLow=100 HorizTwoFingerScroll=1 PalmMinWidth=14 &

isavailable xset && xset m 3/1 0

# Start the window manager
exec xmonad
