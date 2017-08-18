#!/bin/sh

# merge in defaults and keymaps
userresources="$XDG_CONFIG_HOME/X11/Xresources"
usermodmap="$XDG_CONFIG_HOME/X11/Xmodmap"
sysresources="/etc/X11/xinit/.Xresources"
sysmodmap="/etc/X11/xinit/.Xmodmap"
userbindkeys="$XDG_CONFIG_HOME/X11/xbindkeys"
[[ -f $sysresources ]] && xrdb -merge $sysresources
[[ -f $sysmodmap ]] && xmodmap $sysmodmap
[[ -f $userresources ]] && xrdb -merge $userresources
[[ -f $usermodmap ]] && xmodmap $usermodmap
[[ -f $userbindkeys ]] && xbindkeys -f $userbindkeys

if [ -d /etc/X11/xinit/xinitrc.d ] ; then
 for f in /etc/X11/xinit/xinitrc.d/?*.sh ; do
  [[ -x "$f" ]] && . "$f"
 done
 unset f
fi

# set up monitors if on my desktop
[[ $HOST -eq "lyngbakr" ]] &&
  xrandr --output HDMI-1 --primary --auto \
	 --output DVI-I-0 --auto --left-of HDMI-1

# daemons
#emacs --daemon

# start window manager
exec i3 -c $XDG_CONFIG_HOME/i3/init.conf

