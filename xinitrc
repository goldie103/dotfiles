#!/bin/sh

# merge in defaults and keymaps
userresources=$HOME/.Xresources
usermodmap=$HOME/.Xmodmap
sysresources=/etc/X11/xinit/.Xresources
sysmodmap=/etc/X11/xinit/.Xmodmap
[ -f $sysresources ] && xrdb -merge $sysresources
[ -f $sysmodmap ] && xmodmap $sysmodmap
[ -f "$userresources" ] && xrdb -merge "$userresources"
[ -f "$usermodmap" ] && xmodmap "$usermodmap"

if [ -d /etc/X11/xinit/xinitrc.d ] ; then
 for f in /etc/X11/xinit/xinitrc.d/?*.sh ; do
  [ -x "$f" ] && . "$f"
 done
 unset f
fi

# merge Xresources
[[ -f $XDG_CONFIG_HOME/X11/Xresources ]] &&
  xrdb -merge -I$XDG_CONFIG_HOME/X11 $XDG_CONFIG_HOME/X11/Xresources

# set up monitors if on my desktop
[[ $HOST -eq "lyngbakr" ]] &&
  xrandr --output HDMI-1 --primary --auto \
	 --output DVI-I-0 --auto --left-of HDMI-1

# daemons
#emacs --daemon

# start window manager
exec i3 -c$XDG_CONFIG_HOME/i3/init.conf

