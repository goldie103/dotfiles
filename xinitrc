# source everything in /etc/X11/xinit/xinitrc.d
if [ -d /etc/X11/xinit/xinit.d ]; then
  for f in /etc/X11/xinit/xinitrc.d/?*.sh; do
    [ -x "$f" ] && . "$f"
  done
  unset f
fi

# this won't work through xorg for some reason
xrandr --output HDMI-1 --primary --auto --output DVI-I-0 --auto --left-of HDMI-1

[[ -f $XDG_CONFIG_HOME/X11/Xresources ]] && xrdb -merge -I$XDG_CONFIG_HOME/X11 $XDG_CONFIG_HOME/X11/Xresources

# window manager yay
exec awesome
