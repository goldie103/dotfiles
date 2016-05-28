# -*- sh -*-
# windows key modifier
set $mod Mod4

# proper vim keys
set $up k
set $down j
set $left h
set $right l

# window title font
font pango:monospace 8

# mouse+$mod to move floating windows
floating_modifier $mod

# start a terminal
bindsym $mod+grave exec i3-sensible-terminal

# kill focused window
bindsym $mod+Shift+q kill
bindsym $mod+d kill

# start program launcher
bindsym $mod+Return exec rofi -show run
# bindsym $mod+Return exec --no-startup-id i3-dmenu-desktop

# change focus
bindsym $mod+$left focus left
bindsym $mod+$down focus down
bindsym $mod+$up focus up
bindsym $mod+$right focus right
bindsym $mod+Left focus left
bindsym $mod+Down focus down
bindsym $mod+Up focus up
bindsym $mod+Right focus right

# move focused window
bindsym $mod+Shift+$left move left
bindsym $mod+Shift+$down move down
bindsym $mod+Shift+$up move up
bindsym $mod+Shift+$right move right
bindsym $mod+Shift+Left move left
bindsym $mod+Shift+Down move down
bindsym $mod+Shift+Up move up
bindsym $mod+Shift+Right move right

# splits
bindsym $mod+a split h
bindsym $mod+s split v

# enter fullscreen mode for the focused container
bindsym $mod+f fullscreen toggle

# change container layout (stacked, tabbed, toggle split)
bindsym $mod+q layout stacking
bindsym $mod+w layout tabbed
bindsym $mod+e layout toggle split

# toggle tiling / floating
bindsym $mod+Shift+space floating toggle
# change focus between tiling / floating windows
bindsym $mod+space focus mode_toggle

# focus the parent container
bindsym $mod+p focus parent
# focus the child container
#bindsym $mod+Shift+p focus child

# switch to workspace
bindsym $mod+1 workspace 1
bindsym $mod+2 workspace 2
bindsym $mod+3 workspace 3
bindsym $mod+4 workspace 4
bindsym $mod+5 workspace 5
bindsym $mod+6 workspace 6
bindsym $mod+7 workspace 7
bindsym $mod+8 workspace 8
bindsym $mod+9 workspace 9
bindsym $mod+0 workspace 10
# move focused container to workspace
bindsym $mod+Shift+1 move container to workspace 1
bindsym $mod+Shift+2 move container to workspace 2
bindsym $mod+Shift+3 move container to workspace 3
bindsym $mod+Shift+4 move container to workspace 4
bindsym $mod+Shift+5 move container to workspace 5
bindsym $mod+Shift+6 move container to workspace 6
bindsym $mod+Shift+7 move container to workspace 7
bindsym $mod+Shift+8 move container to workspace 8
bindsym $mod+Shift+9 move container to workspace 9
bindsym $mod+Shift+0 move container to workspace 10

# reload the configuration file
bindsym $mod+Shift+Escape reload
# restart i3 inplace (preserves your layout/session, can be used to upgrade i3)
bindsym $mod+Control+Escape restart
# exit i3 (logs you out of your X session)
bindsym $mod+Shift+Control+Escape exec "i3-nagbar -t warning -m 'You pressed the exit shortcut. Do you really want to exit i3? This will end your X session.' -b 'Yes, exit i3' 'i3-msg exit'"

# resize window (you can also use the mouse for that)
mode "resize" {
	bindsym $left resize shrink width 10 px or 10 ppt
	bindsym $down resize grow height 10 px or 10 ppt
	bindsym $up resize shrink height 10 px or 10 ppt
	bindsym $right resize grow width 10 px or 10 ppt
	bindsym Left resize shrink width 10 px or 10 ppt
	bindsym Down resize grow height 10 px or 10 ppt
	bindsym Up resize shrink height 10 px or 10 ppt
	bindsym Right resize grow width 10 px or 10 ppt
	bindsym $mod+r mode "default"
	bindsym Return mode "default"
	bindsym Escape mode "default"
}
bindsym $mod+r mode "resize"

# Start i3bar to display a workspace bar (plus the system information i3status
# finds out, if available)
bar {
	status_command i3status
	position top
}


# Appearance
for_window [class="^.*"] border pixel 0
gaps inner 12
gaps outer 4

# wallpaper
exec --no-startup-id feh --bg-fill ~/Pictures/wall/wall --no-fehbg
exec compton -b --config ~/.config/compton/config.conf
