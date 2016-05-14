-- {{{ Main
theme = {}
theme.wallpaper = "~/Pictures/wall/AnnapurnaFocus.jpg"
theme.wallpaper2 = "~/Pictures/wall/FLWR-vertical.jpg"
-- }}}

-- {{{ Styles
theme.font      = "hallo sans 11"

-- {{{ Colors
theme.fg_normal  = "#ebdbb2"
theme.fg_focus   = "#d5c4a1"
theme.fg_urgent  = theme.fg_normal
theme.bg_normal  = "#1d2021"
theme.bg_focus   = "#282828"
theme.bg_urgent  = "#fb4934"
theme.bg_systray = theme.bg_normal
-- }}}

-- {{{ Borders
theme.border_width  = 0
theme.border_normal = theme.bg_normal
theme.border_focus  = theme.bg_focus
theme.border_marked = theme.fg_urgent
theme.useless_gap_width = 10
-- }}}

-- There are other variable sets
-- overriding the default one when
-- defined, the sets are:
-- [taglist|tasklist]_[bg|fg]_[focus|urgent]
-- titlebar_[normal|focus]
-- tooltip_[font|opacity|fg_color|bg_color|border_width|border_color]
-- Example:
theme.tasklist_bg_focus = theme.bg_focus
theme.taglist_bg_focus = theme.bg_focus
-- }}}

-- {{{ Widgets
theme.fg_widget        = "#b8bb26"
theme.fg_center_widget = "#98971a"
theme.fg_end_widget    = "#fb4934"
theme.bg_widget        = theme.bg_normal
theme.border_widget    = theme.border_normal
-- }}}

-- {{{ Menu
theme.menu_height = 20
theme.menu_width  = 100
-- }}}

return theme
