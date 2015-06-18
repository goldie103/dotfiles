;;; conkyrc-mode.el --- syntax highlight for .conkyrc files
;;; Commentary:
;; A generic major mode providing syntax highlighting for .conkyrc files.
;;; Code:

;; * Vars
;; ** Keywords
(defvar conkyrc-keywords
  '("xftfont" "xftalpha" "use_xft" "use_spacer" "uppercase"
    "update_interval_on_battery" "update_interval" "total_run_times"
    "top_name_width" "top_cpu_separate" "times_in_seconds" "text_buffer_size"
    "templateN" "temperature_unit" "stippled_borders" "show_graph_scale"
    "show_graph_range" "short_units" "pop3" "pad_percents" "own_window_type"
    "own_window_transparent" "own_window_argb_value" "own_window_argb_visual"
    "own_window_title" "own_window_hints" "own_window_colour" "own_window_class"
    "own_window" "overwrite_file" "override_utf8_locale" "out_to_x"
    "out_to_stderr" "out_to_ncurses" "out_to_console" "nvidia_display"
    "no_buffers" "net_avg_samples" "music_player_interval" "mpd_port"
    "mpd_password" "mpd_host" "minimum_size" "maximum_width" "max_user_text"
    "max_text_width" "max_specials" "max_port_monitor_connections" "mail_spool"
    "lua_startup_hook" "lua_shutdown_hook" "lua_load" "lua_draw_hook_pre"
    "lua_draw_hook_post" "imlib_cache_size" "imlib_cache_flush_interval" "imap"
    "if_up_strictness" "hddtemp_port" "hddtemp_host" "gap_y" "gap_x"
    "format_human_readable" "font" "extra_newline" "draw_shades" "draw_outline"
    "draw_graph_borders" "draw_borders" "double_buffer" "display"
    "diskio_avg_samples" "disable_auto_reload" "default_shade_color"
    "default_outline_color" "default_graph_size" "default_gauge_size"
    "default_color" "default_bar_size" "cpu_avg_samples" "border_width"
    "border_outer_margin" "border_inner_margin" "background" "append_file"
    "alignment"))

;; ** Constants
(defvar conkyrc-constants
  '("yes" "no" "top_left" "top_right" "bottom_left" "bottom_right" "none"))

;; ** Variables
(defvar conkyrc-variables
  '("addr" "acpiacad" "apter" "acpifan" "acpitemp" "acpitempf" "adt746xcpu"
    "adt746xfan" "alignr" "alignc" "apm_adapter" "apm_battery_life"
    "apm_battery_time" "battery" "buffers" "cached" "color" "cpu" "cpubar"
    "colour" "diskio" "downspeed" "downspeedf" "colour" "else" "exec" "execbar"
    "execgraph" "execi" "execibar" "execigraph" "font" "freq" "freq_g"
    "freq_dyn" "freq_dyn_g" "fs_bar" "fs_free" "fs_free_perc" "fs_size"
    "fs_used" "head" "hr" "i2c" "i8k_ac_status" "i8k_bios" "i8k_buttons_status"
    "i8k_cpu_temp" "i8k_cpu_tempf" "i8k_left_fan_rpm" "i8k_left_fan_status"
    "i8k_right_fan_rpm" "i8k_right_fan_status" "i8k_serial" "i8k_version"
    "if_running" "if_existing" "if_mounted" "kernel" "linkstatus" "loadavg"
    "machine" "mails" "mem" "membar" "memmax" "memperc" "ml_upload_counter"
    "ml_download_counter" "ml_nshared_files" "ml_shared_counter"
    "ml_tcp_upload_rate" "ml_tcp_download_rate" "ml_udp_upload_rate"
    "ml_udp_download_rate" "ml_ndownloaded_files" "ml_ndownloading_files"
    "mpd_artist" "mpd_album" "mpd_bar" "mpd_bitrate" "mpd_status" "mpd_title"
    "mpd_vol" "mpd_elapsed" "mpd_length" "mpd_percent" "new_mails" "nodename"
    "outlinecolor" "pre_exec" "processes" "running_processes" "shadecolor"
    "stippled_hr" "swapbar" "swap" "swapmax" "swapperc" "sysname" "texeci"
    "offset" "tail" "time" "totaldown" "top" "top_mem" "totalup" "updates"
    "upspeed" "upspeedf" "upspeedgraph" "uptime" "uptime_short" "seti_prog"
    "seti_progbar" "seti_credit" "voffset"))

;; * Mode
(define-generic-mode conkyrc-mode
  ;; FIXME inconsistently highlighting things, but better than nothing.
  '("#")
  conkyrc-keywords
  `(
    ;; variable construct
    ("\$\\({.*?}\\|(.*?)\\)" . font-lock-builtin-face)
    ;; variable name
    (,(format "\$[{(]?\(%s\)" (regexp-opt conkyrc-variables))
     1 font-lock-variable-name-face)
    ;; colors
    ("\${?\(color[0-9]?\)}?" 1 font-lock-variable-name-face)
    ;; constants
    (,(regexp-opt conkyrc-constants 'words) . font-lock-constant-face)
    ;; TEXT
    ("TEXT" . font-lock-builtin-face))
  '("\\.conkyrc$" "\\.conky$")
  '()
  "Generic minor mode for conky system monitor rc files.")

(provide 'conkyrc-mode)
;;; conkyrc-mode.el ends here
