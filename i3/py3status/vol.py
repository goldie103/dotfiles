# -*- coding: utf-8 -*-
"""
Display current sound volume using amixer.

Tweaked from py3status volume_level by using blocks for different volume
levels, and configuration options for i3-gaps.

Format status string parameters:
    {icon} Icon as derived from blocks
    {vol} Percentage volume

Requires:
    alsa-utils: (tested with alsa-utils 1.0.29-1)
"""

import re

from subprocess import check_output, call
from time import time


class Py3status:
    button_down = 0         # Button to decrement volume
    button_mute = 0         # Button to mute
    button_up = 0           # Button to increment volume
    button_full = 0          # Button to change to max volume
    volume_delta = 5        # Amount to change volume by
    cache_timeout = 10      # Cache timeout
    channel = 'Master'      # Channel to use
    device = 'default'      # Device to use
    blocks = ["_", "▁", "▂", "▃", "▄", "▅", "▆", "▇", "█"]  # Icons to represent volume
    format = u'{icon}: {vol}%'  # Format of output
    threshold_bad = 20      # Threshold for low volume
    threshold_degraded = 50 # Threshold for degraded volume
    color = None
    # i3-gaps specific settings
    indicator = "color"     # What to use as an indicator; 'color', 'background', 'border'
    background = None
    border = None
    border_top = 0
    border_bottom = 0
    border_left = 0
    border_right = 0

    def _get_color(self, i3, lv):
        """Return a color from volume using thresholds"""
        if lv <= self.threshold_bad:
            return i3["color_bad"]
        if lv <= self.threshold_degraded:
            return i3["color_degraded"]
        if lv >= 100:
            return i3["color_good"]
        return None

    def _get_icon(self, lv):
        """Return an icon from volume determined by blocks"""
        if lv <= 0:
            return self.blocks[0]
        if lv >= 100:
            return self.blocks[-1]
        inc = int(100 / len(self.blocks))
        for i, icon in enumerate(self.blocks):
            if lv <= (i+1) * inc:
                return icon

    def _get_stats(self):
        """Return current channel volume and mute status using amixer"""

        # attempt to find a percentage value in square brackets
        parsed = re.search("(?P<vol>{vol}).*?(?P<muted>{mute})".format(
                vol=r'(?<=\[)\d{1,3}(?=%\])',
                mute=r'(?<=\[)\w{2,3}(?=\])'),
            check_output(["amixer", "-D", self.device, "sget", self.channel]).decode('utf-8'))
        return int(parsed.group("vol")), parsed.group("muted") == "off"

    def vol(self, i3_output, i3):

        lv, is_mute = self._get_stats()
        if is_mute:
            lv = 0

        icon = ""
        if "{icon}" in self.format:
            icon = self._get_icon(lv)

        response = {
            'cached_until': time() + self.cache_timeout,
            'full_text': self.format.format(icon=icon, vol=lv),
            'color': self.color,
            'background': self.background,
            'border': self.border,
            'border_top': self.border_top,
            'border_bottom': self.border_bottom,
            'border_left': self.border_left,
            'border_right': self.border_right,
        }
        if self.indicator:
            response.update({self.indicator: self._get_color(i3, lv)})
        return response

    def on_click(self, i3s_output_list, i3s_config, event):
        """Volume up/down and toggle mute. """
        button = event['button']
        CMD = ["amixer", "-q", "-D", self.device, "sset", self.channel]
        if self.button_up and button == self.button_up:         # up
            call(CMD + [str(self.volume_delta) + "%+"])
        elif self.button_down and button == self.button_down:   # down
            call(CMD + [str(self.volume_delta) + "%-"])
        elif self.button_mute and button == self.button_mute:   # mute
            call(CMD + ["toggle"])
        elif self.button_full and button == self.button_full:   # full
            call(CMD + ["100%"])


if __name__ == "__main__":
    """Run module in test mode"""
    from py3status.module_test import module_test
    module_test(Py3status, config={
        'color_bad': '#FF0000',
        'color_degraded': '#FFFF00',
        'color_good': '#00FF00',
    })
