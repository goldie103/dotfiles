"""
Clock module that shows different colors for time of day

Format specifiers follow that of strftime
"""

from datetime import datetime
from time import time

class Py3status:
    format = "%Y-%m-%d %H:%M:%S"
    cache_timeout = 10
    sunrise = 7         # Hour of sunrise
    sunset = 5 + 12     # Hour of sunset
    night = None        # Night color
    day = None          # Day color
    color = None        # Normal color
    invert = False      # Swap background and foreground at night
    indicator = 'color' # What to change to indicate time; 'background', 'color', 'border'
    # i3-gaps options
    background = None
    border = None
    border_top = None
    border_bottom = None
    border_left = None
    border_right = None

    def clock(self):
        now = datetime.now()
        is_day = self.sunrise < now.hour < self.sunset

        response = {
            "cached_until": time() + self.cache_timeout,
            "full_text": now.strftime(self.format),
            "color": self.color,
            "background": self.background,
            "border": self.border,
            "border_top": self.border_top,
            "border_bottom": self.border_bottom,
            "border_left": self.border_left,
            "border_right": self.border_right,
        }
        response.update({self.indicator: self.day if is_day else self.night})
        return response
            

if __name__ == "__main__":
    """Run module in test mode"""
    from py3status.module_test import module_test
    module_test(Py3status, config={
        'day': '#FF0000',
        'night': '#FFFF00',
    })
    
