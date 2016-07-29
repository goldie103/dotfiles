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
    color_night = None  # Night color
    color = None        # Normal color
    invert = False      # Invert colors at night
    background = None   # Only works with i3-gaps

    def _get_colors(self, is_day):
        if is_day:
            return {}
        if not self.invert:
            return {"color": self.color_night}
        return {"color": self.background, "background": self.color_night}

    def clock(self):
        now = datetime.now()
        colors = self._get_colors(self.sunrise < now.hour < self.sunset)

        response = {
            "cached_until": time() + self.cache_timeout,
            "full_text": now.strftime(self.format),
            "color": self.color,
            "background": self.background
        }
        response.update(colors)
        return response
            

    
