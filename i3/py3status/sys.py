# -*- coding: utf-8 -*-
"""
Display system stats with configuration parameters for i3-gaps
"""

from time import time

class Py3status:
    cache_timeout = 10
    format = "CPU:{cpu_usage}% Mem:{mem_used}/{mem_total}GB ({mem_usage}%)"
    cpu_temp_file = "/sys/class/thermal/thermal_zone0/temp" # file to read cpu temp from
    indicator = 'color' # what to use for indicator: 'color', 'background', or 'border'
    high_threshold = 75 # threshold for high usage
    med_threshold = 40  # threshold for medium usage
    background = None   # background color
    color = None        # text color
    border = None       # border color
    # border widths
    border_bottom = 0
    border_top = 0
    border_left = 0
    border_right = 0

    def _get_memory(self):
        """Return total and used memory as GB, and used/total % from /proc/meminfo

        MemTotal:        3962704 kB
        MemFree:         2278264 kB
        MemAvailable:    2688436 kB
        Buffers:           60076 kB
        Cached:           566852 kB
        ...
        """
        def to_gb(s):
            return float(s.split()[1]) / 1000000

        with open('/proc/meminfo') as f:
            total = to_gb(next(f))
            free = to_gb(next(f))
            next(f)     # discard reading of available space, using free instead
            # Subtract free, buffers, and cached from total to get used space
            used = total - free - sum(to_gb(next(f)) for _ in range(2))

        return total, used, used / total

    def _get_cpu_usage(self):
        """Return CPU usage as nonidle/total % time from /proc/stat

        Time units are in USER_HZ (typically hundredths of a second).
        guest and guest_nice time are included in user and nice, respectively
        and iowait should be added to idle time.

        /proc/stat example:
        cpu  2255 34 2290 22625563 6290 127 456 0 0
        - user: normal processes executing in user mode
        - nice: niced processes executing in user mode
        - system: processes executing in kernel mode
        - idle: twiddling thumbs
        - iowait: waiting for I/O to complete
        - irq: servicing interrupts
        - softirq: servicing softirqs
        - steal: involuntary wait
        - guest: running a normal guest
        - guest_nice: running a niced guest
        """
        with open('/proc/stat') as f:
            line = f.readline()
        cpu = list(map(float, line.split()[1:]))   # exclude 'cpu' and guest stats
        total = sum(cpu)
        return (total - cpu[3] - cpu[4]) / total * 100  # idle time is idle + iowait

    def _get_cpu_temp(self):
        """Return CPU temperature in degrees by reading a specific file

        Use /sys/class/thermal/thermal_zone0/temp by default. Unit is
        100ths of a degree:
        46000
        """
        with open(self.cpu_temp_file) as f:
            temp = float(f.read().strip())
        return temp / 1000

    def _get_color(self, threshold, i3s_config):
        """Return a color based on threshold"""
        if threshold < self.med_threshold:
            return i3s_config['color_good']
        if (threshold < self.high_threshold):
            return i3s_config['color_degraded']
        return i3s_config['color_bad']

    def sys(self, i3s_output_list, i3s_config):
        # get stats
        cpu_usage = self._get_cpu_usage()
        cpu_temp = ''
        if '{cpu_temp}' in self.format:
            cpu_temp = self._get_cpu_temp()
        mem_total, mem_used, mem_usage = self._get_memory()

        # build response
        response = {
            'cached_until': time() + self.cache_timeout,
            'full_text': self.format.format(
                cpu_usage='%.2f' % (cpu_usage),
                cpu_temp=cpu_temp,
                mem_total='%.2f' % (mem_total),
                mem_used='%.2f' % (mem_used),
                mem_usage='%.2f' % (mem_usage),
            ),
            'background': self.background,
            'color': self.color,
            'border': self.border,
            'border_bottom': self.border_bottom,
            'border_top': self.border_top,
            'border_left': self.border_left,
            'border_right': self.border_right,
        }

        # set colors
        threshold = mem_used
        if '{cpu_usage}' in self.format:
            threshold = cpu_usage
            if ('{mem_usage}' in self.format or '{mem_used}' in self.format):
                threshold = max(cpu_usage, mem_usage)
        response[self.indicator] = self._get_color(threshold, i3s_config)

        return response


if __name__ == "__main__":
    """Run module in test mode"""
    from py3status.module_test import module_test
    module_test(Py3status, config={
        'color_bad': '#FF0000',
        'color_degraded': '#FFFF00',
        'color_good': '#00FF00',
    })
