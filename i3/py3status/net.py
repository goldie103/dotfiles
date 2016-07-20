# -*- coding: utf-8 -*-
"""
Py3status module that toggles information about the current internet status.

"""

import re
from subprocess import check_output
from time import time

class Py3status:
    
    cache_timeout = 10          # amount of time before refresh
    device = 'wlan0'            # wireless device to check connection of
    msg = 'W: {info}'           # message template to show
    down_msg = 'down'           # what to add if there is no wifi signal
    no_ip_msg = "no ip"         # what to add if there is no IP
    ip_button = 1               # button to toggle between SSID and IP
    public_button = 3           # button to toggle between public and local IP
    # Address to retrieve public IP from. Must return a plaintext IP address.
    public_ip_url = "icanhazip.com"

    def __init__(self):
        self._is_ip = False
        self._is_public_ip = False

        # Guess wifi interface
        try:
            devices = re.findall("Interface\s*([^\s]+)", check_output(["iw", "dev"]).decode('utf-8'))
            if devices and self.device not in devices:
                self.device = devices[0]
        except:
            pass

    def net(self):
        def get_text():
            ssid = re.search("SSID: (.+)", check_output(["iw", "dev", self.device, "link"]).decode('utf-8'))
            if not ssid:
                return self.down_msg
            if self._is_ip:
                if self._is_public_ip:
                    return check_output(["curl", self.public_ip_url]).decode("utf-8").strip()
                ip = re.search("inet\s+([0-9.]+)",
                        check_output(["ip", "addr", "list", self.device]).decode("utf-8"))
                return ip.group(1) if ip else self.no_ip_msg
            return ssid.group(1)

        return {
            'cached_until': time() + self.cache_timeout,
            'full_text': self.msg.format(info=get_text())
        }

    def on_click(self, event):
        button = event["button"]
        if button == self.ip_button:
            # toggle ssid
            self._is_ip = not self._is_ip
        elif button == self.public_button:
            # toggle ip type
            self._is_public_ip = not self._is_public_ip


if __name__ == "__main__":
    from py3status.module_test import module_test
    module_test(Py3status)
