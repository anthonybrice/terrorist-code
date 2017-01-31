#!/usr/bin/env bash

batt=$(upower -i /org/freedesktop/UPower/devices/battery_BAT0 | head -n 20 | tail -n -1)

#batt=$(cut -d " ")
echo $batt
