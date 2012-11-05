#!/bin/sh
#
# Formats the data such that my lisp script (see ../wifi-hotspots-prototype.lisp)
#

echo "Converting the wifiData*.csv into a set where I have MAC_ADDR\tSIGNAL for easier processing downroad"

ls wifiData_*.csv | sed -e 's/\.csv$//' | grep -v 'reformatted' | xargs -I{} sh -c "grep 'eduroam' {}.csv | sed -E 's/,/\t/g' | cut -f 2,3 > {}.reformatted.csv"

echo "That's it for now..."
