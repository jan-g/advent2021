#!/bin/sh

for i in {1..25}; do sed -e "s/%%DAY%%/$i/g" DayXX.tmpl > Day$i.hs; done
