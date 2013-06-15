#!/bin/sh

cat `find . -name *.ads` | wc -l
cat `find . -name *.adb` | wc -l
