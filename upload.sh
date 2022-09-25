#!/usr/bin/env bash
./build.sh
rsync -r dist/ pi@raspberrypi:ereader-scratch-2/
