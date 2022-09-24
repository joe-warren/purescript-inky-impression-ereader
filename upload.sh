#!/usr/bin/env bash
npx spago bundle-app
scp index.js pi@raspberrypi:ereader-scratch-2/index.js
scp python_modules/buttons.py  pi@raspberrypi:ereader-scratch-2/buttons.py
