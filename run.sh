#!/usr/bin/env bash
./build.sh
pushd dist
npx node index.js --windowed
