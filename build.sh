#!/usr/bin/env bash
npx spago bundle-app --to dist/index.js
cp -r python_modules dist
cp -r assets dist
