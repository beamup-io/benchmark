#!/bin/bash

curl https://beamup-benchmark.now.sh > results.json
node ./tally.js
