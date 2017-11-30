#!/bin/bash

set -e

echo "### Running 'curl https://get.beamup.io/install | /bin/sh'"
time (curl https://get.beamup.io/install | /bin/sh)

echo "### Success"
