#!/bin/bash

set -e

export PATH="~/.beamup/cli:$PATH"

echo "### Running 'curl https://get.beamup.io/install | /bin/sh'"
time (curl https://get.beamup.io/install | /bin/sh)

echo "### Success"
