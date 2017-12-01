#!/bin/bash

set -e

BENCHMARK_URL=http://163.172.147.195:8080
benchmark_revision=1

start_time=""

main () {
  start "install"
  curl https://get.beamup.io/install | /bin/sh
  end "install"

  start "full_build_without_cache"
  BEAMUP_STORE='/root/.beamup-store' beamup build
  end "full_build_without_cache"
}

# Private

start () {
  echo "### Start '$1'"
  start_time=$(date +%s.%N)
}

end () {
  end_time=$(date +%s.%N)
  diff=$(echo "$end_time - $start_time" | bc)
  echo "### Finished '$1' took $diff"

  result="revision=$benchmark_revision"
  result="$result&subject=$BENCHMARK_SUBJECT"
  result="$result&step=$1"
  result="$result&time=$diff"

  curl -X POST -d "$result" $BENCHMARK_URL
}

ensure_env () {
  if [ -z "$BENCHMARK_SUBJECT" ]; then
    fail 'Please set $BENCHMARK_SUBJECT'
  fi

  if [ -z "$BENCHMARK_URL" ]; then
    fail 'Please set $BENCHMARK_URL'
  fi
}

fail () {
  echo "$1"
  exit 1
}

ensure_env
main
