#!/bin/bash

set -e

# Increment before each push
benchmark_revision=14

BENCHMARK_URL=http://163.172.147.195:8080


git config --global user.email "beamup@example.com"
git config --global user.name "BeamUp Benchmark Bot"

start_time=""

main () {
  start "install"
  curl https://get.beamup.io/install | /bin/sh
  end "install"

  beamup new test_erlang_one
  cd test_erlang_one

  start "full_build_without_cache"
  BEAMUP_STORE=$HOME/.beamup-store beamup build
  end "full_build_without_cache"
}

# Private

start () {
  echo "### Start '$1'"
  start_time=$(date +%s%3N)
}

end () {
  end_time=$(date +%s%3N)
  diff_ms=$(($end_time - $start_time))
  echo "### Finished '$1' took $diff_ms ms"

  result="revision=$benchmark_revision"
  result="$result&subject=$BENCHMARK_SUBJECT"
  result="$result&step=$1"
  result="$result&time=$diff_ms"

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
