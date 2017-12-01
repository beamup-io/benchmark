#!/bin/bash

set -e

# Increment before each push
benchmark_revision=19

BENCHMARK_URL=http://163.172.147.195:8080


git config --global user.email "beamup@example.com"
git config --global user.name "BeamUp Benchmark Bot"

start_time=""

main () {
  echo "### Installing BeamUp"
  start "install"
  curl https://get.beamup.io/install | /bin/sh
  end "install"

  echo "### Creating project scaffold"
  beamup new test_erlang_one
  cd test_erlang_one

  echo "### Building full release, without cache"
  start "full_build_without_cache"
  BEAMUP_STORE=$HOME/.beamup-store beamup build
  end "full_build_without_cache"

  echo "### Making a change to the project"
  cat << EOF > ./apps/test_erlang_one/src/test_erlang_one_app.erl

-module(test_erlang_one_app).
-behaviour(application).
-export([start/2, stop/1]).
start(_StartType, _StartArgs) ->
  io:format("Hello, Joe."), % this line is new
  test_erlang_one_sup:start_link().
stop(_State) ->
  ok.

EOF

  cat ./apps/test_erlang_one/src/test_erlang_one_app.erl

  git add .
  git commit -m 'Add hello joe log output'

  echo "### Building upgrade release, with cache"
  start "upgrade_build_with_cache"
  BEAMUP_STORE=$HOME/.beamup-store beamup build
  end "upgrade_build_with_cache"

  echo "### Clearing cache"
  rm -rf $HOME/.beamup-store

  echo "### Building upgrade release, without cache"
  start "upgrade_build_without_cache"
  BEAMUP_STORE=$HOME/.beamup-store beamup build
  end "upgrade_build_without_cache"
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
