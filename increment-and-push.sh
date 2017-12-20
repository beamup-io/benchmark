#!/bin/bash

last_revision=$(cat ./benchmark_revision)
start_revision=$(($last_revision + 1))
end_revision=$(($start_revision + 60))

for i in $(seq $start_revision $end_revision); do
  echo "Revision $i"

  echo "$i" > ./benchmark_revision

  git add .
  git commit -m "Rev $i"
  git push

  sleep 180
done
