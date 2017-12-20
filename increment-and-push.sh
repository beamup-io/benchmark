#!/bin/bash

for i in {40..100}; do
  echo "Revision $i"

  echo "$i" > ./benchmark_revision

  git add .
  git commit -m "Rev $1"
  git push

  sleep 180
done
