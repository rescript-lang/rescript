#!/bin/sh
set -eu

release_file=$1
printf '%s\n' REWATCH_AFTER_BUILD_READY
while [ ! -f "$release_file" ]; do
  sleep 0.05
done
