#!/bin/bash
cd $(dirname $0)
source "../utils.sh"
cd ../../testrepo

bold "Test: watch.lock blocks watch and stays separate from build.lock"

cleanup() {
  exit_watcher
  rm -f rewatch.log second-watch.log
}

trap cleanup EXIT

error_output=$(rewatch clean 2>&1)
if [ $? -eq 0 ];
then
  success "Repo Cleaned"
else
  error "Error Cleaning Repo"
  printf "%s\n" "$error_output" >&2
  exit 1
fi

clear_locks

rewatch_bg watch > /dev/null 2>&1 &
success "Watcher Started"

if ! wait_for_file "lib/watch.lock" 20; then
  error "watch.lock was not created"
  exit 1
fi

success "watch.lock created"

if rewatch watch > second-watch.log 2>&1;
then
  error "Second watcher should have been blocked by watch.lock"
  cat second-watch.log
  exit 1
elif grep 'Could not start Rescript build:' second-watch.log > /dev/null;
then
  success "Second watcher is blocked by watch.lock"
else
  error "Second watcher failed without the expected lock message"
  cat second-watch.log
  exit 1
fi

build_output=$(rewatch build 2>&1)
build_status=$?

if [ $build_status -ne 0 ];
then
  error "Build should succeed while watch.lock is active"
  printf "%s\n" "$build_output"
  exit 1
fi

success "Build succeeds while watcher is running"

if wait_for_file_gone "lib/build.lock" 10;
then
  success "build.lock is removed after build finishes"
else
  error "build.lock was not removed after build"
  exit 1
fi

if [ -f lib/watch.lock ];
then
  success "watch.lock remains while watcher is running"
else
  error "watch.lock disappeared while watcher was still running"
  exit 1
fi

exit_watcher

if ! wait_for_file_gone "lib/watch.lock" 10; then
  error "watch.lock was not removed"
  exit 1
fi

rewatch_bg watch > rewatch.log 2>&1 &

if wait_for_file "lib/watch.lock" 20; then
  success "Watcher can start again after watch.lock is removed"
else
  error "Watcher did not restart after watch.lock removal"
  cat rewatch.log
  exit 1
fi
