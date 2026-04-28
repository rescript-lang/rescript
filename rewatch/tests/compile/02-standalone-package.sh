#!/bin/bash
cd $(dirname $0)
source "../utils.sh"
cd ../../testrepo

bold "Test: Standalone package can build via rescript from package folder"

rewatch clean &> /dev/null

pushd ./packages/standalone > /dev/null
error_output=$("$REWATCH_EXECUTABLE" build 2>&1)
if [ $? -eq 0 ];
then
  success "Standalone package built"
else
  error "Error building standalone package"
  printf "%s\n" "$error_output" >&2
  popd > /dev/null
  exit 1
fi
popd > /dev/null
