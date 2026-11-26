#!/bin/bash
cd $(dirname $0)
source "../utils.sh"
cd ../../testrepo

bold "Test: Make sure we don't have changes in the test repo"

rewatch clean &> /dev/null
rewatch build &> /dev/null

if git diff --exit-code ./;
then
  success "Output is correct"
else
  error "Output is incorrect"
  exit 1
fi
