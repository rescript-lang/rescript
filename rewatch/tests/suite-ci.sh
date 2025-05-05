#!/bin/bash
# Make sure we are in the right directory
cd $(dirname $0)

source ./utils.sh
(cd ../testrepo && yarn install --immutable)

bold "Check if build exists"
if test -f ../target/release/rewatch; 
then
  success "Build exists"
else 
  error "Build does not exist. Exiting..."
  exit 1
fi

bold "Make sure the testrepo is clean"
if git diff --exit-code ../testrepo &> diff.txt; 
then
  rm diff.txt
  success "Testrepo has no changes"
else 
  error "Testrepo is not clean to start with"
  cat diff.txt
  rm diff.txt
  exit 1
fi

./compile.sh && ./watch.sh && ./lock.sh && ./suffix.sh
