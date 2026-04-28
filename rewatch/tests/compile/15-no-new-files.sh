#!/bin/bash
cd $(dirname $0)
source "../utils.sh"
cd ../../testrepo

bold "Test: Make sure there are no new files created by the build"

rewatch clean &> /dev/null
rewatch build &> /dev/null

# This could happen because of not cleaning up .mjs files after we rename files
new_files=$(git ls-files --others --exclude-standard ./)
if [[ $new_files = "" ]];
then
  success "No new files created"
else
  error "New files created"
  printf "${new_files}\n"
  exit 1
fi
