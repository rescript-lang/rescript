#!/bin/bash
cd $(dirname $0)
source "../utils.sh"
cd ../../testrepo

bold "Test: It should format all files"

git diff --name-only ./
error_output=$("$REWATCH_EXECUTABLE" format)
git_diff_file_count=$(git diff --name-only ./ | wc -l | xargs)
if [ $? -eq 0 ] && [ $git_diff_file_count -eq 9 ];
then
    success "Test package formatted. Got $git_diff_file_count changed files."
    git restore .
else
    error "Error formatting test package"
    echo "Expected 9 files to be changed, got $git_diff_file_count"
    echo $error_output
    exit 1
fi
