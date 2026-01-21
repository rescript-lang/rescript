#!/bin/bash
cd $(dirname $0)
source "../utils.sh"
cd ../../testrepo

bold "Test: It should format a single file"

error_output=$("$REWATCH_EXECUTABLE" format packages/dep01/src/Dep01.res)
git_diff_file_count=$(git diff --name-only ./ | wc -l | xargs)
if [ $? -eq 0 ] && [ $git_diff_file_count -eq 1 ];
then
    success "Single file formatted successfully"
    git restore .
else
    error "Error formatting single file"
    echo $error_output
    exit 1
fi
