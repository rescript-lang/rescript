#!/bin/bash
cd $(dirname $0)
source "../utils.sh"
cd ../../testrepo

bold "Test: It should format only the current project"

error_output=$(cd packages/file-casing && "$REWATCH_EXECUTABLE" format)
git_diff_file_count=$(git diff --name-only ./ | wc -l | xargs)
if [ $? -eq 0 ] && [ $git_diff_file_count -eq 2 ];
then
    success "file-casing formatted"
    git restore .
else
    error "Error formatting current project file-casing"
    echo "Expected 2 files to be changed, got $git_diff_file_count"
    echo $error_output
    exit 1
fi
