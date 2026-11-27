#!/bin/bash
cd $(dirname $0)
source "../utils.sh"
cd ../../testrepo

bold "Test: It should format from stdin"

error_output=$(echo "let x = 1" | "$REWATCH_EXECUTABLE" format --stdin .res)
if [ $? -eq 0 ];
then
    success "Stdin formatted successfully"
else
    error "Error formatting from stdin"
    echo $error_output
    exit 1
fi
