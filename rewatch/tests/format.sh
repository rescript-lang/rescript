source "./utils.sh"
cd ../testrepo

bold "Test: It should format all files"

git diff --name-only ./
error_output=$("$REWATCH_EXECUTABLE" format --all)
git_diff_file_count=$(git diff --name-only ./ | wc -l | xargs)
if [ $? -eq 0 ] && [ $git_diff_file_count -eq 8 ];
then
    success "Test package formatted. Got $git_diff_file_count changed files."
    git restore .
else
    error "Error formatting test package"
    echo "Expected 8 files to be changed, got $git_diff_file_count"
    echo $error_output
    exit 1
fi

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
