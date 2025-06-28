source "./legacy-utils.sh"
cd ../testrepo/packages/compiled-by-legacy

bold "Test: It should use the legacy build system"

error_output=$(rewatch_legacy 2>&1 >/dev/null)
if [ $? -ne 0 ];
then
    error "Error running rewatch legacy"
    echo $error_output
    exit 1
fi

error_output=$(rewatch_legacy clean 2>&1 >/dev/null)
file_count=$(find . -name "*.res.js" | wc -l)
if [ $? -eq 0 ] && [ $file_count -eq 0 ];
then
    success "Test package cleaned"
else
    error "Error cleaning test package. File count was $file_count."
    echo $error_output
    exit 1
fi

error_output=$(rewatch_legacy build 2>&1 >/dev/null)
if [ $? -eq 0 ];
then
    success "Test package built"
else
    error "Error building test package"
    echo $error_output
    exit 1
fi

if git diff --exit-code ./;
then
  success "Test package has no changes"
else
  error "Build has changed"
  exit 1
fi

error_output=$(rewatch_legacy format -all 2>&1 >/dev/null)
git_diff_file_count=$(git diff --name-only ./ | wc -l)
if [ $? -eq 0 ] && [ $git_diff_file_count -eq 1 ];
then
    success "Test package formatted. Got $git_diff_file_count changed files."
else
    error "Error formatting test package"
    echo $error_output
    exit 1
fi