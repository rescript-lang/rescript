source "./legacy-utils.sh"
cd ../testrepo/packages/compiled-by-legacy

bold "Test: It should use the legacy build system"

error_output=$(rewatch_legacy clean 2>&1 >/dev/null)

if [ $? -eq 0 ];
then
    success "Test package cleaned"
else
    error "Error cleaning test package"
    printf "%s\n" "$error_output" >&2
    exit 1
fi

error_output=$(rewatch_legacy build 2>&1 >/dev/null)

if [ $? -eq 0 ];
then
    success "Test package built"
else
    error "Error building test package"
    printf "%s\n" "$error_output" >&2
    exit 1
fi

if git diff --exit-code ./;
then
  success "Test package has no changes"
else
  error "Build has changed"
  exit 1
fi
