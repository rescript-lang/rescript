source "./legacy-utils.sh"
cd ../testrepo/packages/compiled-by-legacy

bold "Test: It should use the legacy build system"

if rewatch_legacy clean &> /dev/null;
then
    success "Test package cleaned"
else
    error "Error cleaning test package"
    exit 1
fi

if rewatch_legacy build &> /dev/null;
then
    success "Test package built"
else
    error "Error building test package"
    exit 1
fi

if git diff --exit-code ./;
then
  success "Test package has no changes"
else
  error "Build has changed"
  exit 1
fi
