source "./utils.sh"
cd ../testrepo

bold "Test: It should support custom suffixes"

# Clean Repo
sleep 1
error_output=$(rewatch clean 2>&1)
if [ $? -eq 0 ];
then
  success "Repo Cleaned"
else
  error "Error Cleaning Repo"
  printf "%s\n" "$error_output" >&2
  exit 1
fi

# Replace suffix
replace "s/.mjs/.res.js/g" rescript.json

error_output=$(rewatch build 2>&1)
if [ $? -eq 0 ];
then
  success "Repo Built"
else
  error "Error Building Repo"
  printf "%s\n" "$error_output" >&2
  exit 1
fi

# Count files with new extension
file_count=$(find ./packages -name *.res.js | wc -l)

if [ "$file_count" -eq 36 ];
then
  success "Found files with correct suffix"
else
  error "Suffix not correctly used, got $file_count files"
  exit 1
fi

error_output=$(rewatch clean 2>&1)
if [ $? -eq 0 ];
then
  success "Repo Cleaned"
else
  error "Error Cleaning Repo"
  printf "%s\n" "$error_output" >&2
  exit 1
fi

# Restore Suffix
replace "s/.res.js/.mjs/g" rescript.json

# Restore original build
error_output=$(rewatch build 2>&1)
if [ $? -eq 0 ];
then
  success "Repo Built"
else
  error "Error Building Repo"
  printf "%s\n" "$error_output" >&2
  exit 1
fi
