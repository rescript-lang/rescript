source "./utils.sh"
cd ../testrepo

bold "Test: It should support custom suffixes"

# Clean Repo
sleep 1
if rewatch clean &> /dev/null;
then
  success "Repo Cleaned"
else
  error "Error Cleaning Repo"
  exit 1
fi

# Replace suffix
replace "s/.mjs/.res.js/g" bsconfig.json

if rewatch build &> /dev/null;
then
  success "Repo Built"
else
  error "Error building repo"
  exit 1
fi

# Count files with new extension
file_count=$(find . -name *.res.js | wc -l)

if [ "$file_count" -eq 20 ];
then
  success "Found files with correct suffix"
else
  error "Suffix not correctly used (file count of $file_count)"
  exit 1
fi

if rewatch clean &> /dev/null;
then
  success "Repo Cleaned"
else
  error "Error Cleaning Repo"
  exit 1
fi

# Restore Suffix
replace "s/.res.js/.mjs/g" bsconfig.json

# Restore original build
if rewatch build &> /dev/null;
then
  success "Repo Built"
else
  error "Error building repo"
  exit 1
fi
