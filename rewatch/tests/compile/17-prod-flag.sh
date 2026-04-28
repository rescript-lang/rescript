#!/bin/bash
cd $(dirname $0)
source "../utils.sh"
cd ../../testrepo

bold "Test: --prod flag skips dev-dependencies and dev sources"

rewatch clean &> /dev/null

# Test 1: A nonexistent dev-dependency should cause a normal build to fail
# but succeed with --prod
replace "s|@testrepo/pure-dev\"]|@testrepo/pure-dev\",\"@testrepo/nonexistent-pkg\"]|" rescript.json

rewatch build &> /dev/null
if [ $? -ne 0 ];
then
  success "Build without --prod correctly failed with nonexistent dev-dependency"
else
  error "Build without --prod should have failed with nonexistent dev-dependency"
  git checkout -- rescript.json
  exit 1
fi

rewatch build --prod &> /dev/null
if [ $? -eq 0 ];
then
  success "Build with --prod succeeded despite nonexistent dev-dependency"
else
  error "Build with --prod should have succeeded by skipping dev-dependencies"
  git checkout -- rescript.json
  exit 1
fi

rewatch clean &> /dev/null
git checkout -- rescript.json

# Test 2: --prod should skip dev source files
rewatch clean &> /dev/null
rewatch build --prod &> /dev/null
if [ $? -ne 0 ];
then
  error "Build with --prod failed"
  exit 1
fi

# Dev sources in with-dev-deps/test/ should NOT be compiled
file_count=$(find ./packages/with-dev-deps/test -name "*.mjs" 2>/dev/null | wc -l | tr -d ' ')
if [ "$file_count" -eq 0 ];
then
  success "--prod skipped dev sources in with-dev-deps/test"
else
  error "Expected 0 compiled dev source files in with-dev-deps/test, found $file_count"
  exit 1
fi

# Dev sources in pure-dev/dev/ should NOT be compiled
file_count=$(find ./packages/pure-dev/dev -name "*.mjs" 2>/dev/null | wc -l | tr -d ' ')
if [ "$file_count" -eq 0 ];
then
  success "--prod skipped dev sources in pure-dev/dev"
else
  error "Expected 0 compiled dev source files in pure-dev/dev, found $file_count"
  exit 1
fi

rewatch clean &> /dev/null

# Test 3: Without --prod, dev sources should be compiled
rewatch build &> /dev/null
if [ $? -ne 0 ];
then
  error "Build without --prod failed"
  exit 1
fi

file_count=$(find ./packages/with-dev-deps/test -name "*.mjs" 2>/dev/null | wc -l | tr -d ' ')
if [ "$file_count" -eq 1 ];
then
  success "Without --prod, dev sources in with-dev-deps/test are compiled"
else
  error "Expected 1 compiled dev source file in with-dev-deps/test, found $file_count"
  exit 1
fi

file_count=$(find ./packages/pure-dev/dev -name "*.mjs" 2>/dev/null | wc -l | tr -d ' ')
if [ "$file_count" -eq 1 ];
then
  success "Without --prod, dev sources in pure-dev/dev are compiled"
else
  error "Expected 1 compiled dev source file in pure-dev/dev, found $file_count"
  exit 1
fi

rewatch clean &> /dev/null
