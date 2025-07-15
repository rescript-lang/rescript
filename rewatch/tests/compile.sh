#!/bin/bash
cd $(dirname $0)
source "./utils.sh"
cd ../testrepo

bold "Test: It should compile"

error_output=$(rewatch clean 2>&1)
if [ $? -eq 0 ];
then
  success "Repo Cleaned"
else
  error "Error Cleaning Repo"
  printf "%s\n" "$error_output" >&2
  exit 1
fi

error_output=$(rewatch 2>&1)
if [ $? -eq 0 ];
then
  success "Repo Built"
else
  error "Error Building Repo"
  printf "%s\n" "$error_output" >&2
  exit 1
fi


if git diff --exit-code ./;
then
  success "Testrepo has no changes"
else
  error "Build has changed"
  exit 1
fi

node ./packages/main/src/Main.mjs > ./packages/main/src/output.txt

mv ./packages/main/src/Main.res ./packages/main/src/Main2.res
rewatch build --snapshot-output &> ../tests/snapshots/rename-file.txt
mv ./packages/main/src/Main2.res ./packages/main/src/Main.res

# Rename a file with a dependent - this should trigger an error
mv ./packages/main/src/InternalDep.res ./packages/main/src/InternalDep2.res
rewatch build --snapshot-output &> ../tests/snapshots/rename-file-internal-dep.txt
# normalize paths so the snapshot is the same on all machines
normalize_paths ../tests/snapshots/rename-file-internal-dep.txt
mv ./packages/main/src/InternalDep2.res ./packages/main/src/InternalDep.res

# Rename a file with a dependent in a namespaced package - this should trigger an error (regression)
mv ./packages/new-namespace/src/Other_module.res ./packages/new-namespace/src/Other_module2.res
rewatch build --snapshot-output &> ../tests/snapshots/rename-file-internal-dep-namespace.txt
# normalize paths so the snapshot is the same on all machines
normalize_paths ../tests/snapshots/rename-file-internal-dep-namespace.txt
mv ./packages/new-namespace/src/Other_module2.res ./packages/new-namespace/src/Other_module.res

rewatch build &>  /dev/null
mv ./packages/main/src/ModuleWithInterface.resi ./packages/main/src/ModuleWithInterface2.resi
rewatch build --snapshot-output &> ../tests/snapshots/rename-interface-file.txt
# normalize paths so the snapshot is the same on all machines
normalize_paths ../tests/snapshots/rename-interface-file.txt
mv ./packages/main/src/ModuleWithInterface2.resi ./packages/main/src/ModuleWithInterface.resi
rewatch build &> /dev/null
mv ./packages/main/src/ModuleWithInterface.res ./packages/main/src/ModuleWithInterface2.res
rewatch build --snapshot-output &> ../tests/snapshots/rename-file-with-interface.txt
# normalize paths so the snapshot is the same on all machines
normalize_paths ../tests/snapshots/rename-file-with-interface.txt
mv ./packages/main/src/ModuleWithInterface2.res ./packages/main/src/ModuleWithInterface.res
rewatch build &> /dev/null

# when deleting a file that other files depend on, the compile should fail
rm packages/dep02/src/Dep02.res
rewatch build --snapshot-output &> ../tests/snapshots/remove-file.txt
# normalize paths so the snapshot is the same on all machines
normalize_paths ../tests/snapshots/remove-file.txt
git checkout -- packages/dep02/src/Dep02.res
rewatch build &> /dev/null

# it should show an error when we have a dependency cycle
echo 'Dep01.log()' >> packages/new-namespace/src/NS_alias.res
rewatch build --snapshot-output &> ../tests/snapshots/dependency-cycle.txt
git checkout -- packages/new-namespace/src/NS_alias.res

# it should compile dev dependencies with the --dev flag
rewatch clean --dev &> /dev/null
rewatch build --dev &> /dev/null;
if [ $? -ne 0 ];
then
  error "Failed to compile dev dependencies"
  exit 1
fi

file_count=$(find ./packages/with-dev-deps/test -name *.mjs | wc -l)
expected_file_count=1
if [ "$file_count" -eq $expected_file_count ];
then
  success "Compiled dev dependencies successfully"
else
  error "Expected $expected_file_count files to be compiled with the --dev flag, found $file_count"
  exit 1
fi

error_output=$(rewatch clean --dev 2>&1 >/dev/null)
file_count=$(find ./packages/with-dev-deps -name *.mjs | wc -l)
if [ "$file_count" -eq 0 ];
then
  success "Cleaned dev dependencies successfully"
else
  error "Expected 0 files remaining after cleaning, found $file_count"
  printf "%s\n" "$error_output" >&2
  exit 1
fi


# it should not loop (we had an infinite loop when clean building with a cycle)
rewatch clean &> /dev/null
echo 'Dep01.log()' >> packages/new-namespace/src/NS_alias.res
git checkout -- packages/new-namespace/src/NS_alias.res
rewatch build &> /dev/null

# make sure we don't have changes in the test repo
if git diff --exit-code ./;
then
  success "Output is correct"
else
  error "Output is incorrect"
  exit 1
fi

# make sure there are no new files created by the build
# this could happen because of not cleaning up .mjs files
# after we rename files
new_files=$(git ls-files --others --exclude-standard ./)
if [[ $new_files = "" ]];
then
  success "No new files created"
else
  error "❌ - New files created"
  printf "${new_files}\n"
  exit 1
fi

# see if the snapshots have changed
changed_snapshots=$(git ls-files  --modified ../tests/snapshots)
if git diff --exit-code ../tests/snapshots &> /dev/null;
then
  success "Snapshots are correct"
else
  error "Snapshots are incorrect:"
  # print filenames in the snapshot dir call bold with the filename
  # and then cat their contents
  printf "\n\n"
  for file in $changed_snapshots; do
    bold $file
    # show diff of file vs contents in git
    git diff $file $file
    printf "\n\n"
  done

  exit 1
fi
