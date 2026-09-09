#!/bin/bash
cd $(dirname $0)
source "../utils.sh"
cd ../../testrepo

bold "Test: It should not loop when clean building with a cycle"

rewatch clean &> /dev/null
echo 'Dep01.log()' >> packages/new-namespace/src/NS_alias.res
restore_tracked_files packages/new-namespace/src/NS_alias.res
rewatch build &> /dev/null

success "No infinite loop detected"
