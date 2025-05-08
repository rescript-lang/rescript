source "./utils.sh"
cd ../testrepo

bold "Test: It should watch"

if rewatch clean &> /dev/null;
then
  success "Repo Cleaned"
else 
  error "Error Cleaning Repo"
  exit 1
fi

exit_watcher() { 
  # we need to kill the parent process (rewatch)
  rm lib/rewatch.lock
}

rewatch_bg watch > /dev/null 2>&1 &
success "Watcher Started"

echo 'Js.log("added-by-test")' >> ./packages/main/src/Main.res

sleep 1

if node ./packages/main/src/Main.mjs | grep 'added-by-test' &> /dev/null; 
then
  success "Output is correct"
else 
  error "Output is incorrect"
  exit_watcher
  exit 1
fi

sleep 1

replace '/Js.log("added-by-test")/d' ./packages/main/src/Main.res;

sleep 1

if git diff --exit-code ./  
then
  success "Adding and removing changes nothing"
else 
  error "Adding and removing changes left some artifacts"
  exit_watcher
  exit 1
fi

exit_watcher