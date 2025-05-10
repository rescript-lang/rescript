source "./utils.sh"
cd ../testrepo

bold "Test: It should lock - when watching"

sleep 1

if rewatch clean &> /dev/null;
then
  success "Repo Cleaned"
else 
  error "Error Cleaning Repo"
  exit 1
fi

exit_watcher() { 
  # Try to find child process, if not found just kill the process directly
  rm lib/rewatch.lock
}

rewatch_bg watch > /dev/null 2>&1 &

sleep 1

if rewatch watch | grep 'Could not start Rewatch:' &> /dev/null; 
then
  # rm output.txt
  success "Lock is correctly set"
  exit_watcher
else 
  # rm output.txt
  error "Not setting lock correctly"
  exit_watcher
  exit 1
fi

sleep 1

touch tmp.txt
rewatch_bg watch > tmp.txt 2>&1 &
success "Watcher Started"

sleep 1

if cat tmp.txt | grep 'Could not start Rewatch:' &> /dev/null; 
then
  error "Lock not removed correctly"
  exit_watcher
  exit 1
else
  success "Lock removed correctly"
  exit_watcher
fi

rm tmp.txt