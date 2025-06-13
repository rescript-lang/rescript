#!/bin/bash
overwrite() { echo -e "\r\033[1A\033[0K$@"; }
success() { echo -e "- ✅ \033[32m$1\033[0m"; }
error() { echo -e "- 🛑 \033[31m$1\033[0m"; }
bold() { echo -e "\033[1m$1\033[0m"; }
rewatch() { RUST_BACKTRACE=1 $REWATCH_EXECUTABLE --no-timing=true --snapshot-output=true $@; }
rewatch_bg() { RUST_BACKTRACE=1 nohup $REWATCH_EXECUTABLE --no-timing=true --snapshot-output=true $@; }

# Detect if running on Windows
is_windows() {
  [[ $OSTYPE == 'msys'* || $OSTYPE == 'cygwin'* || $OSTYPE == 'win'* ]];
}

# get pwd with forward slashes
pwd_prefix() {
  if is_windows; then
    # On Windows, escape backslashes for sed and convert to forward slashes for consistent snapshots
    # This ensures paths like C:\a\b are replaced correctly
    # First get the Windows-style path with backslashes
    local win_path=$(pwd -W | sed "s#/#\\\\#g")
    # Then escape the backslashes for sed replacement
    echo $win_path | sed 's#\\#\\\\#g'
  else
    # On Unix-like systems, escape forward slashes for sed
    echo $(pwd | sed "s#/#\\/#g")
  fi
}

# replace the absolute path so the snapshot is the same on all machines
# then normalize the path separators
normalize_paths() {
  if [[ $OSTYPE == 'darwin'* ]];
  then
    sed -i '' "s#$(pwd_prefix)##g" $1;
  else
    if is_windows; then
      sed -i "s#$(pwd_prefix)##g" $1
      sed -i "s#\\\\#/#g" $1
    else
      sed -i "s#$(pwd_prefix)##g" $1;
    fi
  fi
}

replace() {
  if [[ $OSTYPE == 'darwin'* ]];
  then
    sed -i '' $1 $2;
  else
    sed -i $1 $2;
  fi
}
