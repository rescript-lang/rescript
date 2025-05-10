#!/bin/bash
overwrite() { echo -e "\r\033[1A\033[0K$@"; }
success() { echo -e "- ✅ \033[32m$1\033[0m"; }
error() { echo -e "- 🛑 \033[31m$1\033[0m"; }
bold() { echo -e "\033[1m$1\033[0m"; }
rewatch() { RUST_BACKTRACE=1 $REWATCH_EXECUTABLE --no-timing=true $@; }
rewatch_bg() { RUST_BACKTRACE=1 nohup $REWATCH_EXECUTABLE --no-timing=true $@; }

replace() {
  if [[ $OSTYPE == 'darwin'* ]];
  then
    sed -i '' $1 $2;
  else
    sed -i $1 $2;
  fi
}
