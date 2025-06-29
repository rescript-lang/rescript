source "utils.sh"

rewatch_legacy() { RUST_BACKTRACE=1 "../../$REWATCH_EXECUTABLE" legacy $@; }