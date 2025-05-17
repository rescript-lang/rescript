#!/usr/bin/env bash

set -e

check_statically_linked() {
    local dir=$1
    local all_statically_linked=true

    for file in "$dir"/*; do
        if [ -f "$file" ]; then
            if file "$file" | grep -Eq "statically linked|static-pie linked"; then
                echo "$file is statically linked."
            else
                echo "$file is NOT statically linked."
                all_statically_linked=false
            fi
        fi
    done

    if $all_statically_linked; then
        echo "All files in $dir are statically linked executables."
    else
        echo "Error: Not all files in $dir are statically linked executables."
        exit 1
    fi
}

# rescript
mv lib-ocaml lib/ocaml

# @rescript/{target}
chmod +x binaries-*/*.exe
mv -f binaries-darwin-x64/* "packages/@rescript/darwin-x64/bin"
mv -f binaries-darwin-arm64/* "packages/@rescript/darwin-arm64/bin"
mv -f binaries-linux-x64/* "packages/@rescript/linux-x64/bin"
mv -f binaries-linux-arm64/* "packages/@rescript/linux-arm64/bin"
mv -f binaries-win32-x64/* "packages/@rescript/win32-x64/bin"
check_statically_linked "packages/@rescript/linux-x64/bin"
check_statically_linked "packages/@rescript/linux-arm64/bin"

# @rescript/std
mkdir -p packages/std/lib
cp -R lib/es6 lib/js packages/std/lib
