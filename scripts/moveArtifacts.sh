#!/bin/bash
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

chmod +x binaries-*/*.exe rewatch-*/*.exe

mkdir darwin darwinarm64 linux linuxarm64 win32

mv binaries-darwin/* rewatch-darwin/* darwin
mv binaries-darwinarm64/* rewatch-darwinarm64/* darwinarm64
mv binaries-linux/* rewatch-linux/* linux
mv binaries-linuxarm64/* rewatch-linuxarm64/* linuxarm64
mv binaries-win32/* rewatch-win32/* win32

mv lib-ocaml lib/ocaml
mv ninja/COPYING ninja.COPYING

check_statically_linked "linux"
check_statically_linked "linuxarm64"
