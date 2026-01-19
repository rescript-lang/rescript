#!/bin/bash
# Returns the current platform identifier (e.g., darwin-arm64, linux-x64)
# Used by scripts to find platform-specific binaries

case "$(uname -s)" in
    Darwin)
        case "$(uname -m)" in
            arm64) echo "darwin-arm64" ;;
            *)     echo "darwin-x64" ;;
        esac
        ;;
    Linux)
        case "$(uname -m)" in
            aarch64|arm64) echo "linux-arm64" ;;
            *)             echo "linux-x64" ;;
        esac
        ;;
    MINGW*|MSYS*|CYGWIN*)
        echo "win32-x64"
        ;;
    *)
        echo "unknown" >&2
        exit 1
        ;;
esac
