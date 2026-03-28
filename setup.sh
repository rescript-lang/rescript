#!/bin/sh
# SPDX-License-Identifier: PMPL-1.0-or-later
# setup.sh — Universal setup script for rescript
#
# Detects your shell, platform, and installs prerequisites.
# Then hands off to `just setup` for project-specific configuration.
#
# Usage:
#   curl -fsSL https://raw.githubusercontent.com/hyperpolymath/rescript/main/setup.sh | sh
#   # or after cloning:
#   ./setup.sh
#
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath)

set -eu

# ── Colours (safe — uses symbols too per ADJUST contractile) ──
if [ -t 1 ] && command -v tput >/dev/null 2>&1; then
    RED=$(tput setaf 1 2>/dev/null || true)
    GREEN=$(tput setaf 2 2>/dev/null || true)
    YELLOW=$(tput setaf 3 2>/dev/null || true)
    CYAN=$(tput setaf 6 2>/dev/null || true)
    BOLD=$(tput bold 2>/dev/null || true)
    RESET=$(tput sgr0 2>/dev/null || true)
else
    RED="" GREEN="" YELLOW="" CYAN="" BOLD="" RESET=""
fi

ok()   { printf "  %s[OK]%s   %s\n" "$GREEN" "$RESET" "$1"; }
fail() { printf "  %s[FAIL]%s %s\n" "$RED" "$RESET" "$1"; }
warn() { printf "  %s[WARN]%s %s\n" "$YELLOW" "$RESET" "$1"; }
info() { printf "  %s[INFO]%s %s\n" "$CYAN" "$RESET" "$1"; }

# ── Shell Detection ──
detect_shell() {
    # Check the actual running shell, not just $SHELL
    CURRENT_SHELL="unknown"

    if [ -n "${BASH_VERSION:-}" ]; then CURRENT_SHELL="bash"
    elif [ -n "${ZSH_VERSION:-}" ]; then CURRENT_SHELL="zsh"
    elif [ -n "${FISH_VERSION:-}" ]; then CURRENT_SHELL="fish"
    elif [ -n "${KSH_VERSION:-}" ]; then CURRENT_SHELL="ksh"
    # Check by process name for shells that don't set version vars
    elif command -v ps >/dev/null 2>&1; then
        SHELL_PROC=$(ps -p $$ -o comm= 2>/dev/null || echo "unknown")
        case "$SHELL_PROC" in
            *dash*)    CURRENT_SHELL="dash" ;;
            *tcsh*)    CURRENT_SHELL="tcsh" ;;
            *csh*)     CURRENT_SHELL="csh" ;;
            *elvish*)  CURRENT_SHELL="elvish" ;;
            *nu*)      CURRENT_SHELL="nushell" ;;
            *oil*|*osh*) CURRENT_SHELL="oil" ;;
            *xonsh*)   CURRENT_SHELL="xonsh" ;;
            *murex*)   CURRENT_SHELL="murex" ;;
            *ion*)     CURRENT_SHELL="ion" ;;
            *hilbish*) CURRENT_SHELL="hilbish" ;;
            *oh*)      CURRENT_SHELL="oh" ;;
            *vsh*)     CURRENT_SHELL="vsh" ;;
            *pwsh*|*powershell*) CURRENT_SHELL="powershell" ;;
        esac
    fi

    # Fallback: check $SHELL env var
    if [ "$CURRENT_SHELL" = "unknown" ] && [ -n "${SHELL:-}" ]; then
        case "$SHELL" in
            */bash)  CURRENT_SHELL="bash" ;;
            */zsh)   CURRENT_SHELL="zsh" ;;
            */fish)  CURRENT_SHELL="fish" ;;
            */dash)  CURRENT_SHELL="dash" ;;
            */ksh*)  CURRENT_SHELL="ksh" ;;
            */tcsh)  CURRENT_SHELL="tcsh" ;;
            */csh)   CURRENT_SHELL="csh" ;;
            */vsh)   CURRENT_SHELL="vsh" ;;
        esac
    fi

    printf "%s" "$CURRENT_SHELL"
}

# ── Platform Detection ──
detect_platform() {
    OS="unknown"
    DISTRO="unknown"
    PKG_MGR="unknown"
    ARCH=$(uname -m 2>/dev/null || echo "unknown")

    case "$(uname -s 2>/dev/null)" in
        Linux*)
            OS="linux"
            if [ -f /etc/os-release ]; then
                DISTRO=$(. /etc/os-release && echo "$ID")
            elif [ -f /etc/redhat-release ]; then
                DISTRO="rhel"
            elif [ -f /etc/debian_version ]; then
                DISTRO="debian"
            fi
            # Detect package manager
            if command -v dnf >/dev/null 2>&1; then PKG_MGR="dnf"
            elif command -v apt-get >/dev/null 2>&1; then PKG_MGR="apt"
            elif command -v pacman >/dev/null 2>&1; then PKG_MGR="pacman"
            elif command -v apk >/dev/null 2>&1; then PKG_MGR="apk"
            elif command -v zypper >/dev/null 2>&1; then PKG_MGR="zypper"
            elif command -v rpm-ostree >/dev/null 2>&1; then PKG_MGR="rpm-ostree"
            elif command -v guix >/dev/null 2>&1; then PKG_MGR="guix"
            elif command -v nix >/dev/null 2>&1; then PKG_MGR="nix"
            fi
            ;;
        Darwin*)
            OS="macos"
            DISTRO="macos"
            if command -v brew >/dev/null 2>&1; then PKG_MGR="brew"
            elif command -v port >/dev/null 2>&1; then PKG_MGR="macports"
            fi
            ;;
        CYGWIN*|MINGW*|MSYS*)
            OS="windows"
            DISTRO="msys"
            if command -v winget >/dev/null 2>&1; then PKG_MGR="winget"
            elif command -v scoop >/dev/null 2>&1; then PKG_MGR="scoop"
            elif command -v choco >/dev/null 2>&1; then PKG_MGR="choco"
            fi
            ;;
        FreeBSD*)
            OS="freebsd"
            DISTRO="freebsd"
            PKG_MGR="pkg"
            ;;
    esac
}

# ── Install just ──
install_just() {
    if command -v just >/dev/null 2>&1; then
        ok "just already installed: $(just --version 2>/dev/null | head -1)"
        return 0
    fi

    info "Installing just (task runner)..."

    case "$PKG_MGR" in
        dnf)        sudo dnf install -y just ;;
        apt)        sudo apt-get install -y just 2>/dev/null || {
                        # just not in older apt repos — use installer
                        curl -fsSL https://just.systems/install.sh | bash -s -- --to /usr/local/bin
                    } ;;
        pacman)     sudo pacman -S --noconfirm just ;;
        apk)        sudo apk add just ;;
        brew)       brew install just ;;
        scoop)      scoop install just ;;
        winget)     winget install Casey.Just ;;
        rpm-ostree) sudo rpm-ostree install just ;;
        guix)       guix install just ;;
        nix)        nix-env -iA nixpkgs.just ;;
        *)
            info "Using just installer script..."
            curl -fsSL https://just.systems/install.sh | bash -s -- --to /usr/local/bin
            ;;
    esac

    if command -v just >/dev/null 2>&1; then
        ok "just installed: $(just --version 2>/dev/null | head -1)"
    else
        fail "Could not install just. Install manually: https://just.systems/"
        return 1
    fi
}

# ── Main ──
main() {
    printf "%s=== rescript Setup ===%s\n\n" "$BOLD" "$RESET"

    # Detect environment
    SHELL_NAME=$(detect_shell)
    detect_platform

    info "Shell:    $SHELL_NAME"
    info "Platform: $OS ($DISTRO)"
    info "Arch:     $ARCH"
    info "Packages: $PKG_MGR"
    printf "\n"

    # Warn about exotic shells
    case "$SHELL_NAME" in
        vsh)
            info "Valence Shell detected — experimental support"
            info "Falling back to POSIX sh for setup, vsh for post-setup"
            ;;
        nushell|elvish|murex|ion|hilbish|oil|xonsh|oh)
            info "$SHELL_NAME detected — using POSIX sh for setup"
            ;;
    esac

    # Step 1: Install just
    printf "%sStep 1: Install task runner%s\n" "$BOLD" "$RESET"
    install_just || { fail "Cannot proceed without just"; exit 1; }
    printf "\n"

    # Step 2: Check if we're in the repo directory
    if [ ! -f "Justfile" ] && [ ! -f "justfile" ]; then
        warn "Not in a repo directory (no Justfile found)"
        info "Clone first: git clone https://github.com/hyperpolymath/rescript.git"
        info "Then: cd rescript && ./setup.sh"
        exit 1
    fi

    # Step 3: Run just setup
    printf "%sStep 2: Project setup%s\n" "$BOLD" "$RESET"
    if just --list 2>/dev/null | grep -q "^setup "; then
        just setup
    elif just --list 2>/dev/null | grep -q "^setup-dev "; then
        just setup-dev
    else
        warn "No 'setup' recipe in Justfile — running 'just doctor' instead"
        just doctor 2>/dev/null || true
    fi
    printf "\n"

    # Step 4: Post-install security snapshot
    printf "%sStep 3: Security snapshot%s\n" "$BOLD" "$RESET"
    if command -v firewall-cmd >/dev/null 2>&1; then
        if firewall-cmd --state 2>/dev/null | grep -q running; then
            ok "Firewall: firewalld active"
        else
            warn "Firewall: firewalld installed but not running"
            info "  Enable: sudo systemctl enable --now firewalld"
        fi
    elif command -v ufw >/dev/null 2>&1; then
        if ufw status 2>/dev/null | grep -q "Status: active"; then
            ok "Firewall: ufw active"
        else
            warn "Firewall: ufw installed but not active"
            info "  Enable: sudo ufw enable"
        fi
    else
        warn "Firewall: none detected"
        case "$PKG_MGR" in
            dnf|rpm-ostree) info "  Install: sudo dnf install firewalld && sudo systemctl enable --now firewalld" ;;
            apt) info "  Install: sudo apt install ufw && sudo ufw enable" ;;
            *) info "  Install a firewall for your platform" ;;
        esac
    fi

    if command -v getenforce >/dev/null 2>&1; then
        SE_STATUS=$(getenforce 2>/dev/null || echo "unknown")
        case "$SE_STATUS" in
            Enforcing) ok "SELinux: Enforcing" ;;
            Permissive) warn "SELinux: Permissive (recommend Enforcing: sudo setenforce 1)" ;;
            *) warn "SELinux: $SE_STATUS" ;;
        esac
    fi

    # Write report
    REPORT_FILE="INSTALL-SECURITY-REPORT.adoc"
    {
        printf "// SPDX-License-Identifier: PMPL-1.0-or-later\n"
        printf "= Install Security Report\n"
        printf ":date: %s\n\n" "$(date -Iseconds 2>/dev/null || date)"
        printf "== Platform\n"
        printf "* OS: %s (%s)\n" "$OS" "$DISTRO"
        printf "* Arch: %s\n" "$ARCH"
        printf "* Package manager: %s\n" "$PKG_MGR"
        printf "* Shell: %s\n\n" "$SHELL_NAME"
        printf "== Security Status\n"
        printf "Run \`just doctor\` for full diagnostic.\n"
    } > "$REPORT_FILE"
    info "Security report: $REPORT_FILE"
    printf "\n"

    # Done
    printf "%s=== Setup Complete ===%s\n\n" "${BOLD}${GREEN}" "$RESET"
    printf "Next steps:\n"
    printf "  just doctor     — verify everything works\n"
    printf "  just tour       — guided tour of the project\n"
    printf "  just build      — build the project\n"
    printf "  just help-me    — get help if stuck\n"
}

main "$@"
