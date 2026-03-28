# SPDX-License-Identifier: PMPL-1.0-or-later
# Justfile for rescript

# Default recipe — list available commands
default:
    @just --list

# Self-diagnostic — checks dependencies, permissions, paths
doctor:
    @echo "Running diagnostics for rescript..."
    @echo "Checking required tools..."
    @command -v just >/dev/null 2>&1 && echo "  [OK] just" || echo "  [FAIL] just not found"
    @command -v git >/dev/null 2>&1 && echo "  [OK] git" || echo "  [FAIL] git not found"
    @echo "Checking for hardcoded paths..."
    @grep -rn '/var/mnt/eclipse' --include='*.rs' --include='*.ex' --include='*.res' --include='*.gleam' --include='*.sh' --include='*.toml' . 2>/dev/null | grep -v 'Justfile' | head -5 || echo "  [OK] No hardcoded paths in source"
    @echo "Diagnostics complete."

# Guided tour of key features
tour:
    @echo "=== rescript Tour ==="
    @echo ""
    @echo "1. Project structure:"
    @ls -la
    @echo ""
    @echo "2. Available commands: just --list"
    @echo ""
    @echo "3. Read README.adoc or README.md for full overview"
    @echo "4. Read EXPLAINME.adoc for architecture decisions"
    @echo "5. Run 'just doctor' to check your setup"
    @echo ""
    @echo "Tour complete! Try 'just --list' to see all available commands."

# Open feedback channel with diagnostic context
help-me:
    @echo "=== rescript Help ==="
    @echo "Platform: $(uname -s) $(uname -m)"
    @echo "Shell: $SHELL"
    @echo ""
    @echo "To report an issue:"
    @echo "  https://github.com/hyperpolymath/rescript/issues/new"
    @echo ""
    @echo "Include the output of 'just doctor' in your report."

# Run panic-attacker pre-commit scan
assail:
    @command -v panic-attack >/dev/null 2>&1 && panic-attack assail . || echo "WARN: panic-attack not found — install from https://github.com/hyperpolymath/panic-attacker"

# LLM context dump
llm-context:
    @echo "Project: rescript"
    @echo "License: PMPL-1.0-or-later"
    @test -f README.adoc && head -30 README.adoc || test -f README.md && head -30 README.md || echo "No README found"
