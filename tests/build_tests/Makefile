SHELL = /bin/bash

# The build system's tests (see README.md). `make test-build` at the
# repository root builds the toolchain and runtime and then runs `test`; CI
# runs the targets one at a time. RESCRIPT is the executable under test, by
# default the one in the platform package; pass an absolute path to override.

ROOT := $(abspath ../..)
# Relative, so that a make that spells paths the MSYS way still gives node
# a path it can resolve.
RESCRIPT ?= $(shell node -p "require('../../cli/common/bins.cjs').rescript_exe")

test: unit snapshots verbose interactive isolated integration

# OUnit tests of the build system's modules. The executable runs directly, not
# through `dune test`, so a stuck test still shows its progress.
unit:
	cd $(ROOT) && dune build tests/build_tests/unit/build_system_tests_main.exe
	$(ROOT)/_build/default/tests/build_tests/unit/build_system_tests_main.exe -runner sequential

# Output snapshots of configuration acceptance and command validation, and the
# check that suite.sh runs every integration script. On Windows CI they run
# under a watchdog that stops runaway processes.
snapshots:
	. isolated/process_watchdog.sh && start_process_watchdog && \
	  bash isolated/check_integration_test_coverage.sh && \
	  bash isolated/check_config_acceptance.sh "$(RESCRIPT)" && \
	  bash isolated/check_command_validation.sh "$(RESCRIPT)"

verbose:
	bash isolated/check_verbose_output.sh "$(RESCRIPT)"

# Needs the `script` PTY, which Windows lacks.
interactive:
	bash isolated/check_interactive_output.sh "$(RESCRIPT)"

isolated:
	sh isolated/run.sh "$(RESCRIPT)"

# The suite fails when another process runs its executable as it starts, so it
# runs a copy of the binaries: the other suites, run alongside it with -j or by
# another make, use the original. The copy's path is always the same, so a
# watcher left by an interrupted earlier run still counts as a leftover, and
# the next run replaces it.
INTEGRATION_BIN := $(ROOT)/tmp/integration-bin

integration:
	rm -rf "$(INTEGRATION_BIN)"
	mkdir -p "$(INTEGRATION_BIN)"
	cp -R "$$(dirname "$(RESCRIPT)")"/. "$(INTEGRATION_BIN)"
	./integration/suite.sh "$(INTEGRATION_BIN)/$$(basename "$(RESCRIPT)")"

.PHONY: test unit snapshots verbose interactive isolated integration
