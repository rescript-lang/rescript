SHELL = /bin/bash

# The build system's tests (see README.md). `make test-build` at the
# repository root builds the toolchain and runtime and then runs `test`; CI
# runs the targets one at a time. RESCRIPT is the executable under test, by
# default the one in the platform package; pass an absolute path to override.

ROOT := $(abspath ../..)
RESCRIPT ?= $(shell node -p "require('$(ROOT)/cli/common/bins.cjs').rescript_exe")

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

integration:
	./integration/suite.sh "$(RESCRIPT)"

.PHONY: test unit snapshots verbose interactive isolated integration
