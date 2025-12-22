# Actions tests

Tests for emitted possible actions.

- Add ReScript files that should be producing actions to `tests/build_tests/actions/fixtures`. Make sure you prefix all filenames with `Actions_`, e.g `Actions_UnusedOpen.res`
- Test file output are emitted as actual ReScript files suffixed with `_applied`, into `tests/build_tests/actions/expected`. So `Actions_UnusedOpen_applied.res`
- Run `node tests/build_tests/actions/input.js` to run the tests
- Run `node tests/build_tests/actions/input.js update` to persist any updates to the test output, or write initial output for new tests
