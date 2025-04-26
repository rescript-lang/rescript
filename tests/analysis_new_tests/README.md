# Analysis tests

These tests test the analysis functionality for the editor tooling.

## Test Structure

Test files are located in the `test_files` directory and follow a specific format:

1. Each test file can contain multiple test blocks, separated by `// == TEST:` markers
2. Each test block consists of:
   - A description line immediately following the marker
   - The actual test code
   - Any `^xxx` marker indicating what test to run, and where. Several `^xxx` tests can run per test block

Example:

```rescript
// == TEST: Record field completion in nested record
let x = TestTypeDefs.nestedTestRecord.
//                                    ^com
```

## Directory Structure

### `support_files`

The `support_files` directory contains reusable code that can be shared across multiple tests. These files:

- Are compiled as part of the normal ReScript build process
- Should be valid ReScript code that compiles without errors
- Can contain type definitions, modules, and other code needed by multiple tests

### `test_files`

The `test_files` directory contains all the test files. These files:

- Are compiled incrementally for testing purposes only
- Are not part of the main ReScript build
- Can (should!) contain invalid/incomplete code since they're only used as tests
- Each file can contain multiple test blocks as described above

## Test Execution

The test runner:

1. Extracts all test blocks from all test files
2. Compiles each test block and sets each block up for incremental type checking
3. Runs the editor analysis tool on each test block
4. Compares the output against snapshots

This setup allows testing all editor tooling analysis features, in a flexible manner.

## Configuration Files

The test runner uses a special `rescript.test.json` configuration file when running the analysis tests. This is separate from the normal `rescript.json` configuration and is only used when calling the analysis binary for testing purposes. This separation ensures that test configurations don't interfere with the main project configuration, since the test files will not actually compile.
