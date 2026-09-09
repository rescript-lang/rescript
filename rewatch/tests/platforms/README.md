# Platform module acceptance tests

These tests specify the intended Rewatch behavior for
[platform-specific modules](https://github.com/rescript-lang/rescript/issues/1436).
The source project lives in [`../fixtures/react-native-platforms`](../fixtures/react-native-platforms).

The tests require successful builds and call the Rewatch executable under test,
never the compiler probe or a custom build graph.

Run directly from the repository root, after `make lib`:

```sh
REWATCH_EXECUTABLE="$PWD/rewatch/target/debug/rescript" \
  bash rewatch/tests/platforms/01-react-native-platforms.sh
```

They also run unconditionally in the existing integration suite:

```sh
make test-rewatch
```

The ordinary acceptance checks require only Node and the locally built Rewatch,
compiler, and runtime. Each case copies the tiny source fixture into a temporary
project, so edits and cleanup cannot affect the shared `testrepo` workspace.

Covered contracts:

- Both `Button.android.res` and `Button.ios.res` implement `Button.resi`.
- Generated filenames follow `Button.<platform>.js` and imports allow Metro selection.
- Public constants and private abstract-type representations do not leak across platforms,
  with general cross-module optimization both enabled and disabled.
- No-op builds preserve outputs; implementation edits produce correct behavior on both targets.
- Each implementation is checked against the shared interface after edits, and recovery works.
- Editing the shared interface invalidates the checks.
- Clean removes outputs for both targets, and rebuilding restores them.

The tests require one shared `App.js` backed by conservative compiler metadata;
`App.android.js` and `App.ios.js` are rejected. They cover configuration errors,
incremental builds, cleanup, target addition and removal, namespaces, dependency packages,
CommonJS and ES modules, in-source and out-of-source output, and source maps.
Watcher dirty-state behavior has a focused Rust unit test. Generic and `.native`
fallbacks remain intentionally unsupported.

The heavier real React Native/Metro bundle check is separate; see the fixture's
README. Its build also calls Rewatch directly. Source-map assertions ensure the
bundler includes the matching platform variants and excludes the other ones.
