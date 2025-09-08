Special tests for duplicated symlink warnings


```
.
├── a
│   ├── node_modules
│   │   ├── c -> ../../c
│   │   └── z
│   │       ├── lib
│   │       │   ├── bs
│   │       │   └── ocaml
│   │       └── src
│   └── src
├── b
│   ├── node_modules
│   │   └── c -> ../../c
│   └── src
├── c
│   └── src
├── node_modules
│   ├── a -> ../a
│   ├── b -> ../b
│   └── z
│       └── src
└── out.expected
└── src

```

`c` is symlinked everywhere, while `z` is actually conflicting package. 

`out.expected` has exactly 1 warning for `z` conflict

Run `node ./tests/build_tests/duplicated_symlinked_packages/input.js` to check the tests against previous snapshots.
Run `node ./tests/build_tests/duplicated_symlinked_packages/input.js update` to update the snapshots (assuming you've made some changes to duplicated)
