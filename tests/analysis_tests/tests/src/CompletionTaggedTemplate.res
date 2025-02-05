@module("meh") @taggedTemplate
external meh: (array<string>, array<string>) => string = "default"

// let x = meh`foo`.
//                  ^com

// let y = meh`bar`.len
//                     ^com

/*

dune exec -- rescript-editor-analysis debug-dump verbose test /Users/nojaf/Projects/rescript/tests/analysis_tests/tests/src/CompletionTaggedTemplate.res > /Users/nojaf/Projects/rescript/tests/analysis_tests/tests/src/expected/CompletionTaggedTemplate.res.txt

dune exec -- rescript-editor-analysis completion \
  /Users/nojaf/Projects/rescript/tests/analysis_tests/tests/src/CompletionTaggedTemplate.res \
  10 17 \
  /Users/nojaf/Projects/rescript/tests/analysis_tests/tests/src/CompletionTaggedTemplate.res \
  > /Users/nojaf/Projects/rescript/tests/analysis_tests/tests/src/expected/CompletionTaggedTemplate.res.txt

 */