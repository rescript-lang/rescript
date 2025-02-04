module X = {
    type t = int

    let minus_two = (t:t) => t - 2
    let minus_three = (t:t) => t -3
}

@module("meh") @taggedTemplate
external meh: (array<string>, array<string>) => X.t = "default"

// let x = meh`foo`.
//                  ^com

/*

dune exec -- rescript-editor-analysis debug-dump verbose test /Users/nojaf/Projects/rescript/tests/analysis_tests/tests/src/CompletionTaggedTemplate.res > /Users/nojaf/Projects/rescript/tests/analysis_tests/tests/src/expected/CompletionTaggedTemplate.res.txt

dune exec -- rescript-editor-analysis completion \
  /Users/nojaf/Projects/rescript/tests/analysis_tests/tests/src/CompletionTaggedTemplate.res \
  10 17 \
  /Users/nojaf/Projects/rescript/tests/analysis_tests/tests/src/CompletionTaggedTemplate.res \
  > /Users/nojaf/Projects/rescript/tests/analysis_tests/tests/src/expected/CompletionTaggedTemplate.res.txt

 */