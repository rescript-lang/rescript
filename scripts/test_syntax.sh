#!/bin/bash

# Note:
# 1. This was converted from zsh to bash because zsh is not available on Linux and Windows Github action runners.
# 2. macOS still has bash 3 and therefore no globstar ("**") support.
#    Therefore we need to use find + temp files for the file lists.

scriptDir=`dirname $0`
# macOS 12 does not have the realpath utility,
# so let's use this workaround instead.
DUNE_BIN_DIR=`cd "$scriptDir/../_build/install/default/bin"; pwd -P`

function exp {
  echo "$(dirname $1)/expected/$(basename $1).txt"
}

taskCount=0
function maybeWait {
  let taskCount+=1
  # spawn in batch of 20 processes
  [[ $((taskCount % 20)) = 0 ]] && wait
}

pushd tests

rm -rf temp
mkdir temp

# parsing
find syntax_tests/data/parsing/{errors,infiniteLoops,recovery} -name "*.res" -o -name "*.resi" >temp/files.txt
while read file; do
  $DUNE_BIN_DIR/res_parser -recover -print ml $file &> $(exp $file) & maybeWait
done <temp/files.txt
find syntax_tests/data/parsing/{grammar,other} -name "*.res" -o -name "*.resi" >temp/files.txt
while read file; do
  $DUNE_BIN_DIR/res_parser -print ml $file &> $(exp $file) & maybeWait
done <temp/files.txt

# printing
find syntax_tests/data/{printer,conversion} -name "*.res" -o -name "*.resi" -o -name "*.ml" -o -name "*.mli" >temp/files.txt
while read file; do
  $DUNE_BIN_DIR/res_parser $file &> $(exp $file) & maybeWait
done <temp/files.txt

# printing with ast conversion
find syntax_tests/data/ast-mapping -name "*.res" -o -name "*.resi" -o -name "*.ml" -o -name "*.mli" >temp/files.txt
while read file; do
  $DUNE_BIN_DIR/res_parser -test-ast-conversion -jsx-version 4 $file &> $(exp $file) & maybeWait
done <temp/files.txt

# printing with ppx
find syntax_tests/data/ppx/react -name "*.res" -o -name "*.resi" >temp/files.txt
while read file; do
  $DUNE_BIN_DIR/res_parser -jsx-version 4 $file &> $(exp $file) & maybeWait
done <temp/files.txt

wait

warningYellow='\033[0;33m'
successGreen='\033[0;32m'
reset='\033[0m'

git diff --ignore-cr-at-eol $(find syntax_tests -name expected) >temp/diff.txt
diff=$(cat temp/diff.txt)
if [[ $diff = "" ]]; then
  printf "${successGreen}✅ No unstaged tests difference.${reset}\n"
else
  printf "${warningYellow}⚠️ There are unstaged differences in syntax_tests/data/! Did you break a test?\n${diff}\n${reset}"
  rm -r temp/
  exit 1
fi

# roundtrip tests
if [[ $ROUNDTRIP_TEST = 1 ]]; then
  echo "Running roundtrip tests…"
  roundtripTestsResult="temp/result.txt"
  touch $roundtripTestsResult

  find syntax_tests/data/{idempotency,printer} -name "*.res" -o -name "*.resi" >temp/files.txt
  while read file; do {
    mkdir -p temp/$(dirname $file)
    sexpAst1=temp/$file.sexp
    sexpAst2=temp/$file.2.sexp
    rescript1=temp/$file.res
    rescript2=temp/$file.2.res

    case $file in
      *.res  ) resIntf=""         ;;
      *.resi ) resIntf=-interface ;;
    esac

    $DUNE_BIN_DIR/res_parser $resIntf -print sexp $file > $sexpAst1
    $DUNE_BIN_DIR/res_parser $resIntf -print res $file > $rescript1

    $DUNE_BIN_DIR/res_parser $resIntf -print sexp $rescript1 > $sexpAst2
    $DUNE_BIN_DIR/res_parser $resIntf -print res $rescript1 > $rescript2

    diff --unified $sexpAst1 $sexpAst2
    [[ "$?" = 1 ]] && echo 1 > $roundtripTestsResult
    diff --unified $rescript1 $rescript2
    [[ "$?" = 1 ]] && echo 1 > $roundtripTestsResult
  } & maybeWait
  done <temp/files.txt

  wait

  result=$(cat $roundtripTestsResult)

  if [[ $result = "1" ]]; then
    printf "${warningYellow}⚠️ Roundtrip tests failed.${reset}\n"
    exit 1
  else
    printf "${successGreen}✅ Roundtrip tests succeeded.${reset}\n"
  fi

fi

rm -r temp/
popd
