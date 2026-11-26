#!/bin/bash
cd $(dirname $0)
source "../utils.sh"
cd ../../testrepo

bold "Test: It should keep each file's line endings"

dir=format-line-endings
mkdir -p $dir
trap 'rm -rf "$dir"' EXIT

# Formatted source with lines inside a comment and a template literal, which
# used to get CR CR LF line endings when formatted with CRLF ones
formatted='/* a\n multi-line comment */\nlet s = `line 1\nline 2`\n'
printf "$formatted" > $dir/Lf.res
printf "$formatted" | sed 's/$/\r/' > $dir/Crlf.res
printf 'let  x  =  1\n' > $dir/UnformattedLf.res
printf 'let  x  =  1\r\n' > $dir/UnformattedCrlf.res

if ! check_output=$(rewatch format --check $dir/Lf.res $dir/Crlf.res 2>&1); then
  error "Formatted LF and CRLF files failed the format check"
  echo "$check_output"
  exit 1
fi

cp $dir/Lf.res $dir/Lf.expected
cp $dir/Crlf.res $dir/Crlf.expected
printf 'let x = 1\n' > $dir/UnformattedLf.expected
printf 'let x = 1\r\n' > $dir/UnformattedCrlf.expected

if ! format_output=$(rewatch format $dir/*.res 2>&1); then
  error "Error formatting LF and CRLF files"
  echo "$format_output"
  exit 1
fi

for file in Lf Crlf UnformattedLf UnformattedCrlf; do
  if ! cmp -s $dir/$file.res $dir/$file.expected; then
    error "Formatting $file.res changed its line endings"
    od -c $dir/$file.res | head -10
    exit 1
  fi
done

success "LF and CRLF files keep their line endings"
