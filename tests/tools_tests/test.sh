for file in src/*.{res,resi}; do
  output="$(dirname $file)/expected/$(basename $file).json"
  ../../_build/install/default/bin/rescript-tools doc $file > $output
  # # CI. We use LF, and the CI OCaml fork prints CRLF. Convert.
  if [ "$RUNNER_OS" == "Windows" ]; then
    perl -pi -e 's/\r\n/\n/g' -- $output
  fi
done

for file in ppx/*.res; do
  output="src/expected/$(basename $file).jsout"
  ../../cli/bsc.js -ppx "../../_build/install/default/bin/rescript-tools ppx" $file > $output
  if [ "$RUNNER_OS" == "Windows" ]; then
    perl -pi -e 's/\r\n/\n/g' -- $output
  fi
done

# Test format-codeblocks command
for file in src/docstrings-format/*.{res,resi,md}; do
  output="src/expected/$(basename $file).expected"
  ../../_build/install/default/bin/rescript-tools format-codeblocks "$file" --stdout > $output
  if [ "$RUNNER_OS" == "Windows" ]; then
    perl -pi -e 's/\r\n/\n/g' -- $output
  fi
done

# Test extract-codeblocks command
for file in src/docstrings-format/*.{res,resi,md}; do
  output="src/expected/$(basename $file).extracted.json.expected"
  ../../_build/install/default/bin/rescript-tools extract-codeblocks "$file" --transform-assert-equal > $output
  if [ "$RUNNER_OS" == "Windows" ]; then
    perl -pi -e 's/\r\n/\n/g' -- $output
  fi
done

# Test lint command
for file in src/lint/*.{res,resi}; do
  output="src/expected/$(basename $file).lint.expected"
  ../../_build/install/default/bin/rescript-tools lint "$file" > "$output" || true
  if [ "$RUNNER_OS" == "Windows" ]; then
    perl -pi -e 's/\r\n/\n/g' -- "$output"
  fi

  json_output="src/expected/$(basename $file).lint.json.expected"
  ../../_build/install/default/bin/rescript-tools lint "$file" --json > "$json_output" || true
  if [ "$RUNNER_OS" == "Windows" ]; then
    perl -pi -e 's/\r\n/\n/g' -- "$json_output"
  fi
done

generated_file="src/generated/GeneratedConsumer.res"
generated_output="src/expected/$(basename $generated_file).lint.expected"
../../_build/install/default/bin/rescript-tools lint "$generated_file" > "$generated_output" || true
if [ "$RUNNER_OS" == "Windows" ]; then
  perl -pi -e 's/\r\n/\n/g' -- "$generated_output"
fi

generated_json_output="src/expected/$(basename $generated_file).lint.json.expected"
../../_build/install/default/bin/rescript-tools lint "$generated_file" --json > "$generated_json_output" || true
if [ "$RUNNER_OS" == "Windows" ]; then
  perl -pi -e 's/\r\n/\n/g' -- "$generated_json_output"
fi

unbuilt_file="unbuilt/ForbiddenExplicitNoCmt.res"
unbuilt_output="src/expected/$(basename $unbuilt_file).lint.expected"
../../_build/install/default/bin/rescript-tools lint "$unbuilt_file" > "$unbuilt_output" || true
if [ "$RUNNER_OS" == "Windows" ]; then
  perl -pi -e 's/\r\n/\n/g' -- "$unbuilt_output"
fi

unbuilt_json_output="src/expected/$(basename $unbuilt_file).lint.json.expected"
../../_build/install/default/bin/rescript-tools lint "$unbuilt_file" --json > "$unbuilt_json_output" || true
if [ "$RUNNER_OS" == "Windows" ]; then
  perl -pi -e 's/\r\n/\n/g' -- "$unbuilt_json_output"
fi

../../_build/install/default/bin/rescript-tools lint src/lint > src/expected/lint-root.expected || true
if [ "$RUNNER_OS" == "Windows" ]; then
  perl -pi -e 's/\r\n/\n/g' -- src/expected/lint-root.expected
fi

../../_build/install/default/bin/rescript-tools lint src/lint --json > src/expected/lint-root.json.expected || true
if [ "$RUNNER_OS" == "Windows" ]; then
  perl -pi -e 's/\r\n/\n/g' -- src/expected/lint-root.json.expected
fi

# Test active-rules command
../../_build/install/default/bin/rescript-tools active-rules src/lint > src/expected/active-rules.expected || exit 1
if [ "$RUNNER_OS" == "Windows" ]; then
  perl -pi -e 's/\r\n/\n/g' -- src/expected/active-rules.expected
fi

../../_build/install/default/bin/rescript-tools active-rules src/lint --json > src/expected/active-rules.json.expected || exit 1
if [ "$RUNNER_OS" == "Windows" ]; then
  perl -pi -e 's/\r\n/\n/g' -- src/expected/active-rules.json.expected
fi

# Test show command
../../_build/install/default/bin/rescript-tools show ShowFixture --kind module > src/expected/show-ShowFixture.expected || exit 1
if [ "$RUNNER_OS" == "Windows" ]; then
  perl -pi -e 's/\r\n/\n/g' -- src/expected/show-ShowFixture.expected
fi

../../_build/install/default/bin/rescript-tools show ShowFixture.Nested.makeGreeting --kind value > src/expected/show-ShowFixture.Nested.makeGreeting.expected || exit 1
if [ "$RUNNER_OS" == "Windows" ]; then
  perl -pi -e 's/\r\n/\n/g' -- src/expected/show-ShowFixture.Nested.makeGreeting.expected
fi

../../_build/install/default/bin/rescript-tools show ShowFixture.Nested.makeGreeting --kind value --comments omit > src/expected/show-ShowFixture.Nested.makeGreeting.no-comments.expected || exit 1
if [ "$RUNNER_OS" == "Windows" ]; then
  perl -pi -e 's/\r\n/\n/g' -- src/expected/show-ShowFixture.Nested.makeGreeting.no-comments.expected
fi

../../_build/install/default/bin/rescript-tools show ShowFixture.item --kind type > src/expected/show-ShowFixture.item.expected || exit 1
if [ "$RUNNER_OS" == "Windows" ]; then
  perl -pi -e 's/\r\n/\n/g' -- src/expected/show-ShowFixture.item.expected
fi

../../_build/install/default/bin/rescript-tools show String.localeCompare > src/expected/show-String.localeCompare.expected || exit 1
if [ "$RUNNER_OS" == "Windows" ]; then
  perl -pi -e 's/\r\n/\n/g' -- src/expected/show-String.localeCompare.expected
fi

../../_build/install/default/bin/rescript-tools show String --kind module > /dev/null || exit 1

# Test find-references command
../../_build/install/default/bin/rescript-tools find-references FindReferencesFixture.makeGreeting --kind value > src/expected/find-references-FindReferencesFixture.makeGreeting.expected || exit 1
if [ "$RUNNER_OS" == "Windows" ]; then
  perl -pi -e 's/\r\n/\n/g' -- src/expected/find-references-FindReferencesFixture.makeGreeting.expected
fi

../../_build/install/default/bin/rescript-tools find-references --file src/find_references/FindReferencesUse.res --line 4 --col 35 > src/expected/find-references-location.expected || exit 1
if [ "$RUNNER_OS" == "Windows" ]; then
  perl -pi -e 's/\r\n/\n/g' -- src/expected/find-references-location.expected
fi

../../_build/install/default/bin/rescript-tools find-references FindReferencesFixture --kind module > /dev/null || exit 1

# Test rewrite command
rm -rf .tmp-rewrite-tests
mkdir -p .tmp-rewrite-tests

for file in src/rewrite/*.{res,resi}; do
  tmp_file=".tmp-rewrite-tests/$(basename $file)"
  cp "$file" "$tmp_file"

  output="src/expected/$(basename $file).rewrite.expected"
  ../../_build/install/default/bin/rescript-tools rewrite "$tmp_file" > "$output"
  if [ "$RUNNER_OS" == "Windows" ]; then
    perl -pi -e 's/\r\n/\n/g' -- "$output"
  fi

  rewritten_output="src/expected/$(basename $file).rewrite.source.expected"
  cat "$tmp_file" > "$rewritten_output"
  if [ "$RUNNER_OS" == "Windows" ]; then
    perl -pi -e 's/\r\n/\n/g' -- "$rewritten_output"
  fi

  cp "$file" "$tmp_file"
  json_output="src/expected/$(basename $file).rewrite.json.expected"
  ../../_build/install/default/bin/rescript-tools rewrite "$tmp_file" --json > "$json_output"
  if [ "$RUNNER_OS" == "Windows" ]; then
    perl -pi -e 's/\r\n/\n/g' -- "$json_output"
  fi

  diff_output="src/expected/$(basename $file).rewrite.diff.expected"
  ../../_build/install/default/bin/rescript-tools rewrite "$file" --diff > "$diff_output"
  if [ "$RUNNER_OS" == "Windows" ]; then
    perl -pi -e 's/\r\n/\n/g' -- "$diff_output"
  fi

  diff_json_output="src/expected/$(basename $file).rewrite.diff.json.expected"
  ../../_build/install/default/bin/rescript-tools rewrite "$file" --diff --json > "$diff_json_output"
  if [ "$RUNNER_OS" == "Windows" ]; then
    perl -pi -e 's/\r\n/\n/g' -- "$diff_json_output"
  fi
done

rm -rf .tmp-rewrite-tests/root
mkdir -p .tmp-rewrite-tests/root
cp src/rewrite/*.{res,resi} .tmp-rewrite-tests/root/
../../_build/install/default/bin/rescript-tools rewrite .tmp-rewrite-tests/root > src/expected/rewrite-root.expected
if [ "$RUNNER_OS" == "Windows" ]; then
  perl -pi -e 's/\r\n/\n/g' -- src/expected/rewrite-root.expected
fi

rm -rf .tmp-rewrite-tests/root
mkdir -p .tmp-rewrite-tests/root
cp src/rewrite/*.{res,resi} .tmp-rewrite-tests/root/
../../_build/install/default/bin/rescript-tools rewrite .tmp-rewrite-tests/root --json > src/expected/rewrite-root.json.expected
if [ "$RUNNER_OS" == "Windows" ]; then
  perl -pi -e 's/\r\n/\n/g' -- src/expected/rewrite-root.json.expected
fi

../../_build/install/default/bin/rescript-tools rewrite src/rewrite --diff > src/expected/rewrite-root.diff.expected
if [ "$RUNNER_OS" == "Windows" ]; then
  perl -pi -e 's/\r\n/\n/g' -- src/expected/rewrite-root.diff.expected
fi

../../_build/install/default/bin/rescript-tools rewrite src/rewrite --diff --json > src/expected/rewrite-root.diff.json.expected
if [ "$RUNNER_OS" == "Windows" ]; then
  perl -pi -e 's/\r\n/\n/g' -- src/expected/rewrite-root.diff.json.expected
fi

rm -rf .tmp-rewrite-tests

# Test migrate command
for file in src/migrate/*.{res,resi}; do
  output="src/expected/$(basename $file).expected"
  ../../_build/install/default/bin/rescript-tools migrate "$file" --stdout > $output
  if [ "$RUNNER_OS" == "Windows" ]; then
    perl -pi -e 's/\r\n/\n/g' -- $output
  fi
done

# Move migrated files to expected directory so they can be compiled in the project
for file in src/migrate/StdlibMigration_*.res; do
  expected_file="src/expected/$(basename $file).expected"
  output="src/migrate/migrated/Migrated_$(basename $file)"
  echo "// This file is autogenerated so it can be type checked.
// It's the migrated version of $file." > "$output" && cat "$expected_file" >> "$output"
  ../../cli/rescript.js format "$output"
done

warningYellow='\033[0;33m'
successGreen='\033[0;32m'
reset='\033[0m'

diff=$(git ls-files --modified src/expected)
if [[ $diff = "" ]]; then
  printf "${successGreen}✅ No unstaged tests difference.${reset}\n"
else
  printf "${warningYellow}⚠️ There are unstaged differences in tests/! Did you break a test?\n${diff}\n${reset}"
  git --no-pager diff src/expected
  exit 1
fi
