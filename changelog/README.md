# Changelog fragments

Add a unique `<description>.<category>.md` file for each change that needs release notes. For example, `fix-watcher-recovery.fix.md`:

```markdown
- Recover blocked dependents after a failed build. https://github.com/rescript-lang/rescript/pull/8667
```

The PR number is only known once the PR exists, so open it as a draft with the placeholder link `https://github.com/rescript-lang/rescript/pull/XXXX`, replace the placeholder with the real link, amend and push again, then mark the PR ready for review. The changelog check fails while the placeholder remains. Each file may contain multiple bullets in the same category. Indent wrapped lines by two spaces; each bullet ends with one or more PR links.

| Category | Section |
| --- | --- |
| `breaking` | Breaking Change |
| `feature` | New Feature |
| `fix` | Bug fix |
| `docs` | Documentation |
| `polish` | Polish |
| `internal` | Internal |

`yarn changelog:check` validates all fragments. CI requires a newly added fragment relative to the PR merge base, and rejects edits to `CHANGELOG.md` unless the PR has the `changelog:release` label. Maintainers may apply `changelog:skip` when notes are unnecessary, or `changelog:release` for release preparation. These labels, like Dependabot PRs, are exempt from the requirement, but never from fragment validation. Reviewers decide whether the chosen category is appropriate.

`yarn changelog:preview` prints pending fragments grouped by category, newest PR first, without changing files. It does not include entries already in `CHANGELOG.md`.

During release preparation, run `yarn changelog:release`. It verifies that the first version heading matches `package.json` and is unreleased, validates every fragment before writing, adds fragments above the existing entries of each section, newest PR first, removes `(Unreleased)`, and deletes consumed files. Review and commit all changes in the release PR. Historical entries and existing unreleased notes are preserved. Running it again fails because the version has already been finalized. Releases with no fragments can still finalize existing notes. The publish workflow refuses to release while fragments remain in `changelog/`.

After publishing, run `yarn changelog:open` once `package.json` has the next version. It adds an empty unreleased section with every category heading.

Only release preparation and opening the next unreleased section should edit `CHANGELOG.md`, in PRs labelled `changelog:release`. Keep fragments on the branch containing the associated change; backports should include their fragments if they need notes in the maintenance release.
