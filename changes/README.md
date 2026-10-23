# Changelog fragments

Add a unique `<description>.<category>.md` file for each change that needs release notes. For example, `fix-watcher-recovery.fix.md`:

```markdown
- Recover blocked dependents after a failed build.
```

Each file may contain multiple bullets in the same category. Indent wrapped lines by two spaces.

A bullet needs no PR link: `yarn changelog:release` appends the link of the pull request that added the fragment, taken from the subject of its squash commit (`... (#8667)`). Pull requests are therefore squash-merged, keeping the `(#N)` GitHub adds to the title. A bullet may still end with explicit PR links, for example for a change spanning several pull requests, and those are kept as written. After each push to `master`, the changelog check fails if a fragment without a link cannot be traced to its pull request; add the link to that fragment.

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

During release preparation, run `yarn changelog:release`. It verifies that the first version heading matches `package.json` and is unreleased, validates every fragment before writing, adds fragments above the existing entries of each section, newest PR first, removes `(Unreleased)`, and deletes consumed files. It also bumps the magic numbers in `compiler/ext/config.ml` whose sources changed since the previous release tag, unless they already differ from that tag, so running it again does not bump them twice. CI rejects pull requests without the `changelog:release` label that bump one. Review and commit all changes in the release PR. Historical entries and existing unreleased notes are preserved. Releases with no fragments can still finalize existing notes. The publish workflow refuses to release while fragments remain in `changes/`. If fragments are merged after the release PR but before publishing, run `yarn changelog:release` again: as long as Publish has not created the version's `v<version>` tag, it adds them to the finalized version. Once the tag exists, it refuses, and the fragments wait for the next version after `yarn changelog:open`. With no fragments left, running it again fails. Don't run it while a Publish run is in progress, because the tag only appears once that run gets to tagging, and the notes could then describe changes the run is not publishing.

After publishing, run `yarn changelog:open` once `package.json` has the next version. It adds an empty unreleased section with every category heading.

Only release preparation and opening the next unreleased section should edit `CHANGELOG.md`, in PRs labelled `changelog:release`. Keep fragments on the branch containing the associated change; backports should include their fragments if they need notes in the maintenance release.
