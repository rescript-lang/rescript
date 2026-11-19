- Enforce `sources.public` in the compiler: unlisted compilation units are
  accessible only within their defining ReScript project, including during
  workspace and source-package builds. Nested source entries inherit `public`
  unless explicitly overridden. Consumers that relied on previously ignored
  restrictions must switch to the package's public API.
