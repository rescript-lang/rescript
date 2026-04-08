K9!
# SPDX-License-Identifier: PMPL-1.0-or-later
# Example Kennel-level K9 component: Project metadata
# Security Level: Kennel (pure data, no execution)
# No signature required

{
  pedigree = {
    schema_version = "1.0.0",
    component_type = "project-metadata",
    security = {
      leash = 'Kennel,
      trust_level = "data-only",
      allow_network = false,
      allow_filesystem_write = false,
      allow_subprocess = false,
    },
    metadata = {
      name = "project-metadata",
      version = "1.0.0",
      description = "Pure data configuration for project metadata",
      author = "{{AUTHOR}} <{{AUTHOR_EMAIL}}>",
    },
  },

  # Project configuration
  project = {
    name = "my-project",
    version = "0.1.0",
    description = "A project following Rhodium Standard Repositories",

    repository = {
      url = "https://github.com/hyperpolymath/my-project",
      type = "git",
    },

    author = {
      name = "{{AUTHOR}}",
      email = "{{AUTHOR_EMAIL}}",
      organization = "{{AUTHOR_ORG}}",
    },

    license = "PMPL-1.0-or-later",

    keywords = [
      "rhodium-standard",
      "rsr",
      "hyperpolymath",
    ],
  },

  # Export as JSON for other tools
  export = {
    format = "json",
    destination = "project-metadata.json",
  },
}
