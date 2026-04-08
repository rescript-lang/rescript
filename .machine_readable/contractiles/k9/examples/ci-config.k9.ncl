K9!
# SPDX-License-Identifier: PMPL-1.0-or-later
# Example Yard-level K9 component: CI/CD configuration with validation
# Security Level: Yard (Nickel evaluation, contract validation)
# Signature recommended but not required

{
  pedigree = {
    schema_version = "1.0.0",
    component_type = "ci-configuration",
    security = {
      leash = 'Yard,
      trust_level = "validated-config",
      allow_network = false,
      allow_filesystem_write = false,
      allow_subprocess = false,
    },
    metadata = {
      name = "ci-config",
      version = "1.0.0",
      description = "CI/CD configuration with runtime validation",
      author = "{{AUTHOR}} <{{AUTHOR_EMAIL}}>",
    },
  },

  # CI/CD configuration with Nickel contracts
  ci = {
    # Platform must be a known CI provider
    platform
      | [| 'GitHubActions, 'GitLabCI, 'CircleCI, 'TravisCI |]
      = 'GitHubActions,

    # Build matrix with validation
    matrix = {
      # Operating systems to test on
      os
        | Array String
        | std.array.NonEmpty
        = ["ubuntu-latest", "macos-latest"],

      # Language versions to test
      versions
        | Array String
        | std.array.NonEmpty
        = ["stable", "beta"],
    },

    # Workflow steps with validation
    steps = [
      {
        name = "Checkout",
        action = "actions/checkout@v4",
        # Version must be SHA-pinned for security
        sha | String | std.string.NonEmpty = "b4ffde65f46336ab88eb53be808477a3936bae11",
      },
      {
        name = "Build",
        run = "just build",
      },
      {
        name = "Test",
        run = "just test",
      },
      {
        name = "Lint",
        run = "just lint",
      },
    ],

    # Deployment configuration
    deploy = {
      enabled | Bool = false,

      # Only deploy from main branch
      branch
        | String
        | std.contract.from_predicate (fun b => b == "main" || b == "master")
        = "main",

      # Deployment requires manual approval
      requires_approval | Bool = true,
    },

    # Security scanning
    security = {
      enabled | Bool = true,

      scanners = [
        {
          name = "CodeQL",
          languages = ["rust", "javascript"],
        },
        {
          name = "OSSF Scorecard",
          enabled = true,
        },
        {
          name = "TruffleHog",
          scan_for = "secrets",
        },
      ],
    },

    # Notification settings
    notifications = {
      on_success = "never",
      on_failure = "always",
      channels = ["email"],
    },
  },

  # Validation rules (enforced by Nickel)
  validation = {
    # At least one OS must be specified
    check_os = std.array.length ci.matrix.os > 0,

    # At least one version must be tested
    check_versions = std.array.length ci.matrix.versions > 0,

    # Must have at least build and test steps
    check_steps = std.array.length ci.steps >= 2,

    # Security scanning must be enabled
    check_security = ci.security.enabled == true,
  },
}
