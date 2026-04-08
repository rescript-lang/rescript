K9!
# SPDX-License-Identifier: PMPL-1.0-or-later
# K9 Kennel-level template: Pure data configuration
# Security Level: Kennel (data-only, no execution)
# No signature required - safe for any use

{
  pedigree = {
    schema_version = "1.0.0",
    component_type = "TODO: describe component type (e.g., 'build-config', 'metadata')",
    security = {
      leash = 'Kennel,
      trust_level = "data-only",
      allow_network = false,
      allow_filesystem_write = false,
      allow_subprocess = false,
    },
    metadata = {
      name = "TODO: component-name",
      version = "1.0.0",
      description = "TODO: Brief description of what this component contains",
      author = "{{AUTHOR}} <{{AUTHOR_EMAIL}}>",
    },
  },

  # Your configuration data here
  config = {
    # Example: Pure data values
    setting_1 = "value",
    setting_2 = 42,
    setting_3 = true,

    nested = {
      key = "value",
    },

    list = [
      "item1",
      "item2",
    ],
  },

  # Optional: Export format specification
  export = {
    format = "json",  # or "yaml", "toml"
    destination = "output.json",
  },
}

# Usage:
# 1. Fill in TODO items above
# 2. Add your configuration data to config = { ... }
# 3. Validate: nickel typecheck your-file.k9.ncl
# 4. Export: nickel export your-file.k9.ncl > output.json
