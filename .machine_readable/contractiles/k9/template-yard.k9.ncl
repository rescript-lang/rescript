K9!
# SPDX-License-Identifier: PMPL-1.0-or-later
# K9 Yard-level template: Configuration with validation
# Security Level: Yard (Nickel evaluation with contracts)
# Signature recommended but not required

{
  pedigree = {
    schema_version = "1.0.0",
    component_type = "TODO: describe component type (e.g., 'validated-config', 'schema')",
    security = {
      leash = 'Yard,
      trust_level = "validated-config",
      allow_network = false,
      allow_filesystem_write = false,
      allow_subprocess = false,
    },
    metadata = {
      name = "TODO: component-name",
      version = "1.0.0",
      description = "TODO: Brief description with validation details",
      author = "{{AUTHOR}} <{{AUTHOR_EMAIL}}>",
    },
  },

  # Configuration with Nickel contracts for validation
  config = {
    # Example: String that cannot be empty
    name
      | String
      | std.string.NonEmpty
      = "TODO: default value",

    # Example: Number with range constraint
    port
      | Number
      | std.contract.from_predicate (fun p => p > 0 && p < 65536)
      = 8080,

    # Example: Boolean flag
    enabled | Bool = true,

    # Example: Enum (one of several values)
    environment
      | [| 'Development, 'Staging, 'Production |]
      = 'Development,

    # Example: List with non-empty constraint
    items
      | Array String
      | std.array.NonEmpty
      = ["item1", "item2"],

    # Example: Nested object with contracts
    database = {
      host | String | std.string.NonEmpty = "localhost",
      port | Number | std.contract.from_predicate (fun p => p > 0 && p < 65536) = 5432,
      name | String | std.string.NonEmpty = "mydb",
    },
  },

  # Validation rules (additional cross-field checks)
  validation = {
    # Example: Check that at least one item exists
    check_items = std.array.length config.items > 0,

    # Example: Check that production has secure settings
    check_production =
      if config.environment == 'Production then
        config.enabled == true
      else
        true,

    # Add your custom validation rules here
  },
}

# Usage:
# 1. Fill in TODO items above
# 2. Define your config with appropriate contracts
# 3. Add validation rules in validation = { ... }
# 4. Validate: nickel typecheck your-file.k9.ncl
# 5. Evaluate: nickel eval your-file.k9.ncl
# 6. If validation passes, use in your application
