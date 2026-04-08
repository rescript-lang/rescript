K9!
# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
# K9 Kennel-level guard: burble project metadata and media/comms invariants
# Security Level: Kennel (data-only, no execution)
# No signature required

{
  pedigree = {
    schema_version = "1.0.0",
    component_type = "comms-platform-metadata",
    security = {
      leash = 'Kennel,
      trust_level = "data-only",
      allow_network = false,
      allow_filesystem_write = false,
      allow_subprocess = false,
    },
    metadata = {
      name = "burble-metadata",
      version = "0.1.0",
      description = "Burble: self-hostable WebRTC communications platform — project invariants",
      author = "Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>",
    },
  },

  # Project configuration
  config = {
    # Architecture invariants
    architecture = {
      # Control plane language
      control_plane = "elixir",
      # Media layer language
      media_layer = "zig",
      # Signalling language
      signalling = "elixir",
    },

    # Security invariants (CRITICAL for comms)
    security = {
      # No plaintext signalling
      encryption_required = true,
      # Allowed crypto algorithms
      allowed_algorithms = ["AES-256-GCM", "X25519", "Ed25519", "DTLS-SRTP"],
      # Banned weak algorithms
      banned_algorithms = ["MD5", "SHA1", "RC4", "DES"],
      # No cleartext credentials
      secrets_in_env_only = true,
    },

    # Safety invariants
    safety = {
      forbidden_patterns = [
        "unwrap()",
        "expect()",
        "todo!()",
        "unimplemented!()",
      ],
    },

    # Test requirements
    tests = {
      min_unit_tests = 30,
      integration_tests_required = true,
      media_pipeline_tests_required = true,
    },

    # Language policy
    banned_files = ["Makefile", "Dockerfile"],
    banned_languages = ["python", "typescript", "javascript", "go"],
  },

  # RSR compliance
  rsr = {
    explainme = true,
    ai_manifest = true,
    machine_readable = true,
    spdx_headers_required = true,
    security_md_required = true,
    idris2_abi = true,
    zig_ffi = true,
  },
}
