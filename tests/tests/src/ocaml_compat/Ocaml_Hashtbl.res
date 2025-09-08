// FIXME:
//   This exists for compatibility reason.
//   Move this into Pervasives or Core

// Below is all deprecated and should be removed in v13

@noalloc external seeded_hash_param: (int, int, int, 'a) => int = "%hash"

@deprecated("Use an alternative library. This will be removed in v13")
let hash = x => seeded_hash_param(10, 100, 0, x)
