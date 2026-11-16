- Accept another declaration of a JavaScript class, or a subclass, without
  `:>` at function arguments, record fields (including JSX props), and
  constructor payloads. Within another type, automatic coercion accepts only
  different declarations of the same class; nested subclass coercions still
  require `:>`.
