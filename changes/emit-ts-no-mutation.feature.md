- Support `@tscert.noMutation` on functions with experimental TypeScript emission.
  The emitted return type states an empty write footprint for TScert to check
  and the generated file references the TScert package's effect declarations.
  Annotated forwarding functions keep their signatures in the emitted code.
