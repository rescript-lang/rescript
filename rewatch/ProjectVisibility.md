# Project-owned compilation units

`sources.public` selects the compilation units that another ReScript project may
name. Its value is `"all"` (the default) or an array of module names, without
file extensions. An empty array keeps every unit of that source entry internal.

```json
{
  "name": "bindings",
  "namespace": true,
  "sources": {
    "dir": "src",
    "subdirs": true,
    "public": ["Public", "Types"]
  }
}
```

Every active source in this project may use `Helper`, including a sibling in
another source directory or the project's own dev/test sources. Another project
may use `Bindings.Public`, but not `Bindings.Helper`. The restriction is checked
by the compiler, not by deleting the helper's artifacts or sources.

A nested source entry inherits its parent's selection unless it specifies its
own `public`. This also applies when `subdirs: true` recursively discovers
folders. For example, `"public": "all"` on an explicit child entry resets the
inherited selection. Independent top-level source entries have independent
selections. Overlapping entries follow the same ordered discovery as source and
dev classification.

## Ownership

A project is one resolved `rescript.json` in the build graph, normally matching
one npm package. Ownership is not the npm name, a namespace, the workspace root,
or the command that happened to compile the project. A workspace root build and
a leaf build grant the same access. Folders within a project do not introduce
additional boundaries. `allowed-dependents` remains a separate, whole-package
policy for dependency edges; it does not grant access to internal units.

The build passes the project's identity to the compiler. Currently the identity
is a digest of its canonical resolved root. Editor compilation can keep the
source's original path spelling while passing the canonical owner separately.
Internal `.cmi` files record that identity; their CRC includes the restriction.
Moving a project requires rebuilding it. These identities are not a relocatable
precompiled-package ABI or authenticated credentials.

## Interfaces, aliases and exported APIs

A `.resi` still determines what every other unit sees, siblings included.
Omitting a nested `Impl` hides it from siblings too. This feature does not add
package-private declarations inside a public module: put shared implementation
functors in an internal unit and expose concrete APIs through public signatures.

Namespaces and module aliases do not remove the target unit's restriction.
An alias of an internal unit in a public module still cannot be used by an
external project. A deliberately sealed public submodule can expose a selected
signature without exposing the underlying alias.

Public functions and explicit includes can intentionally expose behavior from
an internal implementation. This is access control over module paths, not
information-flow or capability tracking. Library authors must still review their
public signatures and must not re-export an unsafe generic operation as a public
value or include it into a public API.

The compiler can load an internal interface while expanding types or optimizing
code from a public signature. That does not authorize source references to the
internal unit. Public type aliases and signatures should give consumers useful
names for types that form part of the supported API; the visibility feature does
not automatically invent such aliases or promote private representations.

## Builds and escape hatches

Source-only npm packages must retain their internal sources: consumers compile
those sources as the dependency's project, not their own. The same restriction
applies to installed packages and workspace links, with or without a namespace
or `namespace-entry`. Features select which sources exist; they do not change
ownership. Internal runtime bindings can remain JavaScript exports for sibling
imports, with no additional wrappers or runtime checks.

This is not security against raw JavaScript, unsafe externals, modified package
sources, or a caller deliberately forging internal compiler arguments. There
are no implicit workspace friends or cross-project test privileges. Fine-grained
internal declarations and explicit friend grants are separate proposals.
