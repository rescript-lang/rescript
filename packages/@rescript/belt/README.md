# @rescript/belt

Belt is an extended utility and collection library for ReScript. It provides performance-oriented
collection types, with a particular emphasis on immutable collections, alongside utility modules
for common data types and operations. Belt is distributed separately from the compiler and standard
library.

## Installation

Install the package with your package manager:

```sh
npm install @rescript/belt
```

Then add it to your `rescript.json` dependencies:

```json
{
  "dependencies": ["@rescript/belt"]
}
```

Belt is not opened automatically. Use modules through names such as `Belt.Array` and `Belt.Map`, or explicitly `open Belt` in source files that should use the shorter module names.
