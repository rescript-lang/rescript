## ReScript build configuration

This document contains a list of all bsconfig parameters with remarks, and whether they are already implemented in rewatch. It is based on https://rescript-lang.org/docs/manual/latest/build-configuration-schema.

| Parameter            | JSON type               | Remark | Implemented? |
| -------------------- | ----------------------- | ------ | :----------: |
| version              | string                  |        |     [_]      |
| name                 | string                  |        |     [x]      |
| namespace            | boolean                 |        |     [x]      |
| namespace            | string                  |        |     [x]      |
| sources              | string                  |        |     [x]      |
| sources              | array of string         |        |     [x]      |
| sources              | Source                  |        |     [x]      |
| sources              | array of Source         |        |     [x]      |
| ignored-dirs         | array of string         |        |     [_]      |
| bs-dependencies      | array of string         |        |     [x]      |
| bs-dev-dependencies  | array of string         |        |     [x]      |
| pinned-dependencies  | array of string         |        |     [x]      |
| generators           | array of Rule-Generator |        |     [_]      |
| cut-generators       | boolean                 |        |     [_]      |
| jsx                  | JSX                     |        |     [x]      |
| gentypeconfig        | Gentype                 |        |     [_]      |
| bsc-flags            | array of string         |        |     [x]      |
| warnings             | Warnings                |        |     [x]      |
| ppx-flags            | array of string         |        |     [x]      |
| pp-flags             | array of string         |        |     [_]      |
| js-post-build        | Js-Post-Build           |        |     [_]      |
| package-specs        | array of Module-Format  |        |     [_]      |
| package-specs        | array of Package-Spec   |        |     [x]      |
| entries              | array of Target-Item    |        |     [_]      |
| use-stdlib           | boolean                 |        |     [_]      |
| external-stdlib      | string                  |        |     [_]      |
| bs-external-includes | array of string         |        |     [_]      |
| suffix               | Suffix                  |        |     [x]      |
| reanalyze            | Reanalyze               |        |     [_]      |

### Source

| Parameter        | JSON type                | Remark | Implemented? |
| ---------------- | ------------------------ | ------ | :----------: |
| dir              | string                   |        |     [x]      |
| type             | "dev"                    |        |     [x]      |
| files            | array of string          |        |     [_]      |
| files            | File-Object              |        |     [_]      |
| generators       | array of Build-Generator |        |     [_]      |
| public           | "all"                    |        |     [_]      |
| public           | array of string          |        |     [_]      |
| resources        | array of string          |        |     [_]      |
| subdirs          | boolean                  |        |     [x]      |
| subdirs          | string                   |        |     [_]      |
| subdirs          | array of string          |        |     [x]      |
| subdirs          | Source                   |        |     [_]      |
| subdirs          | array of Source          |        |     [x]      |
| group            | string                   |        |     [_]      |
| group            | Group                    |        |     [_]      |
| internal-depends | array of string          |        |     [_]      |

### File-Object

| Parameter | JSON type       | Remark | Implemented? |
| --------- | --------------- | ------ | :----------: |
| slow-re   | string          |        |     [_]      |
| excludes  | array of string |        |     [_]      |

### Build-Generator

| Parameter | JSON type       | Remark | Implemented? |
| --------- | --------------- | ------ | :----------: |
| name      | string          |        |     [_]      |
| edge      | array of string |        |     [_]      |

### Rule-Generator

| Parameter | JSON type | Remark | Implemented? |
| --------- | --------- | ------ | :----------: |
| name      | string    |        |     [_]      |
| command   | string    |        |     [_]      |

### JSX

| Parameter       | JSON type       | Remark | Implemented? |
| --------------- | --------------- | ------ | :----------: |
| version         | JSX-Version     |        |     [x]      |
| module          | "react"         |        |     [x]      |
| mode            | JSX-Mode        |        |     [x]      |
| v3-dependencies | array of string |        |     [x]      |

### JSX-Version

enum: 3 | 4

### JSX-Mode

enum: "classic" | "automatic"

### Gentype

| Parameter | JSON type | Remark | Implemented? |
| --------- | --------- | ------ | :----------: |
| path      | string    |        |     [_]      |

### Reanalyze

| Parameter  | JSON type                   | Remark | Implemented? |
| ---------- | --------------------------- | ------ | :----------: |
| analysis   | array of Reanalyze-Analysis |        |     [_]      |
| suppress   | array of string             |        |     [_]      |
| unsuppress | array of string             |        |     [_]      |
| transitive | boolean                     |        |     [_]      |

### Reanalyze-Analysis

enum: | "dce" | "exception" | "termination"

### Warnings

| Parameter | JSON type | Remark | Implemented? |
| --------- | --------- | ------ | :----------: |
| number    | string    |        |     [x]      |
| error     | boolean   |        |     [x]      |
| error     | string    |        |     [x]      |

### Js-Post-Build

| Parameter | JSON type | Remark | Implemented? |
| --------- | --------- | ------ | :----------: |
| cmd       | string    |        |     [_]      |

### Package-Spec

| Parameter | JSON type     | Remark | Implemented? |
| --------- | ------------- | ------ | :----------: |
| module    | Module-Format |        |     [_]      |
| in-source | boolean       |        |     [x]      |
| suffix    | Suffix        |        |     [_]      |

### Module-Format

enum: "commonjs" | "es6" | "es6-global"

### Suffix

enum: ".js" | ".mjs" | ".cjs" | ".bs.js" | ".bs.mjs" | ".bs.cjs"

### Target-Item

Not really usable by ReScript, likely to be removed.

| Parameter | JSON type        | Remark | Implemented? |
| --------- | ---------------- | ------ | :----------: |
| kind      | Target-Item-Kind |        |     [_]      |
| main      | string           |        |     [_]      |

### Target-Item-Kind

enum: "native" | "bytecode" | "js"

### Group

What is this even for? The spec says it is not even implemented in ReScript. Likely to be removed.

| Parameter | JSON type | Remark | Implemented? |
| --------- | --------- | ------ | :----------: |
| name      | string    |        |     [_]      |
| hierarchy | boolean   |        |     [_]      |
