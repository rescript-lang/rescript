# ReScript Tools

## Install

The `rescript` package provides the `rescript-tools` executable:

```sh
npm install --save-dev rescript
```

## CLI Usage

```sh
rescript-tools --help
```

### Generate documentation

Print JSON:

```sh
rescript-tools doc src/EntryPointLibFile.res
```

Write JSON:

```sh
rescript-tools doc src/EntryPointLibFile.res > doc.json
```

### Reanalyze

```sh
rescript-tools reanalyze --help
```

## Contributor documentation

- [Migration framework capabilities](src/migrate.md)
- [Reanalyze architecture](../analysis/reanalyze/README.md)

## Decode JSON

`RescriptTools.Docgen` in the standard library (`@rescript/runtime`)
decodes the JSON that `rescript-tools doc` prints:

```rescript
// Read JSON file and parse with `JSON.parseOrThrow`
json->RescriptTools.Docgen.decodeFromJson
```
