# ReScript Tools

## Install

```sh
npm install --save-dev @rescript/tools
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

Add to `bs-dev-dependencies`:

```json
"dev-dependencies": ["@rescript/tools"]
```

```rescript
// Read JSON file and parse with `JSON.parseOrThrow`
json->RescriptTools.Docgen.decodeFromJson
```
