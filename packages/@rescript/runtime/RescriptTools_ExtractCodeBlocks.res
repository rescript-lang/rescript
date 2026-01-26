type codeBlock = {
  id: string,
  name: string,
  code: string,
}

/**
`decodeFromJson(json)` parse JSON generated from `rescript-tools extract-codeblocks` command
*/
external decodeFromJson: Stdlib_JSON.t => result<array<codeBlock>, string> = "%identity"
