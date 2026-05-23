type codeBlock = {
  id: string,
  name: string,
  code: string,
}

/**
`decodeFromJson(json)` parse JSON generated from `rescript-tools extract-codeblocks` command
*/
let decodeFromJson = (json: Stdlib_JSON.t) =>
  switch json {
  | Array(codeblocks) =>
    let codeblocks = codeblocks->Stdlib_Array.filterMap(c => {
      switch c {
      | Object(dict{"id": Stdlib_JSON.String(id), "name": String(name), "code": String(code)}) =>
        Some({id, name, code})
      | _ => None
      }
    })
    Ok(codeblocks)
  | String(error) => Error(error)
  | _ => Error("Failed to decode codeblocks")
  }
