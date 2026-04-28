type field = {
  name: string,
  docstrings: array<string>,
  signature: string,
  optional: bool,
  deprecated?: string,
}

@tag("kind")
type constructorPayload = | @as("inlineRecord") InlineRecord({fields: array<field>})

type constructor = {
  name: string,
  docstrings: array<string>,
  signature: string,
  deprecated?: string,
  payload?: constructorPayload,
}

type rec typeInSignature = {
  path: string,
  genericTypeParameters: array<typeInSignature>,
}

type signatureDetails = {
  parameters: array<typeInSignature>,
  returnType: typeInSignature,
}

@tag("kind")
type detail =
  | @as("record") Record({items: array<field>})
  | @as("variant") Variant({items: array<constructor>})
  | @as("alias") Signature({details: signatureDetails})

type source = {
  filepath: string,
  line: int,
  col: int,
}

@tag("kind")
type rec item =
  | @as("value")
  Value({
      id: string,
      docstrings: array<string>,
      signature: string,
      name: string,
      deprecated?: string,
      source: source,
      /** Additional documentation of signature, if available. */
      detail?: detail,
    })
  | @as("type")
  Type({
      id: string,
      docstrings: array<string>,
      signature: string,
      name: string,
      deprecated?: string,
      source: source,
      /** Additional documentation for constructors and record fields, if available. */
      detail?: detail,
    })
  | @as("module")
  Module({
      id: string,
      docstrings: array<string>,
      deprecated?: string,
      name: string,
      moduletypeid?: string,
      source: source,
      items: array<item>,
    })
  | @as("moduleType")
  ModuleType({
      id: string,
      docstrings: array<string>,
      deprecated?: string,
      name: string,
      source: source,
      items: array<item>,
    })
  | @as("moduleAlias")
  ModuleAlias({
      id: string,
      docstrings: array<string>,
      name: string,
      source: source,
      items: array<item>,
    })

type doc = {
  name: string,
  deprecated: option<string>,
  docstrings: array<string>,
  source: source,
  items: array<item>,
}

/**
`decodeFromJson(json)` parse JSON generated from `restool doc` command
*/
external decodeFromJson: Stdlib_JSON.t => doc = "%identity"
