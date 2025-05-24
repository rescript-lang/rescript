@ocaml.doc(" Generate fresh identifiers ")
module IntMap = Map.Make({
  type t = int
  let compare = (x: int, y: int) => compare(x, y)
})

type typeVarsGen = {
  mutable typeNameMap: IntMap.t<string>,
  mutable typeNameCounter: int,
}

let createTypeVarsGen = () => {typeNameMap: IntMap.empty, typeNameCounter: 0}

let jsTypeNameForAnonymousTypeID = (~typeVarsGen, id) =>
  try typeVarsGen.typeNameMap->IntMap.find(id) catch {
  | Not_found =>
    typeVarsGen.typeNameCounter = typeVarsGen.typeNameCounter + 1
    let name = "T" ++ string_of_int(typeVarsGen.typeNameCounter)
    typeVarsGen.typeNameMap = typeVarsGen.typeNameMap->IntMap.add(id, name)
    name
  }
