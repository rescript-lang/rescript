type t = {
  id: string,
  name: string,
  passGrade: int,
  maxGrade: int,
}

let id = t => t.id

let name = t => t.name

let passGrade = t => t.passGrade

let make = (~id, ~name, ~maxGrade, ~passGrade) => {
  id: id,
  name: name,
  maxGrade: maxGrade,
  passGrade: passGrade,
}

let makeFromJs = ecData =>
  ecData->Js.Array.map(ec =>
    make(~id=ec["id"], ~name=ec["name"], ~maxGrade=ec["maxGrade"], ~passGrade=ec["passGrade"])
  )
