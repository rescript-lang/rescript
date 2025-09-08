type objT = {"name": string, "age": int}

type nestedObjT = {"y": objT}

module Rec = {
  type recordt = {xx: int, ss: string}

  let recordVal: recordt = assert false
}

let object: objT = {"name": "abc", "age": 4}
