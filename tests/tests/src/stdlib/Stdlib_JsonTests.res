let decodeJsonTest = () => {
  let json: JSON.t = %raw(`{"someProp":{"otherProp": null, "thirdProp": [true, false]}}`)

  let decodedCorrectly = switch json {
  | Object(dict) =>
    switch dict->Dict.get("someProp") {
    | Some(Object(dict)) =>
      switch dict->Dict.get("thirdProp") {
      | Some(Array([Boolean(true), Boolean(false)])) => true
      | _ => false
      }
    | _ => false
    }
  | _ => false
  }

  Test.run(__POS_OF__("Should decode JSON successfully"), decodedCorrectly, \"==", true)
}

decodeJsonTest()
