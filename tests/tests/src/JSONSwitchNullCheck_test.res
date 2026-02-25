open Mocha
open Test_utils

// Test cases for JSON switch with null and various block types
// These test the fix for: null incorrectly matching Object case in switch

// Helper to create test JSON values
let jsonNull: JSON.t = %raw(`null`)
let jsonArray: JSON.t = %raw(`["a", "b"]`)
let jsonObject: JSON.t = %raw(`{"key": "value"}`)
let jsonString: JSON.t = %raw(`"hello"`)
let jsonNumber: JSON.t = %raw(`42`)
let jsonBool: JSON.t = %raw(`true`)

describe(__MODULE__, () => {
  describe("JSON direct switch with Null - should NOT match Object", () => {
    // Test: Object | Array | _ with null - null should go to default
    test(
      "switch null { | Object(_) => Object | Array(_) => Array | _ => default }",
      () => {
        let result = switch jsonNull {
        | Object(_) => "Object"
        | Array(_) => "Array"
        | _ => "default"
        }
        eq(__LOC__, result, "default")
      },
    )

    // Test: Object | String | _ with null - null should go to default
    test(
      "switch null { | Object(_) => Object | String(_) => String | _ => default }",
      () => {
        let result = switch jsonNull {
        | Object(_) => "Object"
        | String(_) => "String"
        | _ => "default"
        }
        eq(__LOC__, result, "default")
      },
    )

    // Test: Object | Number | _ with null - null should go to default
    test(
      "switch null { | Object(_) => Object | Number(_) => Number | _ => default }",
      () => {
        let result = switch jsonNull {
        | Object(_) => "Object"
        | Number(_) => "Number"
        | _ => "default"
        }
        eq(__LOC__, result, "default")
      },
    )

    // Test: Multiple block cases with null - null should go to default
    test(
      "switch null { | Object(_) => Object | Array(_) => Array | String(_) => String | _ => default }",
      () => {
        let result = switch jsonNull {
        | Object(_) => "Object"
        | Array(_) => "Array"
        | String(_) => "String"
        | _ => "default"
        }
        eq(__LOC__, result, "default")
      },
    )
  })

  describe("JSON direct switch with Array - should NOT match Object", () => {
    // Test: Array being matched as Object - Object | String | _ with array
    test(
      "switch array { | Object(_) => Object | String(_) => String | _ => default }",
      () => {
        let result = switch jsonArray {
        | Object(_) => "Object"
        | String(_) => "String"
        | _ => "default"
        }
        eq(__LOC__, result, "default")
      },
    )

    // Test: Array being matched as Object - Object | Number | _ with array
    test(
      "switch array { | Object(_) => Object | Number(_) => Number | _ => default }",
      () => {
        let result = switch jsonArray {
        | Object(_) => "Object"
        | Number(_) => "Number"
        | _ => "default"
        }
        eq(__LOC__, result, "default")
      },
    )

    // Test: Array being matched as Object - Object | Boolean | _ with array
    test(
      "switch array { | Object(_) => Object | Boolean(_) => Boolean | _ => default }",
      () => {
        let result = switch jsonArray {
        | Object(_) => "Object"
        | Boolean(_) => "Boolean"
        | _ => "default"
        }
        eq(__LOC__, result, "default")
      },
    )
  })

  describe("JSON switch - correct positive cases", () => {
    test(
      "switch object { | Object(_) => Object | _ => default }",
      () => {
        let result = switch jsonObject {
        | Object(_) => "Object"
        | _ => "default"
        }
        eq(__LOC__, result, "Object")
      },
    )

    test(
      "switch array { | Array(_) => Array | _ => default }",
      () => {
        let result = switch jsonArray {
        | Array(_) => "Array"
        | _ => "default"
        }
        eq(__LOC__, result, "Array")
      },
    )

    test(
      "switch string { | String(_) => String | _ => default }",
      () => {
        let result = switch jsonString {
        | String(_) => "String"
        | _ => "default"
        }
        eq(__LOC__, result, "String")
      },
    )

    test(
      "switch number { | Number(_) => Number | _ => default }",
      () => {
        let result = switch jsonNumber {
        | Number(_) => "Number"
        | _ => "default"
        }
        eq(__LOC__, result, "Number")
      },
    )

    test(
      "switch bool { | Boolean(_) => Boolean | _ => default }",
      () => {
        let result = switch jsonBool {
        | Boolean(_) => "Boolean"
        | _ => "default"
        }
        eq(__LOC__, result, "Boolean")
      },
    )

    test(
      "switch null { | Null => Null | _ => default }",
      () => {
        let result = switch jsonNull {
        | Null => "Null"
        | _ => "default"
        }
        eq(__LOC__, result, "Null")
      },
    )

    test(
      "switch null { | Null => Null | Object(_) => Object | _ => default }",
      () => {
        let result = switch jsonNull {
        | Null => "Null"
        | Object(_) => "Object"
        | _ => "default"
        }
        eq(__LOC__, result, "Null")
      },
    )
  })
})
