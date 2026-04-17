open Mocha
open Test_utils

describe(__MODULE__, () => {
  test("while loop break and continue", () => {
    let values = ref(list{})
    let i = ref(0)

    while i.contents < 6 {
      i := i.contents + 1

      if i.contents == 2 {
        continue
      }

      if i.contents == 5 {
        break
      }

      values := list{i.contents, ...values.contents}
    }

    eq(__LOC__, [1, 3, 4], values.contents->Belt.List.reverse->Belt.List.toArray)
  })

  test("switch inside while targets the loop", () => {
    let values = ref(list{})
    let i = ref(0)

    while i.contents < 6 {
      i := i.contents + 1

      switch i.contents {
      | 2 => continue
      | 5 => break
      | _ => values := list{i.contents, ...values.contents}
      }
    }

    eq(__LOC__, [1, 3, 4], values.contents->Belt.List.reverse->Belt.List.toArray)
  })

  // Keep a JS `switch` in the generated output so this exercises labeled loop control.
  test("string switch inside while targets the loop via JS switch", () => {
    let values = ref(list{})
    let i = ref(0)

    while i.contents < 6 {
      i := i.contents + 1

      let state = switch i.contents {
      | 2 => "skip"
      | 5 => "stop"
      | _ => "keep"
      }

      switch state {
      | "skip" => continue
      | "stop" => break
      | _ => values := list{i.contents, ...values.contents}
      }
    }

    eq(__LOC__, [1, 3, 4], values.contents->Belt.List.reverse->Belt.List.toArray)
  })

  test("for loop break and continue", () => {
    let values = ref(list{})

    for i in 0 to 5 {
      if i == 1 {
        continue
      }

      if i == 4 {
        break
      }

      values := list{i, ...values.contents}
    }

    eq(__LOC__, [0, 2, 3], values.contents->Belt.List.reverse->Belt.List.toArray)
  })

  test("switch inside for targets the loop", () => {
    let values = ref(list{})

    for i in 0 to 5 {
      switch i {
      | 1 => continue
      | 4 => break
      | _ => values := list{i, ...values.contents}
      }
    }

    eq(__LOC__, [0, 2, 3], values.contents->Belt.List.reverse->Belt.List.toArray)
  })

  // Keep a JS `switch` in the generated output so this exercises labeled loop control.
  test("string switch inside for targets the loop via JS switch", () => {
    let values = ref(list{})

    for i in 0 to 5 {
      let state = switch i {
      | 1 => "skip"
      | 4 => "stop"
      | _ => "keep"
      }

      switch state {
      | "skip" => continue
      | "stop" => break
      | _ => values := list{i, ...values.contents}
      }
    }

    eq(__LOC__, [0, 2, 3], values.contents->Belt.List.reverse->Belt.List.toArray)
  })
})
