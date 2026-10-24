module Completion_test = Completion

let%expect_test "completes a nested JSX component prop" =
  let text =
    {|module O = {
  module Comp = {
    @react.component
    let make = (~first="", ~zoo=3, ~second) => React.string(first ++ second ++ Int.toString(zoo))
  }
}
|}
  in
  Test.with_project ~name:"completion-jsx" ~jsx:true ~text
    (fun ~root ~doc client ->
      Test.change_document client doc ~version:1
        ~text:(text ^ {|let comp = <O.Comp z|});
      Test.snapshot ~root client (Completion_test.completion doc (6, 20)));
  [%expect
    {| [ { "detail": "option<int>", "kind": 4, "label": "zoo", "tags": [] } ] |}]

let%expect_test "completes a value in a JSX prop expression" =
  let text =
    {|module Comp = {
  @react.component
  let make = (~second) => React.string(second)
}
let zzz = "hello"
|}
  in
  Test.with_project ~name:"completion-jsx" ~jsx:true ~text
    (fun ~root ~doc client ->
      Test.change_document client doc ~version:1
        ~text:(text ^ {|let comp = <Comp second=z|});
      Test.snapshot ~root client (Completion_test.completion doc (5, 25)));
  [%expect
    {| [ { "detail": "string", "kind": 12, "label": "zzz", "tags": [] } ] |}]
