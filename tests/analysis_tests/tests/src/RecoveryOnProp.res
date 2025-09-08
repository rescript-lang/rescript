let name = ""

let _ =
  <div
    onClick={_ => {
      ()
      //        let _: Res
      //                  ^com
    }}
    name="abc">
    {React.string(name)}
  </div>
