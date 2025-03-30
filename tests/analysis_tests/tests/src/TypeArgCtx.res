type someTyp = {test: bool}
let catchResult = Support.CatchResult.Ok({
  value: {
    test: true,
  },
})

// switch catchResult { | Ok({value: }) => ()
//                                  ^com
