// Migration tests for new deprecations in packages/@rescript/runtime/Js.res

// typeof migration
let tyNum = Js.typeof(1)

// nullToOption
let nToOpt = Js.nullToOption(Js.Null.return(1))
