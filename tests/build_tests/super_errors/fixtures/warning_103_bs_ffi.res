@val external setMode: @string [#Foo | #Bar] => unit = "setMode"

let _ = setMode(#Foo)
