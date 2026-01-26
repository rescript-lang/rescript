// Test case for issue #7676 - @get external with unit => 'a should give proper error
@get
external foo: unit => string = "foo"

let x = foo()
