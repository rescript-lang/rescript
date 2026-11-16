// One package's declarations of the classes of class_identity_test.res. The
// test declares its own slice of the same classes: a class type goes by the
// class it names, whichever declaration names it.

@instanceOf("ClassIdentityNode") type node
@instanceOf("ClassIdentityElement") type element = private node
@instanceOf("ClassIdentityInput") type input = private element
@instanceOf("ClassIdentityEvent") type event

@new external makeInput: unit => input = "ClassIdentityInput"
@new external makeEvent: unit => event = "ClassIdentityEvent"
@send external focus: element => unit = "focus"
@get external focused: element => bool = "focused"
