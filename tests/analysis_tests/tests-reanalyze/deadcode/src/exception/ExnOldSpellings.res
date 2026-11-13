// Only @throws and @doesNotThrow are honored; the other spellings are
// ordinary unknown attributes.

@raises(Not_found)
let oldRaisesAnnotation = () => throw(Not_found)

@raise(Not_found)
let oldRaiseAnnotation = () => throw(Not_found)

@throw(Not_found)
let singularThrowAnnotation = () => throw(Not_found)

let oldDoesNotRaise = () => @doesNotRaise Belt.Array.getExn([], 0)

let oldDoNotRaise = () => @doNotRaise Belt.Array.getExn([], 0)

let doNotThrowVariant = () => @doNotThrow Belt.Array.getExn([], 0)

let lowercaseDoesNotThrow = () => @doesnotthrow Belt.Array.getExn([], 0)
