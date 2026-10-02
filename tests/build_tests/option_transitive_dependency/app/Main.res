let variant = Some(Facade.variant)
let record = Some(Facade.record)
let alias = Some(Facade.alias)
let abstract = Some(Facade.abstract)
let unboxed = Some(Facade.unboxed)

let abstractIsSome = Option.isSome(abstract)
let unboxedIsSome = Option.isSome(unboxed)

let abstractRoundTrip = switch abstract {
| Some(value) => Facade.abstractIsUndefined(value)
| None => false
}

let unboxedRoundTrip = switch unboxed {
| Some(value) => Facade.unboxedIsUndefined(value)
| None => false
}

let optionalAbstract = Facade.optionalAbstract(~value=Facade.abstract)
let optionalUnboxed = Facade.optionalUnboxed(~value=Facade.unboxed)
let omittedAbstract = Facade.optionalAbstract()
let omittedUnboxed = Facade.optionalUnboxed()

let optionalAbstractIsSome = Option.isSome(optionalAbstract)
let optionalUnboxedIsSome = Option.isSome(optionalUnboxed)

let optionalAbstractRoundTrip = switch optionalAbstract {
| Some(value) => Facade.abstractIsUndefined(value)
| None => false
}

let optionalUnboxedRoundTrip = switch optionalUnboxed {
| Some(value) => Facade.unboxedIsUndefined(value)
| None => false
}
