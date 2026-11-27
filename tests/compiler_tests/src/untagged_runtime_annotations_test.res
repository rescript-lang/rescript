open Mocha
open Test_utils

%%raw(`
class Animal {}
class Dog extends Animal {}
globalThis.UntaggedAnimal = Animal
globalThis.UntaggedDog = Dog
globalThis.UntaggedCallable = class Callable extends Function {}
`)

@instanceOf("UntaggedAnimal") type animal
@instanceOf("UntaggedDog") type dog = private animal

@new external makeAnimal: unit => animal = "UntaggedAnimal"
@new external makeDog: unit => dog = "UntaggedDog"

@unboxed
type pet = Animal(animal) | Dog(dog) | Name(string)

let describePet = pet =>
  switch pet {
  | Animal(_) => "animal"
  | Dog(_) => "dog"
  | Name(name) => name
  }

let onlyAnimal = pet =>
  switch pet {
  | Animal(_) => "animal"
  | _ => "other"
  }

// The superclass case is declared after its subclass
@unboxed
type petReversed = DogFirst(dog) | AnimalSecond(animal)

let describeReversed = pet =>
  switch pet {
  | AnimalSecond(_) => "animal"
  | DogFirst(_) => "dog"
  }

// Types reached through a module alias, as the stdlib's are
module Zoo = {
  @instanceOf("UntaggedAnimal") type animal
  @instanceOf("UntaggedDog") type dog = private animal
  @new external makeDog: unit => dog = "UntaggedDog"
}

module ZooAlias = Zoo

@unboxed
type aliased = AliasedAnimal(ZooAlias.animal) | AliasedDog(ZooAlias.dog)

let describeAliased = pet =>
  switch pet {
  | AliasedAnimal(_) => "animal"
  | AliasedDog(_) => "dog"
  }

@typeOf("symbol") type sym
@val external symbolFor: string => sym = "Symbol.for"

@unboxed
type key = Sym(sym) | Str(string)

let keyKind = key =>
  switch key {
  | Sym(_) => "symbol"
  | Str(s) => s
  }

@typeOf("string") type id
external idOfString: string => id = "%identity"

@unboxed
type reference = Id(id) | Index(int)

let referenceKind = reference =>
  switch reference {
  | Id(_) => "id"
  | Index(_) => "index"
  }

@instanceOf("Intl.Locale") type locale
@new external makeLocale: string => locale = "Intl.Locale"

@unboxed
type localeLike = Locale(locale) | Tag(string)

let localeKind = locale =>
  switch locale {
  | Locale(_) => "locale"
  | Tag(tag) => tag
  }

// A phantom type hierarchy declares its subclasses as private abbreviations
// too
type nodeLike<'a>
@instanceOf("UntaggedAnimal") type animalLike<'a> = private nodeLike<'a>
type _dog<'a>
@instanceOf("UntaggedDog") type dogLike<'a> = private animalLike<_dog<'a>>
type _base
@new external makePhantomAnimal: unit => animalLike<_base> = "UntaggedAnimal"
@new external makePhantomDog: unit => dogLike<_base> = "UntaggedDog"

@unboxed
type phantomPet = PhantomAnimal(animalLike<_base>) | PhantomDog(dogLike<_base>)

let describePhantom = pet =>
  switch pet {
  | PhantomAnimal(_) => "animal"
  | PhantomDog(_) => "dog"
  }

// Instances of a class can be functions
@instanceOf("UntaggedCallable") type callable
@new external makeCallable: unit => callable = "UntaggedCallable"

@unboxed
type handler = Function(int => int) | Callable(callable)

let handlerKind = handler =>
  switch handler {
  | Callable(_) => "callable"
  | Function(_) => "function"
  }

// A callable instance without an arm reaches the default, not the function arm
@unboxed
type wideHandler =
  | WideCallable(callable)
  | WideAnimal(animal)
  | WideFunction(int => int)
  | WideName(string)
  | WideCount(float)
  | WideFlag(bool)

let wideHandlerKind = handler =>
  switch handler {
  | WideFunction(_) => "function"
  | WideName(_) => "name"
  | _ => "other"
  }

@unboxed
type boxedAnimal = BoxedAnimal(animal)

// A class declared over a type whose values are objects
@typeOf("object") type plainObject
@instanceOf("UntaggedDog") type objectDog = private plainObject
@new external makeObjectDog: unit => objectDog = "UntaggedDog"

@unboxed
type objectOrName = ObjectDog(objectDog) | ObjectName(string)

let objectOrNameKind = value =>
  switch value {
  | ObjectDog(_) => "dog"
  | ObjectName(name) => name
  }

// Annotations restating the kind a type's structure gives
@typeOf("function") type callback = int => int
type point = {px: int}
@typeOf("object") type pointObject = point

module Restated: {
  @typeOf("function") type callback
  @typeOf("object") type point
  let callback: callback
  let point: point
} = {
  type callback = int => int
  type point = {py: int}
  let callback = x => x + 1
  let point = {py: 1}
}

@unboxed
type restated = Callback(callback) | PointObject(pointObject) | Label(string)

let restatedKind = value =>
  switch value {
  | Callback(callback) => Int.toString(callback(1))
  | PointObject(point) => Int.toString(point.px)
  | Label(label) => label
  }

@unboxed
type restatedAbstract = AbstractCallback(Restated.callback) | AbstractPoint(Restated.point)

let restatedAbstractKind = value =>
  switch value {
  | AbstractCallback(_) => "callback"
  | AbstractPoint(_) => "point"
  }

// A private abbreviation has the kind of the type it abbreviates, and a
// JavaScript object type is an object
type privatePoint = private point
external makePrivatePoint: point => privatePoint = "%identity"
type jsPoint = {"pz": int}

@unboxed
type privateOrName = PrivatePoint(privatePoint) | PrivateName(string)

let privateOrNameKind = value =>
  switch value {
  | PrivatePoint(point) => Int.toString((point :> point).px)
  | PrivateName(name) => name
  }

@unboxed
type jsOrName = JsPoint(jsPoint) | JsName(string)

let jsOrNameKind = value =>
  switch value {
  | JsPoint(point) => Int.toString(point["pz"])
  | JsName(name) => name
  }

@inline(never) let id = x => x

// Typed arrays are types of their own, each recognized by its class
@unboxed
type typedArrays = Bytes(Int8Array.t) | UnsignedBytes(Uint8Array.t) | Doubles(Float64Array.t)

let typedArrayKind = value =>
  switch value {
  | Bytes(_) => "int8"
  | UnsignedBytes(_) => "uint8"
  | Doubles(_) => "float64"
  }

// No class of this name exists at run time
@instanceOf("UntaggedMissingClass") type missing

@unboxed
type withMissing = Missing(missing) | Text(string) | Count(float)

let missingKind = value =>
  switch value {
  | Missing(_) => "missing"
  | Text(text) => text
  | Count(_) => "count"
  }

@unboxed
type shared = SharedDate(Date.t) | SharedObject({x: int}) | SharedText(string) | SharedCount(float)

let missingOrDefault = value =>
  switch value {
  | Missing(_) => "missing"
  | Text(text) => text
  | _ => "other"
  }

let sharedKind = value => {
  let kind = switch value {
  | SharedDate(_) => "date"
  | SharedText(_) => "text"
  | _ => "other"
  }
  kind ++ "!"
}

describe(__MODULE__, () => {
  test("a subclass case is not taken for its superclass", () => {
    eq(__LOC__, "animal", describePet(Animal(makeAnimal())))
    eq(__LOC__, "dog", describePet(Dog(makeDog())))
    eq(__LOC__, "rex", describePet(Name("rex")))
  })

  test("a superclass case alone excludes the subclass", () => {
    eq(__LOC__, "animal", onlyAnimal(Animal(makeAnimal())))
    eq(__LOC__, "other", onlyAnimal(Dog(makeDog())))
  })

  test("declaration order does not matter", () => {
    eq(__LOC__, "dog", describeReversed(DogFirst(makeDog())))
    eq(__LOC__, "animal", describeReversed(AnimalSecond(makeAnimal())))
  })

  test("a subclass upcasts to its superclass", () => {
    let animal: animal = (makeDog() :> animal)
    eq(__LOC__, "dog", describePet(Obj.magic(animal)))
  })

  test("classes reached through a module alias", () => {
    eq(__LOC__, "animal", describeAliased(AliasedAnimal(Obj.magic(makeAnimal()))))
    eq(__LOC__, "dog", describeAliased(AliasedDog(ZooAlias.makeDog())))
  })

  test("typeOf symbol", () => {
    eq(__LOC__, "symbol", keyKind(Sym(symbolFor("key"))))
    eq(__LOC__, "text", keyKind(Str("text")))
  })

  test("typeOf string", () => {
    eq(__LOC__, "id", referenceKind(Id(idOfString("a"))))
    eq(__LOC__, "index", referenceKind(Index(1)))
  })

  test("a class at a dotted path", () => {
    eq(__LOC__, "locale", localeKind(Locale(makeLocale("en"))))
    eq(__LOC__, "de", localeKind(Tag("de")))
  })

  test("an annotated type is not wrapped in an option", () => {
    let animal = makeAnimal()
    ok(__LOC__, Obj.magic(Some(animal)) === animal)
    ok(__LOC__, Obj.magic(Some(BoxedAnimal(animal))) === animal)
  })

  test("a phantom type hierarchy", () => {
    eq(__LOC__, "animal", describePhantom(PhantomAnimal(makePhantomAnimal())))
    eq(__LOC__, "dog", describePhantom(PhantomDog(makePhantomDog())))
  })

  test("a function case excludes class instances", () => {
    eq(__LOC__, "callable", handlerKind(Callable(makeCallable())))
    eq(__LOC__, "function", handlerKind(Function(x => x)))
    eq(__LOC__, "other", wideHandlerKind(WideCallable(makeCallable())))
    eq(__LOC__, "other", wideHandlerKind(WideAnimal(makeAnimal())))
    eq(__LOC__, "function", wideHandlerKind(WideFunction(x => x)))
    eq(__LOC__, "name", wideHandlerKind(WideName("a")))
    eq(__LOC__, "other", wideHandlerKind(WideCount(1.)))
  })

  test("typed arrays of different classes", () => {
    eq(__LOC__, "int8", typedArrayKind(id(Bytes(Int8Array.fromArray([1])))))
    eq(__LOC__, "uint8", typedArrayKind(id(UnsignedBytes(Uint8Array.fromArray([1])))))
    eq(__LOC__, "float64", typedArrayKind(id(Doubles(Float64Array.fromArray([1.])))))
  })

  test("a class declared over an object type", () => {
    eq(__LOC__, "dog", objectOrNameKind(ObjectDog(makeObjectDog())))
    eq(__LOC__, "rex", objectOrNameKind(ObjectName("rex")))
  })

  test("annotations restating a structural kind", () => {
    eq(__LOC__, "2", restatedKind(id(Callback(x => x + 1))))
    eq(__LOC__, "3", restatedKind(id(PointObject({px: 3}))))
    eq(__LOC__, "a", restatedKind(id(Label("a"))))
    eq(__LOC__, "callback", restatedAbstractKind(id(AbstractCallback(Restated.callback))))
    eq(__LOC__, "point", restatedAbstractKind(id(AbstractPoint(Restated.point))))
    eq(__LOC__, "4", privateOrNameKind(id(PrivatePoint(makePrivatePoint({px: 4})))))
    eq(__LOC__, "a", privateOrNameKind(id(PrivateName("a"))))
    eq(__LOC__, "5", jsOrNameKind(id(JsPoint({"pz": 5}))))
    eq(__LOC__, "b", jsOrNameKind(id(JsName("b"))))
  })

  test("primitive cases are tested before class checks", () => {
    eq(__LOC__, "a", missingKind(id(Text("a"))))
    eq(__LOC__, "count", missingKind(id(Count(1.))))
  })

  test("a primitive without an arm skips class checks", () => {
    eq(__LOC__, "a", missingOrDefault(id(Text("a"))))
    eq(__LOC__, "other", missingOrDefault(id(Count(1.))))
  })

  test("a default shared by a primitive and an object case", () => {
    eq(__LOC__, "date!", sharedKind(id(SharedDate(Date.make()))))
    eq(__LOC__, "other!", sharedKind(id(SharedObject({x: 1}))))
    eq(__LOC__, "text!", sharedKind(id(SharedText("a"))))
    eq(__LOC__, "other!", sharedKind(id(SharedCount(1.))))
  })
})
