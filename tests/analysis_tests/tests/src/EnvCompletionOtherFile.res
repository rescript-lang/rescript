type someResult<'a, 'b> = Okay('a) | Failure('b)

type r1 = {age: int}

type theVariant = First | Second(r1)

type someRecord<'thing> = {
  name: string,
  theThing: 'thing,
  theVariant: theVariant,
}

type response = {stuff: theVariant, res: someResult<theVariant, string>}
