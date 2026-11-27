@instanceOf("Animal") type animal
@instanceOf("Animal") type pet

@unboxed
type t = Animal(animal) | Pet(pet)
