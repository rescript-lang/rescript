@instanceOf("Animal") type animal
type phantom<'a> = animal
@instanceOf("Dog") type dog = phantom<int>
