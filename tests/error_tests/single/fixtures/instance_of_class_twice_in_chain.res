@instanceOf("Animal") type animal
@instanceOf("Dog") type dog = private animal
@instanceOf("Animal") type t = private dog
