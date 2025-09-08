type person = {"say": (string, string) => unit}

@val external john: person = "john"

john["say"]("hey", "jude")
