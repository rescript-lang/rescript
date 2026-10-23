type renamed = {@as("b") a: int, b2: int}

type escaped = {@as("a\nb\"c") x: int}

/* A spelling that is not the canonical one for its value. The parsetree keeps
   the literal's source, so printing must give back what was written rather
   than "A". */
type nonCanonical = {@as("\u0041") w: int}

type backquoted = {@as(`tick`) y: int}

type notAName = {@as(42) z: int}

type twoNames = {@as("d1") @as("d2") d: int}

type ordered = {@dead("x") @as("m2") m: int}

type optionalToo = {@as("o2") o?: int}

type inInlineRecord = User({@as("renamed") name: string, age: int})
