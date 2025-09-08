type t

@get external valueMissing: t => bool = ""
@get external typeMismatch: t => bool = ""
@get external patternMismatch: t => bool = ""
@get external tooLong: t => bool = ""
@get external tooShort: t => bool = ""
@get external rangeUnderflow: t => bool = ""
@get external rangeOverflow: t => bool = ""
@get external stepMismatch: t => bool = ""
@get external badInput: t => bool = ""
@get external customError: t => bool = ""
@get external valid: t => bool = ""
