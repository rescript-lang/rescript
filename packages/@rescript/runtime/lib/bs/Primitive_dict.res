// Note: this is exposed to support syntax
external make: array<(string, 'a)> => dict<'a> = "%makedict"

// Note: this is exposed to support syntax
@variadic @val external spread: (dict<'a>, array<dict<'a>>) => dict<'a> = "Object.assign"
