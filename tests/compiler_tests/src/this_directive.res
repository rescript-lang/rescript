// A directive on a @this function is the first statement of its body, before
// the binding of `this`

let plain = @this @directive("'use cache'") (self, x) => self + x

let action = @this @directive("'use server'") async (self, x) => self + x

let unusedSelf = @this @directive("'use cache'") (_self: int, x) => x + 1
