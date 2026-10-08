@deprecated("use List")
module Old = List

// @warning applies to the module looked up by `module type of`, also when a
// dynamic import declares it
module type Suppressed = module type of @warning("-3") Old
module M = @warning("-3") (await Old)

module type Reported = module type of Old
