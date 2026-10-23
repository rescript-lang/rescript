@deprecated("Use newF instead")
let oldF = x => x + 1

@deprecated("Use computeNew instead") @val
external compute: int => int = "compute"

@deprecated("Use NewM instead")
module OldM = {
  let v = 1
}

@deprecated("Use newT instead")
type oldT = int
