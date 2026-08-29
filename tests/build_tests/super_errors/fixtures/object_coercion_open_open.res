/* Fields are invariant when both object rows are open. An open result remains
   eligible for field promotion, so covariantly weakening its readable field
   type would be unsafe: a later write at the narrow type could reach aliases
   which read the field at the wide type. The coercion is therefore rejected.
 */

type wide = {"a": int, "b": int}
type narrow = {"a": int}
let p = (o: {.."x": wide}) => (o :> {.."x": narrow})
