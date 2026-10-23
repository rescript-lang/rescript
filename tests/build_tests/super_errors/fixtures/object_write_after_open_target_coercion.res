/* Coercing a closed source to an open target instantiates the target's row
   tail from the source. The result is therefore closed despite the target's
   written form, and it cannot acquire write capability. This property makes
   covariance sound in the closed-source, open-target case: the narrowed
   result cannot later be promoted and used to write through the source.
 */

type wide = {"a": int, "b": int}
type narrow = {"a": int}
let p = (v: {"x": wide}) => {
  let r = (v :> {.."x": narrow})
  r["x"] = {"a": 1}
}
