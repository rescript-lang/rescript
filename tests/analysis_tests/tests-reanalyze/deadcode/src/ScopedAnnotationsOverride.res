module M = {
  let before = 1 /* Before any @@ annotation in M: should be reported as dead (unused). */

  @@live /* Set default to live roots for the rest of M (until overridden). */
  let live1 = 1 /* Live root (suppressed) and can keep deps alive, if it referenced anything. */

  module NestedInLive = {
    let nestedLive = 1 /* Inherits @@live from M: treated as a live root (suppressed). */
  }

  @@dead /* Override default to dead (suppressed) for the rest of M (until overridden). */
  let dead1 = 1 /* Suppressed, but NOT a liveness root: would not keep dependencies alive. */

  module NestedInDead = {
    let nestedDead = 1 /* Inherits @@dead from M: suppressed. */
  }

  @@live /* Override again: back to live roots for the remainder of M. */
  let live2 = 1 /* Live root (suppressed). */
}

let afterModules = 1 /* Outside M: scope does not leak, so this should be reported as dead (unused). */
