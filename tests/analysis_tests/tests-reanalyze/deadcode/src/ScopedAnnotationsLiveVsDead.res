let leafLive = 1 /* Used only by middleLive; becomes live only via propagation from a live root. */
let middleLive = leafLive /* Used only by LiveScope.root; should be kept alive by @@live. */

module LiveScope = {
  @@live /* From here on (inside LiveScope): treat items as if annotated @live. */
  let root = middleLive /* Live root: keeps middleLive -> leafLive alive via liveness propagation. */
}

let stillDeadOutside = 2 /* Outside any @@live scope and unused: should be reported as dead. */

let leafDead = 3 /* Only referenced from middleDead; should still be reported (dead). */
let middleDead = leafDead /* Only referenced from DeadScope.root; @@dead does not keep it alive. */

module DeadScope = {
  @@dead /* From here on (inside DeadScope): treat items as if annotated @dead (suppressed). */
  let root = middleDead /* Suppressed, but NOT a liveness root: middleDead/leafDead remain dead and are reported. */
}
