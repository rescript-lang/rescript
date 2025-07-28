type x = {one: bool}
type xx = array<x>

let x: xx = [{one: true}]

/* === AVAILABLE ACTIONS:
- RewriteObjectToRecord - Rewrite object to record
*/
