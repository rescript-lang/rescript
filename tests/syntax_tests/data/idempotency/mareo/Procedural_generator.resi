open Object

type obj_coord

let init: unit => unit

/* Procedurally generates a new map of default size */
let generate: (float, float, Dom_html.canvasRenderingContext2D) => (collidable, list<collidable>)
