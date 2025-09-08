/* Initiates the main game loop */
let update_loop: (
  Dom_html.canvasElement,
  (Object.collidable, list<Object.collidable>),
  (float, float),
) => unit

/* Keydown event handler function */
let keydown: Dom.keyboardEvent => bool

/* Keyup event handler function */
let keyup: Dom.keyboardEvent => bool
