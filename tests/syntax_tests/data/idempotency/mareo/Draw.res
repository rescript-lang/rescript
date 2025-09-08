open Sprite

module Html = Dom_html

let render_bbox = (sprite, (posx, posy)) => {
  let context = Dom_html.canvasRenderingContext2DToJsObj(sprite.context)
  let (bbox, bboy) = sprite.params.bbox_offset
  let (bbsx, bbsy) = sprite.params.bbox_size
  context["strokeStyle"] = "#FF0000"
  context["strokeRect"](posx +. bbox, posy +. bboy, bbsx, bbsy)
}

/* Draws a sprite onto the canvas. */
let render = (sprite, (posx, posy)) => {
  let context = Dom_html.canvasRenderingContext2DToJsObj(sprite.context)
  let (sx, sy) = sprite.params.src_offset
  let (sw, sh) = sprite.params.frame_size
  let (dx, dy) = (posx, posy)
  let (dw, dh) = sprite.params.frame_size
  let sx = sx +. float_of_int(sprite.frame.contents) *. sw
  /* print_endline (string_of_int !(sprite.frame)); */
  /* context##clearRect(0.,0.,sw, sh); */
  ignore(context["drawImage"](sprite.img, sx, sy, sw, sh, dx, dy, dw, dh))
}

/* Draws two background images, which needs to be done because of the
 *constantly changing viewport, which is always at most going to be
 *between two background images. */
let draw_bgd = (bgd, off_x) => {
  render(bgd, (-.off_x, 0.))
  render(bgd, (fst(bgd.params.frame_size) -. off_x, 0.))
}

/* Used for animation updating. Canvas is cleared each frame and redrawn. */
let clear_canvas = canvas => {
  let canvas = Dom_html.canvasElementToJsObj(canvas)
  let context = Dom_html.canvasRenderingContext2DToJsObj(canvas["getContext"]("2d"))
  let cwidth = float_of_int(canvas["width"])
  let cheight = float_of_int(canvas["height"])
  ignore(context["clearRect"](0., 0., cwidth, cheight))
}

/* Displays the text for score and coins. */
let hud = (canvas, score, coins) => {
  let score_string = string_of_int(score)
  let coin_string = string_of_int(coins)
  let canvas = Dom_html.canvasElementToJsObj(canvas)
  let context = Dom_html.canvasRenderingContext2DToJsObj(canvas["getContext"]("2d"))
  ignore(context["font"] = "10px 'Press Start 2P'")
  ignore(
    context["fillText"]("Score: " ++ score_string, float_of_int(canvas["width"]) -. 140., 18.),
  )
  ignore(context["fillText"]("Coins: " ++ coin_string, 120., 18.))
}

/* Displays the fps. */
let fps = (canvas, fps_val) => {
  let fps_str = int_of_float(fps_val)->string_of_int
  let canvas = Dom_html.canvasElementToJsObj(canvas)
  let context = Dom_html.canvasRenderingContext2DToJsObj(canvas["getContext"]("2d"))
  ignore(context["fillText"](fps_str, 10., 18.))
}

/* game_win displays a black screen when you finish a game. */
let game_win = ctx => {
  let ctx = Dom_html.canvasRenderingContext2DToJsObj(ctx)
  ignore(ctx["rect"](0., 0., 512., 512.))
  ignore(ctx["fillStyle"] = "black")
  ignore(ctx["fill"]())
  ignore(ctx["fillStyle"] = "white")
  ignore(ctx["font"] = "20px 'Press Start 2P'")
  ignore(ctx["fillText"]("You win!", 180., 128.))
  failwith("Game over.")
}

/* gave_loss displays a black screen stating a loss to finish that level play. */
let game_loss = ctx => {
  let ctx = Dom_html.canvasRenderingContext2DToJsObj(ctx)
  ignore(ctx["rect"](0., 0., 512., 512.))
  ctx["fillStyle"] = "black"
  ignore(ctx["fill"]())
  ctx["fillStyle"] = "white"
  ctx["font"] = "20px 'Press Start 2P'"
  ignore(ctx["fillText"]("GAME OVER. You lose!", 60., 128.))
  failwith("Game over.")
}
