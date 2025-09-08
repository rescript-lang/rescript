@@config({no_export: no_export})

open Belt

module Html = Dom_html

module Pg = Procedural_generator

let loadCount = ref(0)

let imgsToLoad = 4

let level_width = 2400.

let level_height = 256.

/* Canvas is chosen from the index.html file. The context is obtained from
 *the canvas. Listeners are added. A level is generated and the general
 *update_loop method is called to make the level playable. */
let load = _ => {
  Random.self_init()
  let canvas_id = "canvas"
  let canvas = switch Dom_html.getElementById(Dom_html.document, canvas_id) {
  | None =>
    print_endline("cant find canvas " ++ (canvas_id ++ " \n"))
    failwith("fail")
  | Some(el) => Dom_html.elementToCanvasElement(el)
  }
  let context = Dom_html.canvasElementToJsObj(canvas)["getContext"]("2d")
  let _ = Dom_html.addEventListener(Dom_html.document, "keydown", Director.keydown, true)
  let _ = Dom_html.addEventListener(Dom_html.document, "keyup", Director.keyup, true)
  let () = Pg.init()
  Director.update_loop(
    canvas,
    Pg.generate(level_width, level_height, context),
    (level_width, level_height),
  )
}

let inc_counter = _ => {
  loadCount := loadCount.contents + 1
  if loadCount.contents == imgsToLoad {
    load()
  } else {
    ()
  }
}

/* Used for concurrency issues. */
let preload = _ => {
  let root_dir = "sprites/"
  let imgs = list{"blocks.png", "items.png", "enemies.png", "mario-small.png"}
  List.map(imgs, img_src => {
    let img_src = root_dir ++ img_src
    let img = Html.createImg(Dom_html.document)
    Dom_html.imageElementToJsObj(img)["src"] = img_src
    ignore(
      Dom_html.addEventListenerImg(
        img,
        "load",
        _ev => {
          inc_counter()
          true
        },
        true,
      ),
    )
  })
}

Dom_html.windowToJsObj(Dom_html.window)["onload"] = _ => {
  ignore(preload())
  true
}
