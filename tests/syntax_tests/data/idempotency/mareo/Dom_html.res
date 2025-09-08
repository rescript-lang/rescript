type imageElement

type canvasRenderingContext2D

type canvasElement

@val external document: Dom.document = ""

@val external window: Dom.window = ""

/* external createImg: (_ [@as "img"]) -> document -> imageElement = "createElement" [@@send] */
@send
external createImg: (Dom.document, @as("img") _) => imageElement = "createElement"

@val external requestAnimationFrame: (float => unit) => unit = ""

@return(null_to_opt) @send
external getElementById: (Dom.document, string) => option<Dom.element> = ""

@send
external addEventListener: (Dom.document, string, Dom.event_like<'a> => bool, bool) => unit = ""

@send
external addEventListenerImg: (imageElement, string, Dom.event_like<'a> => bool, bool) => unit =
  "addEventListener"

/* unsafe casts */
external imageElementToJsObj: imageElement => {..} = "%identity"

external canvasRenderingContext2DToJsObj: canvasRenderingContext2D => {..} = "%identity"

external canvasElementToJsObj: canvasElement => {..} = "%identity"

external keyboardEventToJsObj: Dom.keyboardEvent => {..} = "%identity"

external elementToCanvasElement: Dom.element => canvasElement = "%identity"

external windowToJsObj: Dom.window => {..} = "%identity"
