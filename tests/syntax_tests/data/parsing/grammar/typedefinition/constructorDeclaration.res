type t = Rgb
type t =
  | Rgb

type t =
  | White(grayscale)
  | Black(grayscale,) // trailing comma
  | Rgb(int, int, int) 
  | Rgba(int, int, int, int,) // trailing comma

type t =
  | Rgb({r: int, g: int, b: int})
  // trailing comma
  | Rgba({r: int, g: int, b: int, a: int,},)
  | JsColor({"gradient": int})
  | JsColor({"gradient": int,})
  | JsColor({"gradient": int},)
  | JsColor({@attr "gradient": int,},)
  | JsColor({"gradient": int}, color)
  | JsColor({"gradient": int}, color,)
  | JsColor({"gradient": int}, {"hex": string}, int)
  | JsColor({@attr "gradient": int}, {@attr "hex": string}, int)
  | JsT({..}) // Note: this compiles to bucklescript
  | JsT({.. "gradient": int}) // Note: this compiles to bucklescript
  | JsT({.. "gradient": int,}) // Note: this compiles to bucklescript
  | JsT({.. @attr "gradient": int,}) // Note: this compiles to bucklescript
  | JsT({.. "gradient": int, "hex": string}, {.. "gradient": int, "hex": string}) // Note: this compiles to bucklescript
  | JsT(
      {.. @attr "gradient": int, @attr "hex": string,},
      {.. @attr "gradient": int, @attr "hex": string,},
    ) // Note: this compiles to bucklescript

// gadt
type t = Rgb : t
type t =
  | Rgb : t

type t =
  | White(grayscale) : t
  | Black(grayscale,) : t// trailing comma
  | Rgb(int, int, int) : t
  | Rgba(int, int, int, int,) : t // trailing comma

type t =
  | Rgb({r: int, g: int, b: int}) : t
  // trailing comma
  | Rgba({r: int, g: int, b: int, a: int,},) : t
  | JsColor({"gradient": int}) : t
  | JsColor({"gradient": int,}) : t
  | JsColor({"gradient": int},) : t
  | JsColor({"gradient": int,},) : t
  | JsColor({@attr "gradient": int,},) : t
  | JsColor({"gradient": int}, color) : t
  | JsColor({"gradient": int}, color,) : t
  | JsColor({"gradient": int}, {"hex": string}, int) : t
  | JsT({..}) : t // Note: this compiles to bucklescript
  | JsT({.. "gradient": int}) : t // Note: this compiles to bucklescript
  | JsT({.. @attr "gradient": int}) : t // Note: this compiles to bucklescript
  | JsT({.. "gradient": int,}) : t // Note: this compiles to bucklescript
  | JsT({.. @attr "gradient": int,}) : t // Note: this compiles to bucklescript
  | JsT({.. "gradient": int,},) : t // Note: this compiles to bucklescript
  | JsT(
      {.. "gradient": int, "hex": string},
      {.. "gradient": int, "hex": string}
    ) : t // Note: this compiles to bucklescript
  | JsT(
      {.. @attr "gradient": int, @attr "hex": string,},
      {.. @attr "gradient": int, @attr "hex": string,},
    ) : t // Note: this compiles to bucklescript

// attributes
// type t = @attr EmptyColor
type t = 
  | @attr EmptyColor
  | @onConstr White(grayscale) : @onGadt t

type node <_, 'value> =
  | Root({
      mutable value: 'value,
      mutable updatedTime: float,
    }): node<root, 'value>
  | Derived({
      mutable cachedValue: 'value,
      parent: node<_, 'value>,
      root: node<root, 'value>,
      updateF: 'value => 'value,
      mutable updatedTime: float,
    }): node<derived, 'value>

type delta = Compute({"blocked_ids": unit} => unit)
type queryDelta =
  | Compute({"blocked_ids": unit} => unit)
  | Compute({"blocked_ids": unit} => unit, {"allowed_ids": unit} => unit)
