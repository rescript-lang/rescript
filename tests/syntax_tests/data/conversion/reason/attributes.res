module Color: {
  type t = private string

  @inline("red") let red: t
  @inline("black") let black: t
} = {
  type t = string

  @inline let red = "red"
  @inline let black = "black"
}

@send external map: (array<'a>, 'a => 'b) => array<'b> = "map"
@send external filter: (array<'a>, 'a => 'b) => array<'b> = "filter"
list{1, 2, 3}->map(a => a + 1)->filter(a => modulo(a, 2) == 0)->Js.log

type t
@new external make: unit => t = "DOMParser"

Js.log(make()->parseHtmlFromString("sdsd"))
