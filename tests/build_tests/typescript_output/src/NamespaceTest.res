module Actions: {
  type t

  @tag("kind")
  type target = This | CssSelector({selector: string})

  @tag("kind")
  type rec action =
    | ToggleClass({target: target, className: string})
    | RemoveClass({target: target, className: string})
    | AddClass({target: target, className: string})
    | RemoveElement({target: target})

  let make: array<action> => t
} = {
  @opaque
  type t = string

  @tag("kind")
  type target = This | CssSelector({selector: string})

  @tag("kind")
  type rec action =
    | ToggleClass({target: target, className: string})
    | RemoveClass({target: target, className: string})
    | AddClass({target: target, className: string})
    | RemoveElement({target: target})

  external stringifyActions: array<action> => string = "JSON.stringify"

  let make = actions => stringifyActions(actions)
}
