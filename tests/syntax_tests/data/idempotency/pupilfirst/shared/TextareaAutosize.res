open Webapi.Dom

@module external autosize: Dom.element => unit = "autosize"
@module("autosize")
external autosizeDestroy: Dom.element => unit = "destroy"
@module("autosize")
external autosizeUpdate: Dom.element => unit = "update"

let perform = (f, id) =>
  document->Document.getElementById(id)->OptionUtils.mapWithDefault(element => element->f, ())

let create = id => id->perform(autosize)
let update = id => id->perform(autosizeUpdate)
let destroy = id => id->perform(autosizeDestroy)
