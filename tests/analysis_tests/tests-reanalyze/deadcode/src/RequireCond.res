@module
@deprecated(
  "Please use this syntax to guarantee safe usage: [%requireCond(`gk, \"gk_name\", ConditionalModule)]"
)
external make: (
  @string [@as("qe.bool") #qeBool | @as("gk") #gk],
  string,
  string,
) => Js.Nullable.t<'a> = "requireCond"

@module
@deprecated(
  "Please use this syntax to guarantee safe usage: [%requireCond(`gk, \"gk_name\", {\"true\": ModuleA, \"false\": ModuleB})]"
)
external either: (
  @string [@as("qe.bool") #qeBool | @as("gk") #gk],
  string,
  {"true": string, "false": string},
) => 'b = "requireCond"

