type t = Dom.domTokenList

@get external length: t => int = ""

@get
external value: t => string = "" /* experimental, from being merged with domSettableTokenList */
@set
external setValue: (t, string) => unit =
  "value" /* experimental, from being merged with domSettableTokenList */
