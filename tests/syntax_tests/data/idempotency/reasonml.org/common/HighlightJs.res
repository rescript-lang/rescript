@deriving(abstract)
type highlightResult = {value: string}

@module("highlight.js/lib/highlight")
external highlight: (~lang: string, ~value: string) => highlightResult = "highlight"
