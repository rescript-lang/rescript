type t

type usage = [#sort | #search]
type sensitivity = [#base | #accent | #case | #variant]
type caseFirst = [#upper | #lower | #"false"]

type options = {
  localeMatcher?: Stdlib_Intl_Common.localeMatcher,
  usage?: usage,
  sensitivity?: sensitivity,
  ignorePunctuation?: bool,
  numeric?: bool,
  caseFirst?: caseFirst,
}

type resolvedOptions = {
  locale: string,
  usage: usage,
  sensitivity: sensitivity,
  ignorePunctuation: bool,
  collation: [Stdlib_Intl_Common.collation | #default],
  numeric?: bool,
  caseFirst?: caseFirst,
}

type supportedLocalesOptions = {localeMatcher: Stdlib_Intl_Common.localeMatcher}

@new external make: (~locales: array<string>=?, ~options: options=?) => t = "Intl.Collator"

@val
external supportedLocalesOf: (array<string>, ~options: supportedLocalesOptions=?) => t =
  "Intl.Collator.supportedLocalesOf"

@send external resolvedOptions: t => resolvedOptions = "resolvedOptions"

@send external compare: (t, string, string) => int = "compare"

/**
  `ignore(collator)` ignores the provided collator and returns unit.

  This helper is useful when you want to discard a value (for example, the result of an operation with side effects)
  without having to store or process it further.
*/
external ignore: t => unit = "%ignore"
