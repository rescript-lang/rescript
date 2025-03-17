type t

type listType = [
  | #conjunction
  | #disjunction
  | #unit
]
type style = [
  | #long
  | #short
  | #narrow
]

type options = {
  localeMatcher?: Stdlib_Intl_Common.localeMatcher,
  \"type"?: listType,
  style?: style,
}

type listPartComponentType = [
  | #element
  | #literal
]

type listPart = {
  \"type": listPartComponentType,
  value: string,
}

type resolvedOptions = {
  locale: string,
  style: style,
  \"type": listType,
}

type supportedLocalesOptions = {localeMatcher: Stdlib_Intl_Common.localeMatcher}

@new external make: (~locales: array<string>=?, ~options: options=?) => t = "Intl.ListFormat"

@val
external supportedLocalesOf: (array<string>, ~options: supportedLocalesOptions=?) => t =
  "Intl.ListFormat.supportedLocalesOf"

@send external resolvedOptions: t => resolvedOptions = "resolvedOptions"

@send external format: (t, array<string>) => string = "format"
@send external formatToParts: (t, array<string>) => array<listPart> = "formatToParts"
