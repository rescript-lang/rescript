/***
Not supported in Firefox
*/
type t

type granularity = [#grapheme | #word | #sentence]

type options = {
  localeMatcher?: Stdlib_Intl_Common.localeMatcher,
  granularity?: granularity,
}

type pluralCategories = [
  | #zero
  | #one
  | #two
  | #few
  | #many
  | #other
]

type resolvedOptions = {locale: string, granularity: granularity}

type supportedLocalesOptions = {localeMatcher: Stdlib_Intl_Common.localeMatcher}

@new external make: (~locales: array<string>=?, ~options: options=?) => t = "Intl.Segmenter"

@val
external supportedLocalesOf: (array<string>, ~options: supportedLocalesOptions=?) => t =
  "Intl.Segmenter.supportedLocalesOf"

@send external resolvedOptions: t => resolvedOptions = "resolvedOptions"

@send external segment: (t, string) => Stdlib_Intl_Segments.t = "segment"
