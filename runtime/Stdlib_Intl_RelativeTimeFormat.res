type t

type numeric = [#always | #auto]
type style = [#long | #short | #narrow]
type timeUnit = [#year | #quarter | #month | #week | #day | #hour | #minute | #second]

type options = {
  localeMatcher?: Stdlib_Intl_Common.localeMatcher,
  numeric?: numeric,
  style?: style,
}

type supportedLocalesOptions = {localeMatcher: Stdlib_Intl_Common.localeMatcher}

type resolvedOptions = {
  locale: string,
  numeric: numeric,
  style: style,
  numberingSystem: string,
}

type relativeTimePartComponent = [#literal | #integer]
type relativeTimePart = {
  \"type": relativeTimePartComponent,
  value: string,
  unit?: timeUnit,
}

@new
external make: (~locales: array<string>=?, ~options: options=?) => t = "Intl.RelativeTimeFormat"

@val
external supportedLocalesOf: (array<string>, ~options: supportedLocalesOptions=?) => t =
  "Intl.RelativeTimeFormat.supportedLocalesOf"

@send external resolvedOptions: t => resolvedOptions = "resolvedOptions"

@send external format: (t, int, timeUnit) => string = "format"
@send
external formatToParts: (t, int, timeUnit) => array<relativeTimePart> = "formatToParts"

/**
  `ignore(relativeTimeFormat)` ignores the provided relativeTimeFormat and returns unit.

  This helper is useful when you want to discard a value (for example, the result of an operation with side effects)
  without having to store or process it further.
*/
external ignore: t => unit = "%ignore"
