module Common = Stdlib_Intl_Common
module Collator = Stdlib_Intl_Collator
module DateTimeFormat = Stdlib_Intl_DateTimeFormat
module ListFormat = Stdlib_Intl_ListFormat
module Locale = Stdlib_Intl_Locale
module NumberFormat = Stdlib_Intl_NumberFormat
module PluralRules = Stdlib_Intl_PluralRules
module RelativeTimeFormat = Stdlib_Intl_RelativeTimeFormat
module Segmenter = Stdlib_Intl_Segmenter
module Segments = Stdlib_Intl_Segments

/**
@throws RangeError
*/
external getCanonicalLocalesExn: string => array<string> = "Intl.getCanonicalLocales"

/**
@throws RangeError
*/
external getCanonicalLocalesManyExn: array<string> => array<string> = "Intl.getCanonicalLocales"

/**
@throws RangeError
*/
external supportedValuesOfExn: string => array<string> = "Intl.supportedValuesOf"
