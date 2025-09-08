@deriving(jsConverter)
type language = [
  | @as("de") #German
  | @as("es-eu") #SpanishEurope
  | @as("es-us") #SpanishAmerica
  | @as("fr-eu") #FrenchEurope
  | @as("fr-us") #FrenchAmerica
  | @as("it") #Italian
  | @as("ja") #Japanese
  | @as("ko") #Korean
  | @as("nl") #Dutch
  | @as("ru") #Russian
  | @as("zh-cn") #ChineseSimplified
  | @as("zh-tw") #ChineseTraditional
  | @as("en") #English
  | @as("en-gb") #EnglishEurope
]

let languages: array<language> = [
  #English,
  #EnglishEurope,
  #SpanishAmerica,
  #SpanishEurope,
  #FrenchAmerica,
  #FrenchEurope,
  #German,
  #Italian,
  #Dutch,
  #Russian,
  #Japanese,
  #Korean,
  #ChineseTraditional,
  #ChineseSimplified,
]

let languageToString = (language: language) =>
  switch language {
  | #German => `Deutsch`
  | #SpanishEurope => `Español (Europe)`
  | #SpanishAmerica => `Español (America)`
  | #FrenchEurope => `Français (Europe)`
  | #FrenchAmerica => `Français (America)`
  | #Italian => `Italiano`
  | #Japanese => `日本語`
  | #Korean => `한국어`
  | #Dutch => `Nederlands`
  | #Russian => `Русский`
  | #ChineseSimplified => `中文`
  | #ChineseTraditional => `繁體中文`
  | #English => `English`
  | #EnglishEurope => `English (UK)`
  }

@val @scope("navigator")
external browserLanguage: string = "language"

let browserLanguage = if browserLanguage == "de" {
  #German
} else if Js.String.toLowerCase(browserLanguage) == "es-es" {
  #SpanishEurope
} else if browserLanguage->Js.String.startsWith("es") {
  #SpanishAmerica
} else if Js.String.toLowerCase(browserLanguage) == "fr-ca" {
  #FrenchAmerica
} else if browserLanguage->Js.String.startsWith("fr") {
  #FrenchEurope
} else if browserLanguage->Js.String.startsWith("it") {
  #Italian
} else if browserLanguage == "ja" {
  #Japanese
} else if browserLanguage == "ko" {
  #Korean
} else if browserLanguage == "nl" {
  #Dutch
} else if browserLanguage->Js.String.startsWith("ru") {
  #Russian
} else if Js.String.toLowerCase(browserLanguage) == "zh-cn" {
  #ChineseSimplified
} else if browserLanguage->Js.String.startsWith("zh") {
  #ChineseTraditional
} else if Js.String.toLowerCase(browserLanguage) == "en-gb" {
  #EnglishEurope
} else {
  #English
}

type state = {language: language}
type action = SetLanguage(language)

let localStorageKey = "language"
let localStorageLanguage = Dom.Storage.localStorage->Dom.Storage.getItem(localStorageKey)

let api = Restorative.createStore(
  {
    language: localStorageLanguage
    ->Belt.Option.flatMap(languageFromJs)
    ->Belt.Option.getWithDefault(browserLanguage),
  },
  (state, action) =>
    switch action {
    | SetLanguage(language) => {language: language}
    },
)

let useLanguage = () => api.useStoreWithSelector(state => state.language, ())
let getLanguage = () => api.getState().language

let setLanguage = (~language) => {
  api.dispatch(SetLanguage(language))
  if language == browserLanguage {
    Dom.Storage.localStorage->Dom.Storage.removeItem(localStorageKey)
  } else {
    Dom.Storage.localStorage->Dom.Storage.setItem(localStorageKey, languageToJs(language))
  }
  Analytics.Amplitude.logEventWithProperties(
    ~eventName="Language Changed",
    ~eventProperties={"language": languageToJs(language)},
  )
}
