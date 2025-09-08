type translations

@module("react-translate")
external useTranslate: (. string, . string) => string = "useTranslate"

type translationModeContext = {
  translationMode: string,
  translationModeCrypto: bool,
  setTranslationModeCrypto: bool => unit,
}

module TranslatorProvider = {
  @module("react-translate") @react.component
  external make: (~children: React.element, ~translations: translations) => React.element =
    "TranslatorProvider"
}

let cryptoMuggleTranslations: translations = %raw(`
    {
      locale: "en",
      crypto: {
        bluetext: "Always for sale",
        ethereum: "ethereum based",
        tokens: "tokens",
        subHeading:
          "Link your social profile to your address and showcase your giving.",
        nft: "Non-Fungible Token",
        whatIsANFT:
          " is a way to ensure digital assets are unique and easily tradable on a blockchain.",
        harbergerTax: "Harberger Tax"
      },
      muggle: {
        bluetext: "Always",
        ethereum: "raising funds for",
        tokens: "",
        subHeading: "Adopt an animal to start giving today. ",
        nft: "Blockchain Animal",
        whatIsANFT:
          " is a digital animal that is completely unique and easily tradable on a blockchain.",
        harbergerTax: "Monthly Pledge"
      }
    }
`)

module TranslationContext = {
  let translationContext = React.createContext({
    translationMode: "undefined",
    translationModeCrypto: false,
    setTranslationModeCrypto: _ => (),
  })

  let make = React.Context.provider(translationContext)
}
let useTranslationModeContext = () => React.useContext(TranslationContext.translationContext)

@react.component
let make = (~children) => {
  let (translationModeCrypto, setTranslationModeCrypto) = React.useState(_ => false)

  <TranslatorProvider translations=cryptoMuggleTranslations>
    <TranslationContext
      value={
        translationMode: translationModeCrypto ? "crypto" : "muggle",
        translationModeCrypto: translationModeCrypto,
        setTranslationModeCrypto: newMode => setTranslationModeCrypto(_ => newMode),
      }>
      children
    </TranslationContext>
  </TranslatorProvider>
}
