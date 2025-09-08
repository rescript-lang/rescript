Console.log("---")
Console.log("Intl.Segmenter")

Intl.Segmenter.supportedLocalesOf(["en-US", "en-GB"])->ignore
Intl.Segmenter.supportedLocalesOf(["en-US", "en-GB"], ~options={localeMatcher: #lookup})->ignore

let _formatter = Intl.Segmenter.make()
let _formatter = Intl.Segmenter.make(~locales=["en-US", "en-GB"])
let _formatter = Intl.Segmenter.make(
  ~options={
    granularity: #word,
  },
)
let formatter = Intl.Segmenter.make(
  ~locales=["en-US"],
  ~options={
    granularity: #word,
  },
)

formatter->Intl.Segmenter.resolvedOptions->Console.log
let segments = formatter->Intl.Segmenter.segment("This is a sentence with several words")

Console.log(segments)

Intl.Segments.containing(segments)->Console.log
Intl.Segments.containingWithIndex(segments, 1)->Console.log
