Console.log("---")
Console.log("Intl.NumberFormat")

let currencyFormatter = Intl.NumberFormat.make(
  ~locales=["fr-FR"],
  ~options={currency: "EUR", style: #currency},
)

Console.log(Intl.NumberFormat.supportedLocalesOf(["fr-FR", "en-US"]))
Console.log(currencyFormatter->Intl.NumberFormat.format(123.23))

let roundingFormatter = Intl.NumberFormat.make(
  ~options={
    roundingIncrement: #500,
    minimumFractionDigits: #2,
    maximumFractionDigits: #2,
  },
)

let groupingFormatter1 = Intl.NumberFormat.make(
  ~options={
    useGrouping: Intl.NumberFormat.Grouping.fromBool(true),
  },
)
let groupingFormatter2 = Intl.NumberFormat.make(
  ~options={
    useGrouping: Intl.NumberFormat.Grouping.fromString(#auto),
  },
)

let sigFormatter = Intl.NumberFormat.make(
  ~options={
    minimumIntegerDigits: #1,
    minimumFractionDigits: #1,
    maximumFractionDigits: #1,
    minimumSignificantDigits: #1,
    maximumSignificantDigits: #1,
  },
)

let options = sigFormatter->Intl.NumberFormat.resolvedOptions
Console.log(options)
options.useGrouping->Intl.NumberFormat.Grouping.parseJsValue->Console.log
