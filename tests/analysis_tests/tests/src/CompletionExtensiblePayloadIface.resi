type attr = ..
type tone = Light | Dark
module Inner: {
  type attr += Toned(tone)
}
