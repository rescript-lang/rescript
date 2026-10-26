// A class on a public abbreviation of an opaque type would make every value
// of that type an instance, and two such abbreviations could disagree
type node
@instanceOf("Element") type element = node
