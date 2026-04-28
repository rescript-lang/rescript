let emptyDictPatternShouldCompile = switch JSON.Object(dict{}) {
| JSON.Object(dict{}) => true
| _ => false
}
