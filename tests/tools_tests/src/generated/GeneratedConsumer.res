type wrapper = {
  item: GeneratedModel.generatedType,
}

let next = GeneratedModel.generatedValue + 1

let acceptGenerated = (value: GeneratedModel.generatedType) => {
  next + GeneratedModel.generatedValue + value.id
}
