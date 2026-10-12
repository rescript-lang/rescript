let value = ObjectProducer.make()
let answer = switch value {
| Left({value}) | Right({value}) => value
}
let dynamicAnswer = ObjectProducer.read(value)
