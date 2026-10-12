let value = ObjectDomainProducer.make()
let answer = switch value {
| Literal({value}) => value
| Text({value}) => -value
| Number({amount}) => Float.toInt(amount)
}
let dynamicAnswer = ObjectDomainProducer.read(value)
