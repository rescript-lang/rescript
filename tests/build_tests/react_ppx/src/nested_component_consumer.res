@react.component
let make = () =>
  <Nested_component_producer.Provider>
    <Nested_component_producer.Trigger label="x" />
    <Nested_component_producer.Menu.Item>{React.null}</Nested_component_producer.Menu.Item>
    <Nested_component_producer.Ext>{React.null}</Nested_component_producer.Ext>
    <Nested_component_producer.RecA />
  </Nested_component_producer.Provider>

let reexported =
  <Nested_component_reexport.Provider>
    <Nested_component_reexport.Menu.Item>{React.null}</Nested_component_reexport.Menu.Item>
  </Nested_component_reexport.Provider>

let awaited = async () => {
  module Producer = await Nested_component_producer
  module Menu = Producer.Menu
  <Producer.Provider>
    <Menu.Item>{React.null}</Menu.Item>
  </Producer.Provider>
}

module AwaitedProducer = await Nested_component_producer

let awaitedTop = <AwaitedProducer.Provider>{React.null}</AwaitedProducer.Provider>
