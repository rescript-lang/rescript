@react.component
let make = () =>
  <Nested_component_producer.Provider>
    <Nested_component_producer.Trigger label="x" />
    <Nested_component_producer.Menu.Item>{React.null}</Nested_component_producer.Menu.Item>
    <Nested_component_producer.Ext>{React.null}</Nested_component_producer.Ext>
    <Nested_component_producer.RecA />
  </Nested_component_producer.Provider>
