@@config({flags: ["-bs-jsx", "4"]})

module ComponentWithOptionalProps = {
  @react.component
  let make = (
    ~i as _: option<int>=?,
    ~s as _: option<string>=?,
    ~element as _: option<React.element>=?,
  ) => React.null
}

let _element = <ComponentWithOptionalProps i=1 s="test" element={<div />} />
