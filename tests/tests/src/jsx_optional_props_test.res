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

let _comment_only_host = <div>{/* no children prop */}</div>
let _comment_only_fragment = <>{/* no children */}</>
let _comment_single_child =
  <div>
    {/* before */}
    <span />
    {/* after */}
  </div>
let _comment_multiple_children =
  <div>
    <span />
    {/* between */}
    <span />
  </div>
