@@config({
  flags: ["-bs-jsx", "4", "-bs-jsx-preserve"],
})

let getNow = () => {
  Promise.make((res, _) => {
    setTimeout(() => {
      res(Date.make())
    }, 1000)->ignore
  })
}

module Foo = {
  @react.component
  let make = async () => {
    let now = await getNow()
    <div>
      <p> {React.string(now->Date.toLocaleString)} </p>
    </div>
  }
}

module Bar = {
  @react.component
  let make = () => {
    <div>
      <Foo />
    </div>
  }
}
