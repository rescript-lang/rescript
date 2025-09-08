let f = a => Js.Promise.resolve(a + a)

module C0 = {
  @react.component
  let make = async (~a) => {
    let a = await f(a)
    <div> {React.int(a)} </div>
  }
}

module C1 = {
  @react.component
  let make = async (~status) => {
    switch status {
    | #on => React.string("on")
    | #off => React.string("off")
    }
  }
}