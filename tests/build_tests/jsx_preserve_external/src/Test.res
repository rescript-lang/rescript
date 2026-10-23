// Regression for https://github.com/rescript-lang/rescript/issues/8047.
// External components must remain imported values, without function wrappers.
module Head = {
  type props = {children?: Preact.element}

  @module("some-lib")
  external make: Preact.component<props> = "Head"
}

let test =
  <Head>
    <div />
  </Head>
