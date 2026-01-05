// Use type from a namespaced dependency
let copy = (params: TestModule.Globals.URLSearchParams.t): TestModule.Globals.URLSearchParams.t => {
  let newParams = TestModule.Globals.URLSearchParams.make()
  TestModule.Globals.URLSearchParams.forEach(params, (value, key, _) => {
    TestModule.Globals.URLSearchParams.append(newParams, key, value)
  })
  newParams
}
