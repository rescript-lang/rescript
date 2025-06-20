open Jest

describe("Expect", () => {
  open Expect
  test("toBe", () => expect(Accounting.dummyTimes2(5))->toBe(10))
})
