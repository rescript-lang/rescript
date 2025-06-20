open Jest

describe("Expect", () => {
  open Expect
  test("toBe", () => expect(true)->toEqual(true))
})
