let f =
  @directive("'use cache'")
  async (p1, ~p2, ~p3) => {
    await Promise.make((resolve, _reject) => resolve((p1, p2, p3)))
  }

let result = f(1, ~p2=2, ~p3=3)
