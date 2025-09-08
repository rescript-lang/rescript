/* module Mt = Mock_mt */

Mt.from_pair_suites(
  "Closure",
  {
    open Mt
    list{
      (
        "partial",
        _ => Eq(
          {
            open Test_google_closure
            (a, b, c)
          },
          ("3", 101, [1, 2]),
        ),
      ),
    }
  },
)
