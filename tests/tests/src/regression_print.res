include (
  {
    @val external to_str: 'a => string = "JSON.stringify"
    let debug = x => Console.log(to_str(x))

    let () = {
      debug(2)
      debug(1)
    }
  }: {}
)
