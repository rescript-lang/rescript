module type Signature = {
  exception Exit
  exception Exit(int)
  exception Exit(int, string)
  // trailing comma
  exception Exit(int, string,)
  exception Terminate({time: int, status: int})
  // trailing comma
  exception Terminate({time: int, status: int},)

  // attributes
  @attr
  exception Exit

  @attr
  exception Exit(int)
}
