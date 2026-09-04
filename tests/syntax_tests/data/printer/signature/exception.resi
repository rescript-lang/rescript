exception Exit

@onConstructor
exception ExitEarly

exception Exit = Terminate 
exception Exit = Lib.Terminate 
exception Exit = Ns.Lib.Terminate 

/** doc comment */
exception ExitEarly({x: int})
/** doc comment */
exception ExitEarly({x: int, y: string})
