exception ExitEarly
exception ExitEarly(int)
exception ExitEarly({x: int})
exception ExitEarly({"jsExit": int})
exception ExitEarly({@attr "jsExit": int})
exception ExitEarly({@attr "jsExit": int,})
exception ExitEarly({@attr "jsExit": int, @attr "code": int,})
exception ExitEarly({"jsExit": int},)
exception ExitEarly({"jsExit": int}, {"code": int})
exception ExitEarly({"jsExit": int}, int, {"code": int},)
exception ExitEarly(
  {@attr "jsExit": int, @attr "code": int,},
  {@attr "jsExit": int, @attr "code": int,},
)
exception ExitJsStyle({..})
exception ExitJsStyle({.. "code": int})
exception ExitJsStyle({.. "code": int,})
exception ExitJsStyle({.. @attr "code": int})
exception ExitJsStyle({.. @attr "code": int,})
exception ExitJsStyle({.. "code": int, "time": int,})
exception ExitJsStyle({.. @attr "code": int, @attr "time": int,})

@onConstructor
exception ExitEarly
@onConstructor
exception ExitEarly(int)

exception Exit = Terminate 
exception Exit = Lib.Terminate 
exception Exit = Ns.Lib.Terminate 

@onConstructor
exception Exit = Terminate 
@onConstructor
exception Exit = Lib.Terminate 
