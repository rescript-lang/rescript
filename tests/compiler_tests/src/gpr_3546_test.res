@deriving(accessors) type rec terminal<'a> = T_error: terminal<unit>

@deriving(accessors) type rec terminal2<'a> = T_error2: terminal2<unit>

@deriving(accessors) type rec terminal3<'a> = T_error3(int): terminal3<int>
