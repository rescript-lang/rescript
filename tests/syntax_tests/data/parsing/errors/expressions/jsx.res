let x = <di-v />
let x = <Unclosed >;
let x = <Foo.Bar></Free.Will>;
let x = <Foo.Bar.Baz></Foo.Bar.Boo>
let x = <Foo.bar> </Foo.baz>
let x = <Foo.bar.baz />

// Trailing hyphen errors
let x = <a- />
let x = <a-b- />
let x = <V.a- />

// Trailing dots in tag names
let x = <Foo. />
let x = <Foo></Foo.>