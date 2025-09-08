// note: we overparse Pext_rebindg
type t += Foo
type Foo.Bar.t += Foo
type t<'a, 'b> += Foo('a)
type Foo.Bar.t<'a, 'b> += Foo('a)

type t += private Foo
type t += private Foo | Bar
type t += private | Foo
type t += private | Foo | Bar

type t += Circle(string, int) | Point({x: int, y: int})
type t += | Circle(string, int) | Point({x: int, y: int})

type t += Foo = Bar
type t += Foo = Bar | Circle = Circle2
type t += Foo = Bar | Circle = Geo.Circle2
type t += private Foo = Bar
type t += private Foo = Bar | Circle = Circle2
type t += private Foo = Bar | Circle = Geo.Circle2
type t += | Foo = Bar
type t += | Foo = Bar | Circle = Circle2
type t += | Foo = Bar | Circle = Geo.Circle2
type t += private | Foo = Bar
type t += private | Foo = Bar | Circle = Circle2
type t += private | Foo = Bar | Circle = Geo.Circle2
type t += Foo = Geo.Metry.Bar
