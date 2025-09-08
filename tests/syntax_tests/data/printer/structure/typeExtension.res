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

@attr type t += @attr1 Foo
@attr
type t += | @attr1 Foo
@attr
type t += @attr1 Foo | @attr2 Bar
@attr
type t += | @attr1 Foo | @attr2 Bar

@attr
type t += private @attr1 Foo
@attr
type t += private | @attr1 Foo
@attr
type t += private @attr1 Foo | @attr2 Bar
@attr
type t += private | @attr1 Foo | @attr2 Bar

@attr
type t += @attr1 Foo = Bar
@attr
type t += | @attr1 Foo = Bar
@attr
type t += @attr1 Foo = Foo2 | @attr2 Bar = Bar2
@attr
type t += | @attr1 Foo = Foo2 | @attr2 Bar = Bar2

@attr
type t += private @attr1 Foo = Foo2
@attr
type t += private | @attr1 Foo = Foo2
@attr
type t += private @attr1 Foo = Foo2 | @attr2 Bar = Bar2
@attr
type t += private | @attr1 Foo = Foo2 | @attr2 Bar = Bar2


type t += private
  | @attr1 Foo = Foo2

type t +=
  private
  | @attr1 Foo = Foo2
  | @attr2 Bar = Bar2


type t +=
  | Foo = Bar

module Tid = {
  type t<_> = ..
  type s<_> = ..
  type u<_> = ..
}

module type Tid = {
  type t
  type Tid.t<_> += Tid: Tid.t<t>

  type Tid.s<_> +=
    | Tid: Tid.s<t>

  type Tid.u<_> +=
    | Uid: Tid.u<t>
    | Uid2: Tid.u<t>
}
