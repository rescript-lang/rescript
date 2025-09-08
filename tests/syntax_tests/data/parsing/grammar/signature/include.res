module type Sig = {
  include BtreeType 
  include Ns.BtreeType 

  @onInclude
  include @onModType BtreeType 

  include (BtreeType)

  @onInclude
  include @onModType (BtreeType)

  include (S: SetLike) => {let s: S.t}
  include SetLike => {let s: int}

  include {
    let s: string
    let y: int
  }

  @onInclude
  include @onSignature {
    let s: string
    let y: int
  }

  include %extension
  include %extension.with.args("foo")

  @onInclude
  include @onExtension %extension

  include Foo with type t = string
  @onInclude
  include @onModType Foo with type t = string

  include module type of String
  @onInclude
  include @onModtype module type of String
}
