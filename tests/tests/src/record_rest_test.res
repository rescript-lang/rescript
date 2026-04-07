open Mocha
open Test_utils

type config = {
  name: string,
  version: string,
  debug: bool,
}

type subConfig = {
  version: string,
  debug: bool,
}

type aliasedSubConfig = subConfig

type renamedConfig = {
  @as("user-name")
  name: string,
  version: string,
  debug: bool,
}

let describeConfig = (c: config) =>
  switch c {
  | {name, ...subConfig as rest} => (name, rest)
  }

let getAliasedRest = ({name: _, ...aliasedSubConfig as rest}: config) => rest

let getRenamedRest = ({name: _, ...subConfig as rest}: renamedConfig) => rest

let getName = ({name, ...subConfig as _rest}: config) => name
let getWholeConfig = ({...config as rest}: config) => rest

type fullProps = {
  className?: string,
  style?: string,
  onClick: unit => unit,
}

type baseProps = {
  className?: string,
  style?: string,
  onClick: unit => unit,
}

let extractClassName = ({className: ?_, ...baseProps as rest}: fullProps) => rest

type container<'a> = {
  id: string,
  value: 'a,
}

type valueContainer<'a> = {
  value: 'a,
}

let getValue = ({id: _, ...valueContainer<'a> as rest}: container<'a>) => rest

type wrapped =
  | Wrap(config)
  | Mirror(config)

let getTupleRest = (({name: _, ...subConfig as rest}, _): (config, int)) => rest

let getWrappedRest = wrapped =>
  switch wrapped {
  | Wrap({name: _, ...subConfig as rest})
  | Mirror({name: _, ...subConfig as rest}) => rest
  }

type inlineWrapped =
  | InlineWrap({name: string, version: string, debug: bool})
  | InlineMirror({name: string, version: string, debug: bool})

let getInlineWrappedRest = wrapped =>
  switch wrapped {
  | InlineWrap({name: _, ...subConfig as rest})
  | InlineMirror({name: _, ...subConfig as rest}) => rest
  }

type renamedInlineWrapped =
  | RenamedInlineWrap({
      @as("user-name")
      name: string,
      version: string,
      debug: bool,
    })
  | RenamedInlineMirror({
      @as("user-name")
      name: string,
      version: string,
      debug: bool,
    })

let getRenamedInlineWrappedRest = wrapped =>
  switch wrapped {
  | RenamedInlineWrap({name: _, ...subConfig as rest})
  | RenamedInlineMirror({name: _, ...subConfig as rest}) => rest
  }

@tag("kind")
type customTaggedInlineWrapped =
  | CustomInlineWrap({name: string, version: string, debug: bool})
  | CustomInlineMirror({name: string, version: string, debug: bool})

let getCustomTaggedInlineWrappedRest = wrapped =>
  switch wrapped {
  | CustomInlineWrap({name: _, ...subConfig as rest})
  | CustomInlineMirror({name: _, ...subConfig as rest}) => rest
  }

@tag("custom-tag")
type dashedTaggedInlineWrapped =
  | DashedInlineWrap({name: string, version: string, debug: bool})
  | DashedInlineMirror({name: string, version: string, debug: bool})

let getDashedTaggedInlineWrappedRest = wrapped =>
  switch wrapped {
  | DashedInlineWrap({name: _, ...subConfig as rest})
  | DashedInlineMirror({name: _, ...subConfig as rest}) => rest
  }

describe(__MODULE__, () => {
  test("let binding captures record rest value", () => {
    let {name, ...subConfig as rest} = ({name: "test", version: "1.0", debug: true}: config)
    eq(__LOC__, name, "test")
    eq(__LOC__, rest, {version: "1.0", debug: true})
  })

  test("match arm returns the named field and the rest record", () => {
    eq(
      __LOC__,
      describeConfig({name: "match", version: "2.0", debug: false}),
      ("match", {version: "2.0", debug: false}),
    )
  })

  test("function parameter destructuring keeps the named field", () => {
    eq(__LOC__, getName({name: "param", version: "3.0", debug: true}), "param")
  })

  test("record rest accepts type aliases to record shapes", () => {
    eq(
      __LOC__,
      getAliasedRest({name: "aliased", version: "3.1", debug: false}),
      {version: "3.1", debug: false},
    )
  })

  test("record rest excludes fields renamed with @as", () => {
    eq(
      __LOC__,
      getRenamedRest({name: "renamed", version: "3.2", debug: true}),
      {version: "3.2", debug: true},
    )
  })

  test("empty-field rest pattern still binds the whole record", () => {
    eq(
      __LOC__,
      getWholeConfig({name: "whole", version: "3.5", debug: false}),
      {name: "whole", version: "3.5", debug: false},
    )
  })

  test("rest-only record patterns can also bind the whole alias", () => {
    let {...config as rest} as whole = ({name: "wholeAlias", version: "3.6", debug: true}: config)
    eq(__LOC__, whole, {name: "wholeAlias", version: "3.6", debug: true})
    eq(__LOC__, rest, {name: "wholeAlias", version: "3.6", debug: true})
  })

  test("optional overlap keeps the remaining fields in the rest object", () => {
    let onClick = () => ()
    let rest = extractClassName({className: "btn", style: "bold", onClick})
    eq(__LOC__, rest, {style: "bold", onClick})
  })

  test("polymorphic rest captures the value field", () => {
    let {id, ...valueContainer<int> as intRest} = ({id: "1", value: 42}: container<int>)
    eq(__LOC__, id, "1")
    eq(__LOC__, intRest, {value: 42})
    eq(__LOC__, getValue({id: "2", value: "hello"}), {value: "hello"})
  })

  test("tuple nested record rest is initialized", () => {
    eq(
      __LOC__,
      getTupleRest((({name: "tuple", version: "4.0", debug: false}: config), 1)),
      {version: "4.0", debug: false},
    )
  })

  test("variant payload rest works through the or-pattern path", () => {
    eq(
      __LOC__,
      getWrappedRest(Wrap({name: "wrapped", version: "5.0", debug: true})),
      {version: "5.0", debug: true},
    )
    eq(
      __LOC__,
      getWrappedRest(Mirror({name: "mirror", version: "6.0", debug: false})),
      {version: "6.0", debug: false},
    )
  })

  test("inline record variant rest removes the runtime tag field", () => {
    eq(
      __LOC__,
      getInlineWrappedRest(InlineWrap({name: "inline", version: "7.0", debug: true})),
      {version: "7.0", debug: true},
    )
    eq(
      __LOC__,
      getInlineWrappedRest(InlineMirror({name: "inlineMirror", version: "8.0", debug: false})),
      {version: "8.0", debug: false},
    )
  })

  test("inline record variant rest excludes fields renamed with @as", () => {
    eq(
      __LOC__,
      getRenamedInlineWrappedRest(
        RenamedInlineWrap({name: "inlineRenamed", version: "8.5", debug: true}),
      ),
      {version: "8.5", debug: true},
    )
    eq(
      __LOC__,
      getRenamedInlineWrappedRest(
        RenamedInlineMirror({name: "inlineRenamed2", version: "8.6", debug: false}),
      ),
      {version: "8.6", debug: false},
    )
  })

  test("inline record variant rest removes a custom runtime tag field", () => {
    eq(
      __LOC__,
      getCustomTaggedInlineWrappedRest(
        CustomInlineWrap({name: "customInline", version: "9.0", debug: true}),
      ),
      {version: "9.0", debug: true},
    )
    eq(
      __LOC__,
      getCustomTaggedInlineWrappedRest(
        CustomInlineMirror({name: "customInlineMirror", version: "10.0", debug: false}),
      ),
      {version: "10.0", debug: false},
    )
  })

  test("inline record rest works with a non-identifier custom tag name", () => {
    eq(
      __LOC__,
      getDashedTaggedInlineWrappedRest(
        DashedInlineWrap({name: "dashedInline", version: "11.0", debug: true}),
      ),
      {version: "11.0", debug: true},
    )
    eq(
      __LOC__,
      getDashedTaggedInlineWrappedRest(
        DashedInlineMirror({name: "dashedInlineMirror", version: "12.0", debug: false}),
      ),
      {version: "12.0", debug: false},
    )
  })
})
