let x = <div className="container" className2="container2" className3="container3" onClick />
let x = <Foo className="container" />
let x = <Foo.Bar className="container" />
let x = <Foo.Bar.Baz className="container" />
let x = <Foo.bar className="container" />
let x = <Foo.baz
 className="multiline"
/>
let x = <custom-tag className="container" />
let x = <Foo.custom-tag className="container" />

// https://github.com/rescript-lang/syntax/issues/570
let x = <A> <B> <C> <D /> <E /> </C> <F> <G /> <H /> </F> </B> </A>
let x = <A> {children} <B/> </A>
let x = <A> <B/> {children} </A>
let x = <A> {a} </A>
let x = <A> {a} {b} </A>
let x = <custom-tag className="container" > {a} <B/> </custom-tag>
let x = <Foo.custom-tag className="container" > {a} <B/> </Foo.custom-tag>

let x =
  <div
  className="container" className2="container2" className3="container3" onClick
  />


let nav = <Nav isMobile={isMobile} fullScreen={!isMobile ? false : isOpen}>
      <NavContent>
        <WidescreenMenu
          menuItems={props.items}
          trial={props.trial}
          user={props.user}
          viewer={props.viewer}
        />
      </NavContent>
    </Nav>

let nav2 =
  <Nav isMobile={isMobile} fullScreen={!isMobile ? false : isOpen}>
    <NavContent>
      {switch isMobile {
      | true =>  (
        <MobileMenu
          handleOpenToggle={_ => setOpen(open_ => !open_)}
          menuItems={props.items}
          isOpen={isOpen}
          forceOpen={_ => setOpen(true)}
          forceClose={_ => setOpen(false)}
          user={props.user}
        />
      ) 
      | false => (
        <WidescreenMenu
          menuItems={props.items}
          trial={props.trial}
          user={props.user}
          viewer={props.viewer}
        />
      )}}
    </NavContent>
  </Nav>

let nav3 = (
  <Nav isMobile={isMobile} fullScreen={!isMobile ? false : isOpen}>
    <NavContent>
      {isMobile ? (
        <MobileMenu
          handleOpenToggle={_ => setOpen(open_ => !open_)}
          menuItems={props.items}
          isOpen={isOpen}
          forceOpen={_ => setOpen(true)}
          forceClose={_ => setOpen(false)}
          user={props.user}
        />
      ) : (
        <WidescreenMenu
          menuItems={props.items}
          trial={props.trial}
          user={props.user}
          viewer={props.viewer}
        />
      )}
    </NavContent>
  </Nav>
)

let avatarSection = (
    <>
      <div style={{"zIndex": "1", "opacity": opacityUser}}>
        <Avatar user={user} size={45} />
      </div>
      {user.email !== viewer.email ? (
        <div
          style={{
            "marginLeft": marginLeft + "em",
            "opacity": opacityAdmin,
            "zIndex": "0",
            "cursor": "pointer"
          }}
          onMouseEnter={_ => setHoveringAdmin(true)}
          onMouseLeave={_ => setHoveringAdmin(false)}
          onClick={_e => {
            stopImpersonating(csrfToken);
          }}
        >
          <Avatar user={viewer} size={45} />
        </div>
      ) : React.nullElement}
    </>
  );

let x = <> </>;

let x =
  <div
    ident // punned
    ?ident // punned optional
    ident=?identifierExpr 
    ident=identifierExpr
    constant="constant"
    letExpr={
      let a = 1
      let b = 2
      a + b
    }
    onClick={event => handleClick(event)}
    onClick={(event1, event2, event3) => {
      Js.log("debug click")
      handleClick(event)
    }}
    onClick={(aaaaaaaaaaaaaaarg1, aaaaaaaaaaaaaaaaaaaaaaarg2, aaaaaaaaaaaaaaarg3) => {
      sideEffect(aaaaaaaaaaaaaaarg1)
      sideEffect(aaaaaaaaaaaaaaaaaaaaaaarg2)
      sideEffect(aaaaaaaaaaaaaaarg3)
    }}
    apply={myFunction(x)}
    applyWithDot={myFunction(. x)}
    street={streets[0]}
    binary={a + b}
    int={-1}
    float={-1.2}
    matchExpression={switch color {
      | Blue => "blue"
      | Red => "red"
    }}
    tryExpression={try sideEffect() catch {
      | Error => ()
    }}
    tuple=(a, b, c)
    constr=Rgb(red, blue, green)
    list1=list{}
    list2=list{a, b}
    list3=list{a, b, ...x}
    array=[a, b, c]
    record={x: 1, y: 2}
    field=foo.bar
    setField={user.name = "Steve"}
    ifThenElse={if true {
      ()
    } else {
      ()
    }}
    seq={{sideEffect1(); sideEffect2()}}
    whileExpr={while true {
      loop()
    }}
    forExpr={for i in 0 to 10 {
      do(i)
    }}
    constraintExpr={(20: int)}
    letModule={{
      module L = Log;
      L.log()
    }}
    letException={{
      exception Exit;
      raise(Exit)
    }}
    assertExpr={assert(true)}
    pack=module(Foo)
    pack={module(Foo)}
    pack=module(Foo: Bar)
    pack={module(Foo: Bar)}
    openExpr={open React; React.render()}
    extension=%raw("eval()")
    jsObject={"x": 1, "y": 2} 
    withAttr={@attr foo}
 />

  let x = <div>
    ident "constant"
    {
      let a = 1
      let b = 2
      a + b
    }
    {event => handleClick(event)}
    {(event1, event2, event3) => {
      Js.log("debug click")
      handleClick(event)
    }}
    {(aaaaaaaaaaaaaaarg1, aaaaaaaaaaaaaaaaaaaaaaarg2, aaaaaaaaaaaaaaarg3) => {
      sideEffect(aaaaaaaaaaaaaaarg1)
      sideEffect(aaaaaaaaaaaaaaaaaaaaaaarg2)
      sideEffect(aaaaaaaaaaaaaaarg3)
    }}
    {myFunction(x)}
    {myFunction(. x)}
    {streets[0]}
    {a + b}
    {-1}
    {-1.2}
    {switch color {
      | Blue => "blue"
      | Red => "red"
    }}
    {try sideEffect() catch {
      | Error => ()
    }}
    {(a, b, c)}
    Rgb(red, blue, green)
    list{}
    list{a, b}
    list{a, b, ...x}
    [a, b, c]
    {x: 1, y: 2}
    foo.bar
    {user.name = "Steve"}
    {if true {
      ()
    } else {
      ()
    }}
    {{sideEffect1(); sideEffect2()}}
    {while true {
      loop()
    }}
    {for i in 0 to 10 {
      do(i)
    }}
    {(20: int)}
    {{
      module L = Log;
      L.log()
    }}
    {{
      exception Exit;
      raise(Exit)
    }}
    {assert(true)}
    {module(Foo)}
    module(Foo)
    {module(Foo: Bar)}
    module(Foo: Bar)
    {open React; React.render()}
    %raw("eval()")
    {"x": 1, "y": 2} 
    {@attr ident} 
  </div>

let x = <MyComponent sidebar=<div> test </div> nav=<Navbar /> />

<div>
  {possibleGradeValues
  |> List.filter(g => g <= state.maxGrade)
  |> List.map(possibleGradeValue =>
      <option
        key={possibleGradeValue |> string_of_int}
        value={possibleGradeValue |> string_of_int}>
        {possibleGradeValue |> string_of_int |> str}
      </option>
    )
  |> Array.of_list
  |> ReasonReact.array}
</div>

// https://github.com/rescript-lang/syntax/issues/113
<div>  {Js.log(a <= 10)} </div>
<div> <div> {Js.log(a <= 10)} </div> </div>
<div> <div onClick={_ => Js.log(a <= 10) }> <div> {Js.log(a <= 10)} </div> </div> </div>

module App = {
  @react.component
  let make = () => {
    <> 1 </>
  }
}

module App = {
  @react.component
  let make = () => {
    <> <div /> </>
  }
}

// leading line comment on rhs jsx prop should indent nicely
<ContributorsRow
  amount={
    // ->BN.new_("86400")
    // There are 86400 seconds in a day.
    amount->BN.div(BN.new_("86400"))->BN.toString
  }
/>

// leading line comment on rhs jsx prop should indent nicely
<ContributorsRow
  amount={
    // There are 86400 seconds in a day.
    compute(amount)
  }
/>

// leading line comment on braced children should indent nicely
<div>
  {// comment
    content
  }

  { // comment
    if condition() {
      renderBranch()
    } else {
      doSomeOtherThings()
    }
  }
</div>

<Animated> ...{x => <div />} </Animated>
<div>...c</div>;

<Animated initialValue=0.0 value>
  ...{
       ReactDOMRe.Style.make(
         ~width="20px",
         ~height="20px",
         ~borderRadius="100%",
         ~backgroundColor="red",
       )
     }
</Animated>;

<Animated initialValue=0.0 value>
  ...{
       value =>
         <div
           style={
             ReactDOMRe.Style.make(
               ~width="20px",
               ~height="20px",
               ~borderRadius="100%",
               ~backgroundColor="red",
             )
           }
         />
     }
</Animated>

<Animated initialValue=0.0 value>
  ...{
     (value) :ReasonReact.element =>
         <div
           style={
             ReactDOMRe.Style.make(
               ~width="20px",
               ~height="20px",
               ~borderRadius="100%",
               ~backgroundColor="red",
             )
           }
         />
     }
</Animated>;

<Animated initialValue=0.0 value>
  ...{value => {
       let width = "20px"
       let height = "20px"

       <div
         style={
           ReactDOMRe.Style.make(
             ~width,
             ~height,
             ~borderRadius="100%",
             ~backgroundColor="red",
           )
         }
       />
     }
  }
</Animated>


let v =
  <A>
    <B>
      ...{_ => {
        let renderX = x => {
          let y = x ++ x;
          <div key=y />;
        }
        renderX("foo")
      }}
    </B>
  </A>

let x = <A x="y" {...str} />

// https://github.com/rescript-lang/rescript-compiler/issues/6002
let x = props =>
  <A
    {...props}
    className="inline-block px-6 py-2.5 bg-blue-600 text-white font-medium text-xs leading-tight"
  />

let x = <C> ...{() => msg->React.string} </C>

let x = <C> ...{array->Array.map(React.string)} </C>

let x = <> ...{array->Array.map(React.string)} </>

let x = {
  let _ = <div />
  msg->React.string
}

let x = {
  let _ = <div> {children} </div>
  msg->React.string
}

let x = {
  let _ = <> {children} </>
  msg->React.string
}

let x = {
  let _ = <C />
  msg->React.string
}

let x = {
  let _ = <C> {children} </C>
  msg->React.string
}

let x =
  <div>
    {
      hello()
      hello()
    } 
    {
      world()
      world()
    }

    {
      hello()
      hello()
    } {
      world()
      world()
    }
    // another test
    <span
      id="1"
      className="sdf sdfdsf sdfs sdf asdf dsf"
      onClick={() => {
        ()
      }}
    />

    // Comment 2

    <span
      id="2"
      className="sdf sdfdsf sdfs sdf asdf dsf"
      onClick={() => {
        ()
      }}
    >
      <span
        id="2-1"
        className="sdf sdfdsf sdfs sdf asdf dsf"
        onClick={() => {
          ()
        }}
      />


      // Comment
      <span
        id="2-2"
        className="sdf sdfdsf sdfs sdf asdf dsf"
        onClick={() => {
          ()
        }}
      />

      <span id="2-3" 
      />
    </span>
    <span
      id="3"
      className="sdf sdfdsf sdfs sdf asdf dsf"
      onClick={() => {
        ()
      }}
    />
  </div>
