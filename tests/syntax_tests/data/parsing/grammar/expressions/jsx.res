let _ = <div> </div>
let _ = <div></div>
let _ = <div className="menu"> </div>
// handle weird whitespace
let _ = <   div className   =   "menu"  > <   /    div>
// handle weird whitespace
let _ = < 
  div
className 
  = 
  "menu" 
  >
  <
  / 
  div
>
let _ = <div className="menu"></div>
let _ = <div className="menu" onClick={_ => Js.log("click")}> </div>
let _ = <div className="menu" onClick={_ => Js.log("click")}></div>

let _ = <Navbar />
let _ = <Navbar> </Navbar>
let _ = <Navbar></Navbar>
let _ = <Navbar className="menu"> </Navbar>

let _ = <Dot.Up />
let _ = <Dot.Up> </Dot.Up>
let _ = <Dot.Up></Dot.Up>
let _ = <Dot.Up><Dot.low /></Dot.Up>
let _ = <Dot.Up><Dot.Up /></Dot.Up>
let _ = <Dot.Up className="menu"> </Dot.Up>

let _ = <Dot.low />
let _ = <Dot.low> </Dot.low>
let _ = <Dot.low></Dot.low>
let _ = <Dot.low><Dot.Up /></Dot.low>
let _ = <Dot.low><Dot.low /></Dot.low>
let _ = <Dot.low className="menu"> </Dot.low>

// punning
let _ = <el punned> </el>
let _ = <el ?punned> </el>
let _ = <el punned />
let _ = <el ?punned />

// optional jsx-expr
let _ = <el a=?b> </el>
let _ = <el a=?b />

// fragment
let _ = <> </>
// weird whitespace
let _ = <
  > 
<
  /
  >

// parse nested jsx
let _ =
  <div className="menu">
    <div className="submenu"> sub1 </div>
    <div className="submenu"> sub2 </div>
  </div>
  
// parse nested jsx with weird whitespace
let _ =
  < div className="menu">
    <       div className="submenu"> sub1 <  /   div>
    <     div className="submenu"> sub2 <   /  div>
  <    /    div>

// spread
let _ = <div> ...child </div>
let _ = <Foo> ...(a => 1) </Foo>
let _ = <Foo> ...<Foo2 /> </Foo>
let _ = <Foo> ...[a] </Foo>
let _ = <Foo> ...(1, 2) </Foo>
let _ = <Foo> ...(1, 2)</Foo> // no whitespace between / & </

// children
let _ = <div> ident [1, 2, 3] {call(a, b)} x.y.z </div>

// jsx as prop
let _ = <Outer inner=<Inner /> />

let _ = <div onClick=onClickHandler> <> "foobar" </> </div>


let _ =
   <Window
      style={
        width: 10,
        height: 10,
        paddingTop: 10,
        paddingLeft: 10,
        paddingRight: 10,
        paddingBottom: 10,
      }
  />

let _ = <OverEager fiber=Metal.fiber />


let arrayOfListOfJsx = [<> </>]
let arrayOfListOfJsx = [<> <Foo> </Foo> </>]
let arrayOfListOfJsx = [<> <Foo /> </>, <> <Bar /> </> ]


let sameButWithSpaces = [ <> </>]
let sameButWithSpaces = [ <> <Foo /> </>]
let sameButWithSpaces = [ <> <Foo /> </>, <> <Bar /> </>]
let sameButWithSpaces = [ <> <Foo /> </>, <> <Bar /> </>]

/*
 * Test named tag right next to an open bracket.
 */
let arrayOfJsx = [];
let arrayOfJsx = [<Foo> </Foo>];
let arrayOfJsx = [<Foo />, <Bar /> ]

let sameButWithSpaces = []
let sameButWithSpaces = [<Foo />]
let sameButWithSpaces = [<Foo />, <Bar />]


// doesn't make sense, make sure we have test coverage
let _ = <a /> < <b />
let _ = <a /> > <b />
let _ = <a> </a> < <b> </b>
let _ = <a> </a> > <b> </b>

let y =
  <Routes
    path=Routes.stateToPath(state)
    isHistorical=true
    onHashChange={(_oldPath,_oldUrl,newUrl) =>
      updater((latestComponentBag,_) => {
         let currentActualPath = Routes.hashOfUri(newUrl);
         let pathFromState = Routes.stateToPath(latestComponentBag.state);
         currentActualPath == pathFromState ?
           None : dispatchEventless(State.UriNavigated(currentActualPath),latestComponentBag,())
       },
       ()
     )
   }
  />

let z =
  <div
    style=ReactDOMRe.Style.make(
              ~width,
              ~height,
              ~color,
              ~backgroundColor,
              ~margin,
              ~padding,
              ~border,
              ~borderColor,
              ~someOtherAttribute,
              ())
    key=string_of_int(1)
  />

let omega =
  <div
    aList=list{
      width,
      height,
      color,
      backgroundColor,
      margin,
      padding,
      border,
      borderColor,
      someOtherAttribute
    }
    key=string_of_int(1)
  />

let someArray = <div
 anArray=[width, height, color, backgroundColor, margin, padding, border,borderColor,someOtherAttribute] key=string_of_int(1) />

let tuples =
  <div
    aTuple=(width,
            height,
            color,
            backgroundColor,
            margin,
            padding,
            border,
            borderColor,
            someOtherAttribute,
            definitelyBreakere)
    key=string_of_int(1)
  />

let icon =
  <Icon
    name={switch (state.volume) {
     | v when v < 0.1 => "sound-off"
     | v when v < 0.11 => "sound-min"
     | v when v < 0.51 => "sound-med"
     | _ => "sound-max"
     }
   }
  />

let _ = <MessengerSharedPhotosAlbumViewPhotoReact
  ref=?(
         foo["bar"] === baz ?
           Some(foooooooooooooooooooooooo(setRefChild)) : None
       )
  key=node["legacy_attachment_id"]
/>

/* punning */
let _ = <Foo bar />

/* punning for explicitly passed optional */
let _ = <Foo bar=?bar />

/* don't pun explicitly passed optional with module identifier */
let _ = <Foo bar=?Baz.bar />

let x = <div />

let _ = <div asd=1></div>

foo["bar"] = <bar />
foo #= <bar />
foo #=<bar />

let x =[<div />]


let z = (<div />)

let z = (<Button onClick=handleStaleClick />, <Button onClick=handleStaleClick />)

let y = [<div />, <div />];

let y = [<Button onClick=handleStaleClick />, <Button onClick=handleStaleClick />]

let _ = <Description term={<Text text="Age" />}> child </Description>
let _ = <Description term={Text.createElement(~text="Age", ~children=[], ())}> child </Description>
let _ = <Description term={@JSX Text.createElement(~text="Age", ())}> child </Description>

let _ = <Description term={<Text superLongPunnedProp anotherSuperLongOneCrazyLongThingHere text="Age" />}> child </Description>

let _ = <Foo bar={<Baz superLongPunnedProp anotherSuperLongOneCrazyLongThingHere/>}/>

let _ = <div><span>(str("hello"))</span></div>

let _ =<description term={<text text="Age" />}>child</description>

let _ = <description term={text(~text="Age",~children=[], ())}>child</description>
let _ = <description term={@JSX text(~text="Age",~children=[])}>child</description>
let _ = <description term={@JSX text(~text="Age", ())}>child</description>

let _ = <description term={<div superLongPunnedProp anotherSuperLongOneCrazyLongThingHere text="Age" />}> child </description>

let _ = <div onClick={(event) => handleChange(event)} />
let _ = <div onClick={(eventWithLongIdent) => handleChange(eventWithLongIdent)} />
let _ = <div
  onClick={(event) => {
    Js.log(event)
    handleChange(event)
  }}
/>

let _ = <StaticDiv
  onClick={(foo, bar, baz, lineBreak, identifier) => {
    doStuff(foo, bar, baz);
    bar(lineBreak, identifier);
  }}
/>

let _ = <AttrDiv onClick={@bar (event) => handleChange(event)} />
let _ = <AttrDiv onClick={@bar (eventLongIdentifier) => handleChange(eventLongIdentifier)} />

let _ = <StaticDivNamed
  onClick={(
    ~foo,
    ~bar,
    ~baz,
    ~lineBreak,
    ~identifier,
    ()
  ) =>
    bar(lineBreak, identifier)
  }
/>

let _ = <div
  onClick={(e): event => {
    doStuff()
    bar(foo)
  }}
/>

let _ = <div
  onClick={(e, e2): event => {
    doStuff()
    bar(foo)
  }}
/>

let _ = <div
  onClick={(
    foo,
    bar,
    baz,
    superLongIdent,
    breakLine,
  ): (
    event,
    event2,
    event3,
    event4,
    event5
  ) => {
    doStuff()
    bar(foo)
  }}
/>

let _ = <div
  onClick={(
    foo,
    bar,
    baz,
    superLongIdent,
    breakLine,
  ): (
    event,
    event2,
    event3,
    event4,
    event5
  ) =>
    doStuff()
  }
/>;

let _ = <div>
  {switch(color) {
    | Black => ReasonReact.string("black")
    | Red => ReasonReact.string("red")
    }}
</div>

let _ = <div
 style={
   @foo
   ReactDOMRe.Style.make(
     ~width="20px",
     ~height="20px",
     ~borderRadius="100%",
     ~backgroundColor="red",
   )
 }
/>

let _ = <Animated initialValue=0.0 value>
  ...{
       ReactDOMRe.Style.make(
         ~width="20px",
         ~height="20px",
         ~borderRadius="100%",
         ~backgroundColor="red",
       )
     }
</Animated>

let _ = <Animated initialValue=0.0 value>
  ...{ value =>
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

let _ = <Animated initialValue=0.0 value>
  ...{(value) :ReasonReact.element =>
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

let _ = <Animated initialValue=0.0 value>
  ...{@foo value => {
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
 }
</Animated>

let _ = <Animated initialValue=0.0 value>
  ...{value => {
       let width = "20px";
       // TODO: check semi
       let height = "20px";

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

let _ = <div callback={reduce(() => !state)} />

let _ = <button ?id className={Cn.make(["button", "is-fullwidth"])} onClick>
  {"Submit"->ste}
</button>

let _ = <button ?id className={Cn.make(list{"button", "is-fullwidth"})} onClick>
  {"Submit"->ste}
</button>

let _ = <button ?id className={Cn.make(("button", "is-fullwidth"))} onClick>
  {"Submit"->ste}
</button>

let _ = <button ?id className={Cn.make({a: b})} onClick>
  {"Submit"->ste}
</button>

let _ = <X y={z->Belt.Option.getWithDefault("")} />

let _ = <div style={getStyle()}> {ReasonReact.string("BugTest")} </div>;

let _ = <div>
  {
    let left = limit->Int.toString
    `${left} characters left`->React.string
  }
</div>

let _ = <View style=styles["backgroundImageWrapper"]>
  {
    // TODO: semicolon
    let uri = "/images/header-background.png";
    <Image
      resizeMode=Contain
      style=styles["backgroundImage"]
      uri
    />
  }
</View>

<div>
  {possibleGradeValues
  ->List.filter(g => g <= state.maxGrade)
  ->List.map(possibleGradeValue =>
      <option
        key={possibleGradeValue->string_of_int}
        value={possibleGradeValue->string_of_int}>
        {possibleGradeValue->string_of_int->str}
      </option>
    )
  ->Array.of_list
  ->ReasonReact.array}
</div>;

// https://github.com/rescript-lang/syntax/issues/113
// <= should be scanned as <=
<div>  {Js.log(a <= 10)} </div>
<div> <div> {Js.log(a <= 10)} </div> </div>
<div> <div onClick={_ => Js.log(a <= 10) }> <div> {Js.log(a <= 10)} </div> </div> </div>


<div> ...element </div>
<div> ...{(a) => 1} </div>
<div> ...<span /> </div>
<div> ...[a] </div>
<div> ...(1, 2) </div>
<div> ...{array->f} </div>

<> ...element </>
<> ...{(a) => 1} </>
<> ...<span /> </>
<> ...[a] </>
<> ...(1, 2) </>
<> ...{array->f} </>

let _ = <A x="y" {...str} />

let _ = <p-x-y></p-x-y>
let _ = <V.x-y> </V.x-y>
let _ = <a-b-c />
let _ = <V.x-y-z></V.x-y-z>
