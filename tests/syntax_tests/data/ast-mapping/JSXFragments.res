let empty = <></>

let fragmentWithBracedExpresssion = <>{React.int(1 + 2)}</>

let fragmentWithJSXElements = <>
    <h1>{React.string("Hi")}</h1>
    <p>{React.string("Hello")}</p>
</>

let nestedFragments = <>
    <h1>{React.string("Hi")}</h1>
    <p>{React.string("Hello")}</p>
    <>{React.string("Bye")}</>
</>