let emptyUnary = <input />

let emptyNonunary = <div></div>

let emptyUnaryWithAttributes = <input type_="text" />

let emptyNonunaryWithAttributes = <div className="container"></div>

let elementWithChildren = <div>
    <h1>{React.string("Hi")}</h1>
    <p>{React.string("Hello")}</p>
</div>

let elementWithChildrenAndAttributes = <div className="container">
    <h1>{React.string("Hi")}</h1>
    <p>{React.string("Hello")}</p>
</div>

let elementWithConditionalChildren = <div>
    {if true {
        <h1>{React.string("Hi")}</h1>
    } else {
        React.null
    }}
</div>