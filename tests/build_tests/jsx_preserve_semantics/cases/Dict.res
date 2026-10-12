// Props that are a dict, spread as one object whatever its keys
let baseProps: JsxDOM.domProps = {title: "foo", className: "foo"}
let otherProps: JsxDOM.domProps = {id: "bar"}

let twoSpreads = <input {...dict{...baseProps->Obj.magic, ...otherProps->Obj.magic}->Obj.magic} />
let spacedKey = <input {...dict{...baseProps->Obj.magic, "foo bar": "x"}->Obj.magic} />
let protoKey = <input {...dict{...baseProps->Obj.magic, "__proto__": "x"}->Obj.magic} />
let hyphenatedKey = <input {...dict{...baseProps->Obj.magic, "aria-label": "x"}->Obj.magic} />
let repeatedChildren =
  <div
    {...dict{
      ...baseProps->Obj.magic,
      "children": React.string("first"),
        "children": React.string("second"),
    }->Obj.magic}
  />
let repeatedKey =
  <input {...dict{...baseProps->Obj.magic, "title": "x", "title": "y"}->Obj.magic} />
let keyProp = <input {...dict{...baseProps->Obj.magic, "key": "inner"}->Obj.magic} key="outer" />
let keyPropAfter =
  <input key="outer" {...dict{...baseProps->Obj.magic, "key": "inner"}->Obj.magic} />
