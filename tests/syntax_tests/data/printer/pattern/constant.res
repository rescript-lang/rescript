let 1 = 1
let "string" = 1
let 3.14 = 1

let print = (ppf, i) =>
  switch i.stamp {
  | 0 => fprintf(ppf, "%s!", i.name)
  | -1 => fprintf(ppf, "%s#", i.name)
  | +1 => fprintf(ppf, "%s#", i.name)
  | -1. => fprintf(ppf, "%s#", i.name)
  | +1. => fprintf(ppf, "%s#", i.name)
  }

let [-1, +1, 1] = x

switch science {
| (1.12, -3.13) => true
| [1.12, -3.13] => true
| list{1.12, -3.13} => true
| {x: 1.12, y: -3.13} => true
| Constructor(1.12, -2.45) => true
| #Constuctor(1.12, -2.45) => true
| -4.15 as x => true
| -4.15 | +4.15 => true
| (-3.14 : float) => true
| exception 19.34 => true
| _ => false
}

<div>
  {switch state.uploadProgress {
    | None => React.null
    | Some(1.0) => React.null
    | Some(uploadProgress) =>
      <div className=Styles.fill>
        <StudioUploadProgressIndicator progress=uploadProgress />
      </div>
    }}
</div>

switch literal {
| `literal` => true
| (`literal1`, `literal2`) => true
| [`literal1`, `literal2`] => true
| list{`literal1`, `literal2`} => true
| {x: `literal1`, y: `literal2`} => true
| Constructor(`literal1`, `literal2`) => true
| #Constuctor(`literal1`, `literal2`) => true
| `literal` as x => true
| `literal` | `literal` => true
| (`literal` : string) => true
| exception `literal` => true
| _ => false
}

let `literal constant` = x

for `literal constant` in 0 to 10 { () }
