type polyvariant = [#Error(string) | #Valid]

let f: polyvariant = #Valid

switch f {
| #Invalid => ()
}
