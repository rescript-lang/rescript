


Its essential is 

```ocaml
external destruct : 'b -> (exn -> 'a) 
```

However it does not prevent things like 

```ocaml
destruct v begin fun exn -> 
   Js.log exn ; 
   match exn with 
   | .. 
   | .. 
```

Here it forces us to answer whether `v` is exception or not, 

while such syntax below does not need us answer `v` is exception 
or not,  it just asks us to answer it matches a branch of exception or not which can be done in a sound way.

```ocaml
match%exn v with 
| .. 
| .. 
```

However, we need make sure such cases not happen

```ocaml
match%exn v with 
| e -> ...

```
Or any vagous pattern which needs us to answer if 
it is an exception or not


Another proposal is 
```ocaml
match%exn computation with 
| A ..
| B .. 
| Js.NonCamlOpenVariant .. 
| v ->  ..
```

Here we pack the data `v` 

==>
```

match (Js_enx.internalTOOCamlException compuation) with
| A .. 
| B 
| exception .. )
```

The same problem is

```
match (Js_enx.internalTOOCamlException compuation) with
| _ -> ..
```

What will happen if JS side `raises` an OCaml extensible variant, 
we view it as OCaml exception..
It is slightly differnt in OCaml, since it always start from `catch(id)..` 
where `id` is defined by the compiler

Another very similar proposal would be 

```ocaml
fun[@bs:exn] e -> 
  match e with 
  | Js.Exn.Error ..
  | .. 
```