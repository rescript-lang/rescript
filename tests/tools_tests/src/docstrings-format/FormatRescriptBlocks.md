# Format ReScript code blocks

This markdown file should be formatted.

This is the first docstring with unformatted ReScript code.

```rescript
let badly_formatted=(x,y)=>{
let result=x+y
if result>0{Console.log("positive")}else{Console.log("negative")}
result
}
```

And another code block in the same docstring:

```res
type user={name:string,age:int,active:bool}
let createUser=(name,age)=>{name:name,age:age,active:true}
```
