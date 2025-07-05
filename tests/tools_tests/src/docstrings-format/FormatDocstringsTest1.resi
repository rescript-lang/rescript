/**
This is the first docstring with unformatted ReScript code.

```rescript
let badly_formatted=(x,y)=>{
let result=x+y
if result>0{Console.log("positive")}else{Console.log("negative")}
result
}
```

And another code block in the same docstring:

```rescript
type user={name:string,age:int,active:bool}
let createUser=(name,age)=>{name:name,age:age,active:true}
```
*/
let testFunction1: unit => string

module Nested: {
  /**
  This is a second docstring with different formatting issues.

  But if I add another line here it should be fine.

  ```rescript
  module UserService={
    let validate=user => user.age>=18 && user.name !== ""
    let getName = user=>user.name
  }
  ```  
*/
  let testFunction2: unit => string
}

/**
Third docstring with array and option types.

```rescript
let processUsers=(users:array<user>)=>{
users
->Array.map(user=>{...user,active:false})->Array.filter(u=>u.age>21)
}

type status=|Loading|Success(string)|Error(option<string>)
```
*/
let testFunction3: unit => string
