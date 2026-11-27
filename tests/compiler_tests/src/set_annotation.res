type student = {@set "age": int, @set "name": string}
@module("MyJSFile") external student1: student = "student1"

student1["name"] = "Mary"
