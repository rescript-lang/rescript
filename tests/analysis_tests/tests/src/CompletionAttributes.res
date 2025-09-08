// @modu
//      ^com

// @module("") external doStuff: t = "test"
//          ^com

// @@js
//     ^com

// @@jsxConfig({})
//              ^com

// @@jsxConfig({m})
//               ^com

// @@jsxConfig({module_: })
//                       ^com

// @@jsxConfig({module_: "", })
//                           ^com

// @module({}) external doStuff: t = "default"
//          ^com

// @module({with: }) external doStuff: t = "default"
//               ^com

// @module({with: {}}) external doStuff: t = "default"
//                 ^com

// @module({from: "" }) external doStuff: t = "default"
//                 ^com

// @module({from: }) external doStuff: t = "default"
//               ^com

// let dd = %t
//            ^com

