(* Copyright (C) 2019-Present Hongbo Zhang, Authors of ReScript
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)

module STbl = struct
  #if OCAML_VERSION >= (5, 0, 0)
    include Hashtbl.Make (String)
  #else
    module StringHash : Hashtbl.HashedType with type t = string = struct
      type t = string
      let equal = String.equal
      let hash = Hashtbl.hash (* polymorphic hash function *)
    end
    include Hashtbl.Make (StringHash)
  #endif

  let of_array arr =
    let tbl = create (Array.length arr) in
    let () = Array.iter (fun el -> add tbl el ()) arr in
    tbl
end

(** Words that can never be identifier's name.

    See https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Lexical_grammar#reserved_words
 *)
let js_keywords = STbl.of_array [|
  "break";
  "case";
  "catch";
  "class";
  "const";
  "continue";
  "debugger";
  "default";
  "delete";
  "do";
  "else";
  "export";
  "extends";
  "false";
  "finally";
  "for";
  "function";
  "if";
  "import";
  "in";
  "instanceof";
  "new";
  "null";
  "return";
  "super";
  "switch";
  "this";
  "throw";
  "true";
  "try";
  "typeof";
  "var";
  "void";
  "while";
  "with";

  (* The following are also reserved in strict context, including ESM *)
  "let";
  "static";
  "yield";

  (* `await` is reserved in async context, including ESM *)
  "await";

  (* Future reserved words *)
  "enum";
  "implements";
  "interface";
  "package";
  "private";
  "protected";
  "public";

  (* Special identifiers

     `arguments` and `eval` is not real *keywords*

     However, they cannot be as identifiers in the strict mode,
     and the compiler output is always in strict mode.

     See https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Lexical_grammar#identifiers_with_special_meanings
   *)
  "arguments";
  "eval";
|]

let is_js_keyword s = STbl.mem js_keywords s

(** Identifiers with special meanings.

    They can have different meanings depending on the context when used as identifier names, so it should be done carefully.

    See https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Lexical_grammar#identifiers_with_special_meanings

    However, these names are actually used with no problems today. (Except `arguments` and `eval`)
 *)
let js_special_words = STbl.of_array [|
  "arguments";
  "as";
  "async";
  "eval";
  "from";
  "get";
  "of";
  "set";
|]

let is_js_special_word s = STbl.mem js_special_words s

(** Identifier names _might_ need to care about *)
let js_globals = STbl.of_array [|
  (* JavaScript standards built-ins
     See https://developer.mozilla.org/ko/docs/Web/JavaScript/Reference/Global_Objects
  *)
  "AggregateError";
  "Array";
  "ArrayBuffer";
  "AsyncFunction";
  "AsyncGenerator";
  "AsyncGeneratorFunction";
  "AsyncIterator";
  "Atomics";
  "BigInt";
  "BigInt64Array";
  "BigUint64Array";
  "Boolean";
  "DataView";
  "Date";
  "decodeURI";
  "decodeURIComponent";
  "encodeURI";
  "encodeURIComponent";
  "Error";
  "eval";
  "EvalError";
  "FinalizationRegistry";
  "Float16Array";
  "Float32Array";
  "Float64Array";
  "Function";
  "Generator";
  "GeneratorFunction";
  "globalThis";
  "Infinity";
  "Int16Array";
  "Int32Array";
  "Int8Array";
  "Intl";
  "isFinite";
  "isNaN";
  "Iterator";
  "JSON";
  "Map";
  "Math";
  "NaN";
  "Number";
  "Object";
  "parseFloat";
  "parseInt";
  "Promise";
  "Proxy";
  "RangeError";
  "ReferenceError";
  "Reflect";
  "RegExp";
  "Set";
  "SharedArrayBuffer";
  "String";
  "Symbol";
  "SyntaxError";
  "TypedArray";
  "TypeError";
  "Uint16Array";
  "Uint32Array";
  "Uint8Array";
  "Uint8ClampedArray";
  "undefined";
  "URIError";
  "WeakMap";
  "WeakRef";
  "WeakSet";

  (* A few of the HTML standard globals
  
     See https://developer.mozilla.org/en-US/docs/Web/API/Window
     See https://developer.mozilla.org/en-US/docs/Web/API/WorkerGlobalScope
    
     But we don't actually need to protect these names.
   
  "window";
  "self";
  "document";
  "location";
  "navigator";
  "origin";
  *)

  (* A few of the Node.js globals
  
     Specifically related to the CommonJS module system
     They cannot be redeclared in nested scope.
  *)
  "__dirname";
  "__filename";
  "require";
  "module";
  "exports";

  (* Bun's global namespace *)
  "Bun";

  (* Deno's global namespace *)
  "Deno";
|]

let is_js_global s = STbl.mem js_globals s
