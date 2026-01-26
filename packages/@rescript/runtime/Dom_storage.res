type t = Dom_storage2.t

@send @return(null_to_opt) external getItem: (t, string) => option<string> = "getItem"
let getItem = (s, obj) => obj->getItem(s)
/* https://developer.mozilla.org/en-US/docs/Web/API/Storage/getItem 
  If the key does not exist, `null` is returned
*/

@send external setItem: (t, string, string) => unit = "setItem"
let setItem = (k, v, obj): unit => obj->setItem(k, v)
@send external removeItem: (t, string) => unit = "removeItem"
let removeItem = (s, obj): unit => obj->removeItem(s)
@send external clear: t => unit = "clear"
@send @return(null_to_opt) external key: (t, int) => option<string> = "key"
/* A DOMString containing the name of the key. If the index does not exist, null is returned.
  If the key does not exist, `null` is returned
*/
let key = (i, obj): option<string> => obj->key(i)
@get external length: t => int = "length"

@val external localStorage: t = "localStorage"
@val external sessionStorage: t = "sessionStorage"
