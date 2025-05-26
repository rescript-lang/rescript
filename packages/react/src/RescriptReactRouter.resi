@ocaml.doc(
  "update the url with the string path. Example: `push(\"/book/1\")`, `push(\"/books#title\")` "
)
let push: string => unit

@ocaml.doc(
  "update the url with the string path. modifies the current history entry instead of creating a new one. Example: `replace(\"/book/1\")`, `replace(\"/books#title\")` "
)
let replace: string => unit
type watcherID
type url = {
  /* path takes window.location.path, like "/book/title/edit" and turns it into `["book", "title", "edit"]` */
  path: list<string>,
  /* the url's hash, if any. The # symbol is stripped out for you */
  hash: string,
  /* the url's query params, if any. The ? symbol is stripped out for you */
  search: string,
}

@ocaml.doc(
  "start watching for URL changes. Returns a subscription token. Upon url change, calls the callback and passes it the url record "
)
let watchUrl: (url => unit) => watcherID

@ocaml.doc(" stop watching for URL changes ")
let unwatchUrl: watcherID => unit

@ocaml.doc("this is marked as \"dangerous\" because you technically shouldn't
      be accessing the URL outside of watchUrl's callback; you'd read a potentially
      stale url, instead of the fresh one inside watchUrl.

      But this helper is sometimes needed, if you'd like to initialize a page
      whose display/state depends on the URL, instead of reading from it in
      watchUrl's callback, which you'd probably have put inside didMount (aka
      too late, the page's already rendered).

      So, the correct (and idiomatic) usage of this helper is to only use it in
      a component that's also subscribed to watchUrl. Please see
      https://github.com/reasonml-community/reason-react-example/blob/master/src/todomvc/TodoItem.re
      for an example.")
let dangerouslyGetInitialUrl: (~serverUrlString: string=?, unit) => url

@ocaml.doc("hook for watching url changes.
 * serverUrl is used for ssr. it allows you to specify the url without relying on browser apis existing/working as expected
 ")
let useUrl: (~serverUrl: url=?, unit) => url
