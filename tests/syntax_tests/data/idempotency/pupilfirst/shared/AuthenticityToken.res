exception CSRFTokenMissing
exception CSRFTokenEmpty

open Webapi.Dom

let fromHead = () => {
  let metaTag = document->Document.querySelector("meta[name='csrf-token']")

  switch metaTag {
  | None => raise(CSRFTokenMissing)
  | Some(tag) =>
    switch tag->Element.getAttribute("content") {
    | None => raise(CSRFTokenEmpty)
    | Some(token) => token
    }
  }
}
