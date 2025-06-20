exception CSRFTokenMissing
exception CSRFTokenEmpty

open Webapi.Dom

let fromHead = () => {
  let metaTag = document->Document.querySelector("meta[name='csrf-token']")

  switch metaTag {
  | None => throw(CSRFTokenMissing)
  | Some(tag) =>
    switch tag->Element.getAttribute("content") {
    | None => throw(CSRFTokenEmpty)
    | Some(token) => token
    }
  }
}
