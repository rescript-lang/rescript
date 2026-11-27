// An extra constructor at the front is reported as extra, and a constructor
// each declaration lacks at the same position as a renaming
module M: {
  type t = Red | Green | Blue | Gray
} = {
  type t = Black | Red | Green | Blue | Grey
}
