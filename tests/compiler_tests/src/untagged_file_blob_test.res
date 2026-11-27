open Mocha
open Test_utils

@new external makeBlob: array<string> => Blob.t = "Blob"
@new external makeFile: (array<string>, string) => File.t = "File"

// Blob is declared before its subclass File (#8938)
@unboxed
type upload = Blob(Blob.t) | File(File.t)

let kind = upload =>
  switch upload {
  | Blob(_) => "blob"
  | File(_) => "file"
  }

let onlyBlob = upload =>
  switch upload {
  | Blob(_) => "blob"
  | _ => "other"
  }

describe(__MODULE__, () => {
  test("a file is not taken for a blob", () => {
    eq(__LOC__, "blob", kind(Blob(makeBlob(["x"]))))
    eq(__LOC__, "file", kind(File(makeFile(["x"], "a.txt"))))
  })

  test("a blob case alone excludes files", () => {
    eq(__LOC__, "blob", onlyBlob(Blob(makeBlob(["x"]))))
    eq(__LOC__, "other", onlyBlob(File(makeFile(["x"], "a.txt"))))
  })

  test("a file upcasts to a blob", () => {
    let blob: Blob.t = (makeFile(["x"], "a.txt") :> Blob.t)
    eq(__LOC__, "file", kind(Obj.magic(blob)))
  })
})
