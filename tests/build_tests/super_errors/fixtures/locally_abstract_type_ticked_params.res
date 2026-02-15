// Repro for rescript-lang/rescript#7850
// Using locally abstract types with ticked params inside the annotation
// previously threw an uncaught Syntaxerr.Error. This fixture ensures we
// surface a proper diagnostic instead.

type event<'inputStream,'callback> =
  | Pipe
  | Data
  | End

let rec patternMatching : type inputStream callback. (ev: event<'inputStream, 'callback>) => unit {
  switch ev {
  | Pipe => patternMatching(Data)
  | Data => patternMatching(End)
  | End => ()
  }
}

