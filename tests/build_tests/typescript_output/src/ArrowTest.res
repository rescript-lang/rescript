let isDev = true

let loadStaticFiles: unit => promise<array<string>> = %raw(`() => Promise.resolve(["public/foo.js", "dist/bar.js"])`)

let test = async () => {
  let files = await loadStaticFiles()
  let files =
    files
    ->Array.map(f => {
      (
        switch isDev {
        | true if f->String.startsWith("public/") => f->String.slice(~start=7)
        | false if f->String.startsWith("dist/") => f->String.slice(~start=5)
        | _ => f
        },
        f,
      )
    })
    ->Map.fromArray
  files
}
