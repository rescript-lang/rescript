module ReScript = {
  @react.component
  let make = () => React.null
}


let getIconForLanguageExtension = (language: string) => {
  switch language {
  | "res" | "rescript" => <ReScript  />
  | _ => React.null
  }
}
