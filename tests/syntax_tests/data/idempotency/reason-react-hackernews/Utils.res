/* require css file for side effect only */
@val external requireCSS: string => unit = "require"

/* require an asset (eg. an image) and return exported string value (image URI) */
@val external requireAssetURI: string => string = "require"

@val external currentTime: unit => int = "Date.now"

/* format a timestamp in seconds as relative humanised time sentence */
let fromNow = unixtime => {
  let delta = currentTime() / 1000 - unixtime
  if delta < 3600 {
    string_of_int(delta / 60) ++ " minutes ago"
  } else if delta < 86400 {
    string_of_int(delta / 3600) ++ " hours ago"
  } else {
    string_of_int(delta / 86400) ++ " days ago"
  }
}

@send @return(nullable)
external getAttribute: ('a, string) => option<string> = "getAttribute"

let dangerousHtml: string => 'a = html => {"__html": html}

let distanceFromBottom: unit => int = () => {
  let bodyClientHeight = %raw("document.body.clientHeight")
  let windowScrollY = %raw("window.scrollY")
  let windowInnerHeight = %raw("window.innerHeight")
  bodyClientHeight - (windowScrollY + windowInnerHeight)
}

@module
external registerServiceWorker: unit => unit = "src/registerServiceWorker"
