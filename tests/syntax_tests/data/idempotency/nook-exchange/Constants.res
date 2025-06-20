@val @scope(("process", "env"))
external nodeEnv: option<string> = "NODE_ENV"
@val @scope(("process", "env"))
external gitCommitRef: option<string> = "COMMIT_REF"
let gitCommitRef = Belt.Option.getWithDefault(gitCommitRef, "")->Js.String.slice(~from=0, ~to_=8)

let apiUrl =
  nodeEnv === Some("paul-development") ? "http://localhost:3000" : "https://a.nook.exchange"
let cdnUrl =
  nodeEnv === Some("paul-development") ? "" : "https://b.nook.exchange/file/nook-exchange"

let gtagId = "UA-55966633-6"
let amplitudeApiKey = "d91231e0b8a96baf38ba67eb36d25a48"
let sentryId = "https://b3e870897abf4c5caef3e12320202dee@o378523.ingest.sentry.io/5202020"

exception Uhoh

let headerHeight = 52
@val external encodeURIComponent: string => string = "encodeURIComponent"
let discordOauthRedirectUri = state =>
  "https://discord.com/api/oauth2/authorize?client_id=703109829610176522&redirect_uri=" ++
  (encodeURIComponent(
    {
      open Webapi.Dom
      location->Location.origin
    } ++ "/discord_oauth2",
  ) ++
  ("&response_type=code&scope=guilds.join%20identify&prompt=none&state=" ++ state))
