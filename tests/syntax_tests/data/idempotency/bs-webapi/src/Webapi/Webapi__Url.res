module URLSearchParams = {
  type t

  @new external make: string => t = "URLSearchParams"
  @new external makeWithDict: dict<string> => t = "URLSearchParams"
  @new external makeWithArray: array<(string, string)> => t = "URLSearchParams"
}

type t

@new external make: string => t = "URL"
@new external makeWithBase: (string, string) => t = "URL"
@get external hash: t => string = ""
@set external setHash: (t, string) => unit = "hash"
@get external host: t => string = ""
@set external setHost: (t, string) => unit = "host"
@get external hostname: t => string = ""
@set external setHostname: (t, string) => unit = "hostname"
@get external href: t => string = ""
@set external setHref: (t, string) => unit = "href"
@get external origin: t => string = ""
@get external password: t => string = ""
@set external setPassword: (t, string) => unit = "password"
@get external pathname: t => string = ""
@set external setPathname: (t, string) => unit = "pathname"
@get external port: t => string = ""
@set external setPort: (t, string) => unit = "port"
@get external protocol: t => string = ""
@set external setProtocol: (t, string) => unit = "protocol"
@get external search: t => string = ""
@set external setSearch: (t, string) => unit = "search"
@get external searchParams: t => URLSearchParams.t = ""
@get external username: t => string = ""
@set external setUsername: (t, string) => unit = "username"
@get external toJson: t => string = ""

@val @scope("URL") external createObjectURL: Webapi__File.t => string = ""
@val @scope("URL") external revokeObjectURL: string => unit = ""
