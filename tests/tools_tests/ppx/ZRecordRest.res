let _ = 0

type config = {
  name: string,
  version: string,
  debug: bool,
}

type subConfig = {
  version: string,
  debug: bool,
}

let extract = ({name, ...subConfig as rest}: config) => (name, rest)
