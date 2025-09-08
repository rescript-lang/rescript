type location = {
  search: string,
  href: string,
}
type locationState

type history = {replaceState: (. locationState, string, string) => unit}

type rec window = {
  history: history,
  location: location,
  mutable global: window,
}

@val external window: window = "window"
