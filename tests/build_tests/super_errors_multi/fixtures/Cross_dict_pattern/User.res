let read = (cfg: Defs.config) =>
  switch cfg {
  | dict{"port": p} => p
  }
