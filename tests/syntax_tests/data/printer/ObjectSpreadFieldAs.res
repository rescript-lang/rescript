type base = {"a": int}

type extended = {...base, @as("renamed") "b": int}
