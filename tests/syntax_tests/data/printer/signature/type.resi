type s = string

type user = {name: string, age: int}

type color =
  | Red
  | Blue
  | Black

type options = {
  permissions: {
    all: {
      stuff: bool
    }
  }
}
