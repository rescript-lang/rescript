type baseProps = {"name": string}

type props = {
  ...baseProps,
  label: string,
}

let label: props = {
  "name": "hello",
  "label": "label",
}
