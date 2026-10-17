export function jsx(type, props) {
  return { type, props };
}

export function memo(type) {
  return { type };
}

export function forwardRef(render) {
  return { render };
}

export let effects = 0;

export function effect() {
  effects++;
}
