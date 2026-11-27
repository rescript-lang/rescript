let count = 0;

export const tag = (_strings, ..._values) => {
  count += 1;
  return 1;
};

export const join = (strings, ...values) => {
  count += 1;
  return strings.reduce((acc, s, i) => acc + values[i - 1] + s);
};

export const on = (name, payload) => `${name}:${payload}`;

export const ignoreFirst = value => value;

export const calls = () => count;

export const reset = () => {
  count = 0;
};
