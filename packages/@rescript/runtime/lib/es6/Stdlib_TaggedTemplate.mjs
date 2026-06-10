


let make = (transform => (strings, ...values) => transform(strings, values));

export {
  make,
}
/* No side effect */
