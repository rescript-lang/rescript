let fn = async () => 12

let other = async (): int => {
  await fn()
}

/* === AVAILABLE ACTIONS:
- AddAwait - Await promise
*/
