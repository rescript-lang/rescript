let splitIntoChunks = (arr, ~chunkSize) => {
  let chunks = []
  let length = arr->Array.length
  let startRef = ref(0)

  while startRef.contents < length {
    let end = startRef.contents + chunkSize
    let chunk = arr->Array.slice(~start=startRef.contents, ~end)

    chunks->Array.push(chunk)
    startRef := end
  }

  chunks
}

let forEachAsyncParallel = async (arr, f) => {
  let _: array<unit> = await Promise.all(arr->Array.map(f))
}

let forEachAsyncSerially = async (arr, f) =>
  for i in 0 to arr->Array.length - 1 {
    await f(arr->Array.getUnsafe(i))
  }

let forEachAsyncInBatches = (arr, ~batchSize, f) =>
  arr
  ->splitIntoChunks(~chunkSize=batchSize)
  ->forEachAsyncSerially(chunk => chunk->forEachAsyncParallel(f))
