  let parsedPayload =
    try (JSON.parseOrThrow(response)) {
    | _ => JSON.null
    }
