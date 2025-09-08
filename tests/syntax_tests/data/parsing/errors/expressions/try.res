  let parsedPayload =
    try (Js.Json.parseExn(response)) {
    | _ => Js.Json.null
    }
