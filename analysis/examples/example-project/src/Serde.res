let rec deserialize_Hello__TryIt____lockfile: Json.t => Belt.Result.t<
  MyNamespace.Hello.lockfile,
  string,
> = record =>
  switch record {
  | Json.Object(items) =>
    switch Belt.List.getAssoc(items, "current", \"=") {
    | None => Belt.Result.Error(@reason.raw_literal("No attribute ") "No attribute " ++ "current")
    | Some(json) =>
      switch (
        list =>
          switch list {
          | Json.Array(items) =>
            let transformer = json =>
              switch json {
              | Json.Array(list{arg0, arg1}) =>
                switch (
                  number =>
                    switch number {
                    | Json.Number(number) => Belt.Result.Ok(int_of_float(number))
                    | _ => Error(@reason.raw_literal("Expected a float") "Expected a float")
                    }
                )(arg1) {
                | Belt.Result.Ok(arg1) =>
                  switch deserialize_Hello__TryIt____shortReference(arg0) {
                  | Belt.Result.Ok(arg0) => Belt.Result.Ok(arg0, arg1)
                  | Error(error) => Error(error)
                  }
                | Error(error) => Error(error)
                }
              | _ => Belt.Result.Error(@reason.raw_literal("Expected array") "Expected array")
              }
            let rec loop = items =>
              switch items {
              | list{} => Belt.Result.Ok(list{})
              | list{one, ...rest} =>
                switch transformer(one) {
                | Belt.Result.Error(error) => Belt.Result.Error(error)
                | Belt.Result.Ok(value) =>
                  switch loop(rest) {
                  | Belt.Result.Error(error) => Belt.Result.Error(error)
                  | Belt.Result.Ok(rest) => Belt.Result.Ok(list{value, ...rest})
                  }
                }
              }
            loop(items)
          | _ => Belt.Result.Error(@reason.raw_literal("expected an array") "expected an array")
          }
      )(json) {
      | Belt.Result.Error(error) => Belt.Result.Error(error)
      | Belt.Result.Ok(attr_current) =>
        switch Belt.List.getAssoc(items, "pastVersions", \"=") {
        | None =>
          Belt.Result.Error(@reason.raw_literal("No attribute ") "No attribute " ++ "pastVersions")
        | Some(json) =>
          switch deserialize_Belt_HashMapInt____t(list =>
            switch list {
            | Json.Array(items) =>
              let transformer = json =>
                switch json {
                | Json.Array(list{arg0, arg1}) =>
                  switch (
                    number =>
                      switch number {
                      | Json.Number(number) => Belt.Result.Ok(int_of_float(number))
                      | _ => Error(@reason.raw_literal("Expected a float") "Expected a float")
                      }
                  )(arg1) {
                  | Belt.Result.Ok(arg1) =>
                    switch deserialize_Hello__TryIt____shortReference(arg0) {
                    | Belt.Result.Ok(arg0) => Belt.Result.Ok(arg0, arg1)
                    | Error(error) => Error(error)
                    }
                  | Error(error) => Error(error)
                  }
                | _ => Belt.Result.Error(@reason.raw_literal("Expected array") "Expected array")
                }
              let rec loop = items =>
                switch items {
                | list{} => Belt.Result.Ok(list{})
                | list{one, ...rest} =>
                  switch transformer(one) {
                  | Belt.Result.Error(error) => Belt.Result.Error(error)
                  | Belt.Result.Ok(value) =>
                    switch loop(rest) {
                    | Belt.Result.Error(error) => Belt.Result.Error(error)
                    | Belt.Result.Ok(rest) => Belt.Result.Ok(list{value, ...rest})
                    }
                  }
                }
              loop(items)
            | _ => Belt.Result.Error(@reason.raw_literal("expected an array") "expected an array")
            }
          )(json) {
          | Belt.Result.Error(error) => Belt.Result.Error(error)
          | Belt.Result.Ok(attr_pastVersions) =>
            switch Belt.List.getAssoc(items, "version", \"=") {
            | None =>
              Belt.Result.Error(@reason.raw_literal("No attribute ") "No attribute " ++ "version")
            | Some(json) =>
              switch (
                number =>
                  switch number {
                  | Json.Number(number) => Belt.Result.Ok(int_of_float(number))
                  | _ => Error(@reason.raw_literal("Expected a float") "Expected a float")
                  }
              )(json) {
              | Belt.Result.Error(error) => Belt.Result.Error(error)
              | Belt.Result.Ok(attr_version) =>
                Belt.Result.Ok({
                  version: attr_version,
                  pastVersions: attr_pastVersions,
                  current: attr_current,
                })
              }
            }
          }
        }
      }
    }
  | _ => Belt.Result.Error(@reason.raw_literal("Expected an object") "Expected an object")
  }
and deserialize_Hello__TryIt____shortReference: Json.t => Belt.Result.t<
  MyNamespace.Hello.shortReference,
  string,
> = value =>
  (
    json =>
      switch json {
      | Json.Array(list{arg0, arg1, arg2}) =>
        switch (
          string =>
            switch string {
            | Json.String(string) => Belt.Result.Ok(string)
            | _ => Error(@reason.raw_literal("epected a string") "epected a string")
            }
        )(arg2) {
        | Belt.Result.Ok(arg2) =>
          switch (
            list =>
              switch list {
              | Json.Array(items) =>
                let transformer = string =>
                  switch string {
                  | Json.String(string) => Belt.Result.Ok(string)
                  | _ => Error(@reason.raw_literal("epected a string") "epected a string")
                  }
                let rec loop = items =>
                  switch items {
                  | list{} => Belt.Result.Ok(list{})
                  | list{one, ...rest} =>
                    switch transformer(one) {
                    | Belt.Result.Error(error) => Belt.Result.Error(error)
                    | Belt.Result.Ok(value) =>
                      switch loop(rest) {
                      | Belt.Result.Error(error) => Belt.Result.Error(error)
                      | Belt.Result.Ok(rest) => Belt.Result.Ok(list{value, ...rest})
                      }
                    }
                  }
                loop(items)
              | _ => Belt.Result.Error(@reason.raw_literal("expected an array") "expected an array")
              }
          )(arg1) {
          | Belt.Result.Ok(arg1) =>
            switch (
              string =>
                switch string {
                | Json.String(string) => Belt.Result.Ok(string)
                | _ => Error(@reason.raw_literal("epected a string") "epected a string")
                }
            )(arg0) {
            | Belt.Result.Ok(arg0) => Belt.Result.Ok(arg0, arg1, arg2)
            | Error(error) => Error(error)
            }
          | Error(error) => Error(error)
          }
        | Error(error) => Error(error)
        }
      | _ => Belt.Result.Error(@reason.raw_literal("Expected array") "Expected array")
      }
  )(value)
and deserialize_Belt_HashMapInt____t: 'arg0. (
  Json.t => Belt.Result.t<'arg0, string>,
  Json.t,
) => Belt.Result.t<Belt_HashMapInt.t<'arg0>, string> = bTransformer =>
  TransformHelpers.deserialize_Belt_HashMapInt____t(bTransformer)
let rec serialize_Hello__TryIt____lockfile: MyNamespace.Hello.lockfile => Json.t = record => Json.Object(list{
  ("version", (i => Json.Number(float_of_int(i)))(record.version)),
  (
    "pastVersions",
    serialize_Belt_HashMapInt____t(list => Json.Array(
      Belt.List.map(list, ((arg0, arg1)) => Json.Array(list{
        serialize_Hello__TryIt____shortReference(arg0),
        (i => Json.Number(float_of_int(i)))(arg1),
      })),
    ))(record.pastVersions),
  ),
  (
    "current",
    (
      list => Json.Array(
        Belt.List.map(list, ((arg0, arg1)) => Json.Array(list{
          serialize_Hello__TryIt____shortReference(arg0),
          (i => Json.Number(float_of_int(i)))(arg1),
        })),
      )
    )(record.current),
  ),
})
and serialize_Hello__TryIt____shortReference: MyNamespace.Hello.shortReference => Json.t = value =>
  (
    ((arg0, arg1, arg2)) => Json.Array(list{
      (s => Json.String(s))(arg0),
      (list => Json.Array(Belt.List.map(list, s => Json.String(s))))(arg1),
      (s => Json.String(s))(arg2),
    })
  )(value)
and serialize_Belt_HashMapInt____t: 'arg0. (
  'arg0 => Json.t,
  Belt_HashMapInt.t<'arg0>,
) => Json.t = bTransformer => TransformHelpers.serialize_Belt_HashMapInt____t(bTransformer)
