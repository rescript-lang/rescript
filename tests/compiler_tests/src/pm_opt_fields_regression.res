module SingleFormatCase = {
  type format1 = Int32

  type schema = {format?: format1}

  let bad1 = schema => {
    switch schema {
    | {format: Int32} => "int32"
    | _ => "default"
    }
  }

  let good1 = schema => {
    switch schema {
    | {format: _} => "int32"
    | _ => "default"
    }
  }
}

module MultipleFormatCase = {
  type format2 = Int32 | DD

  type schema = {format?: format2}

  let bad2 = schema => {
    switch schema {
    | {format: Int32} => "int32"
    | {format: DD} => "dd"
    | _ => "default"
    }
  }

  let good2 = schema => {
    switch schema {
    | {format: Int32} => "int32"
    | {format: _} => "dd"
    | _ => "default"
    }
  }
}
