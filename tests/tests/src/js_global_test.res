open Js_global

let suites = {
  open Mt
  list{
    (
      "setTimeout/clearTimeout sanity check",
      _ => {
        let handle = setTimeout(() => (), 0)
        clearTimeout(handle)
        Ok(true)
      },
    ),
    (
      "setInerval/clearInterval sanity check",
      _ => {
        let handle = setInterval(() => (), 0)
        clearInterval(handle)
        Ok(true)
      },
    ),
    ("encodeURI", _ => Eq(encodeURI("[-=-]"), "%5B-=-%5D")),
    ("decodeURI", _ => Eq(decodeURI("%5B-=-%5D"), "[-=-]")),
    ("encodeURIComponent", _ => Eq(encodeURIComponent("[-=-]"), "%5B-%3D-%5D")),
    ("decodeURIComponent", _ => Eq(decodeURIComponent("%5B-%3D-%5D"), "[-=-]")),
  }
}

Mt.from_pair_suites(__MODULE__, suites)
