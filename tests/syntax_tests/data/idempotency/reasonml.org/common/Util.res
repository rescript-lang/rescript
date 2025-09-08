module ReactStuff = {
  let s = ReasonReact.string
  let ate = ReasonReact.array

  module Unsafe = {
    external elementAsString: React.element => string = "%identity"
  }
  module Style = ReactDOMRe.Style
}

module String = {
  let camelCase: string => string = %raw(
    str =>
      "{
     return str.replace(/-([a-z])/g, function (g) { return g[1].toUpperCase(); });
    }"
  )

  let capitalize: string => string = %raw(
    str =>
      "{
      return str && str.charAt(0).toUpperCase() + str.substring(1);
    }"
  )
}

module Url = {
  let isAbsolute: string => bool = %raw(`
    function(str) {
      var r = new RegExp('^(?:[a-z]+:)?//', 'i');
      if (r.test(str))
      {
        return true
      }
      return false;
    }
  `)
}
