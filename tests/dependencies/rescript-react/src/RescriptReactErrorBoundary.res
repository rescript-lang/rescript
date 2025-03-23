@ocaml.doc("
 * Important note on this module:
 * As soon as React provides a mechanism for error-catching using functional component,
 * this is likely to be deprecated and/or move to user space.
 ")
type info = {componentStack: string}

type params<'error> = {
  error: 'error,
  info: info,
}

type reactComponentClass
@module("react") external component: reactComponentClass = "Component"
let noOp: reactComponentClass => unit = %raw(`function (_x) {}`)
let reactComponentClass = component
// this is so that the compiler doesn't optimize away the previous line
noOp(reactComponentClass)

%%raw(`

var ErrorBoundary = (function (Component) {
  function ErrorBoundary(props) {
    Component.call(this);
    this.state = { error: undefined };
  }
  ErrorBoundary.prototype = Object.create(Component.prototype);
  ErrorBoundary.prototype.componentDidCatch = function (error, info) {
    this.setState({ error: { error: error, info: info } });
  };
  ErrorBoundary.prototype.render = function () {
    return this.state.error != undefined
      ? this.props.fallback(this.state.error)
      : this.props.children;
  };
  return ErrorBoundary;
})(reactComponentClass);
`)

@react.component @val
external make: (
  ~children: React.element,
  ~fallback: params<'error> => React.element,
) => React.element = "ErrorBoundary"
