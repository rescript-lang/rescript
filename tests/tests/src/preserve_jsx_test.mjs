import { Fragment, jsx, jsxs } from "react/jsx-runtime";
import * as Primitive_option from "rescript/lib/es6/Primitive_option.js";
import * as JsxRuntime from "react/jsx-runtime";
let React = {};
let ReactDOM = {};
function Preserve_jsx_test$Icon(props) {
  return /* @__PURE__ */ jsx("strong", {});
}
let Icon = {
  make: Preserve_jsx_test$Icon
};
let _single_element_child = /* @__PURE__ */ jsx("div", { children: /* @__PURE__ */ jsx("h1", { children: "Hello, world!" }) });
let _multiple_element_children = /* @__PURE__ */ jsxs("div", { children: [
  /* @__PURE__ */ jsx("h1", { children: "Hello, world!" }),
  /* @__PURE__ */ jsx(Preserve_jsx_test$Icon, {})
] });
let _single_element_fragment = /* @__PURE__ */ jsx(Fragment, { children: Primitive_option.some(/* @__PURE__ */ jsx("input", {})) });
let _multiple_element_fragment = /* @__PURE__ */ jsxs(Fragment, { children: [
  /* @__PURE__ */ jsx("input", { type: "text" }),
  /* @__PURE__ */ jsx("input", { type: "number" })
] });
let _unary_element_with_props = /* @__PURE__ */ jsx("input", { className: "foo", type: "text" });
let _container_element_with_props_and_children = /* @__PURE__ */ jsx("div", { className: "foo", title: "foo", children: "Hello, world!" });
let baseProps = {
  className: "foo",
  title: "foo"
};
let newrecord = { ...baseProps };
let _unary_element_with_spread_props = /* @__PURE__ */ jsx("input", { ...newrecord, type: "text" });
let newrecord$1 = { ...baseProps };
let _container_with_spread_props = /* @__PURE__ */ jsxs("div", { ...newrecord$1, title: "barry", className: "barry", children: [
  "Hello, world!",
  /* @__PURE__ */ jsx("input", { type: "text" })
] });
let baseChildren = [
  /* @__PURE__ */ jsx("span", { children: "Hello, world!" }),
  /* @__PURE__ */ jsx("span", { children: "Hello, world!" })
];
let _container_with_spread_children = /* @__PURE__ */ jsx("div", { className: "barry", title: "barry", children: baseChildren });
let newrecord$2 = { ...baseProps };
let _container_with_spread_props_and_children = /* @__PURE__ */ jsx("div", { ...newrecord$2, title: "barry", className: "barry", children: baseChildren });
let newrecord$3 = { ...baseProps };
let _unary_element_with_spread_props_keyed = /* @__PURE__ */ jsx("input", { ...newrecord$3, type: "text" }, "barry-key");
let newrecord$4 = { ...baseProps };
let _container_with_spread_props_keyed = /* @__PURE__ */ jsxs("div", { ...newrecord$4, title: "barry", className: "barry", children: [
  "Hello, world!",
  /* @__PURE__ */ jsx("input", { type: "text" })
] }, "barry-key");
let _unary_element_with_only_spread_props = /* @__PURE__ */ jsx("input", { ...baseProps });
function QueryClientProvider(props) {
  return props.children;
}
;
let A = {};
function Preserve_jsx_test$B(props) {
  return /* @__PURE__ */ jsx("p", { children: "Hello, world!" });
}
let B = {
  make: Preserve_jsx_test$B
};
let _external_component_with_children = /* @__PURE__ */ jsxs(QueryClientProvider, { children: [
  /* @__PURE__ */ jsx("strong", {}),
  /* @__PURE__ */ jsx(Preserve_jsx_test$B, {})
] });
export {
  A,
  B,
  Icon,
  React,
  ReactDOM,
  _container_element_with_props_and_children,
  _container_with_spread_children,
  _container_with_spread_props,
  _container_with_spread_props_and_children,
  _container_with_spread_props_keyed,
  _external_component_with_children,
  _multiple_element_children,
  _multiple_element_fragment,
  _single_element_child,
  _single_element_fragment,
  _unary_element_with_only_spread_props,
  _unary_element_with_props,
  _unary_element_with_spread_props,
  _unary_element_with_spread_props_keyed,
  baseChildren,
  baseProps
};
