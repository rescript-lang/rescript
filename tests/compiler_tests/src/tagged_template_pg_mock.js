// Mock of a third-party package (e.g. `postgres`) used to prove that a tag
// bound to a *bare* (non-relative) import specifier is still invoked with real
// tagged-template syntax at runtime. It is wired up via the Node `imports`
// subpath map in this package's `package.json` (`#tagged-template-pg`), so the
// bare specifier resolves to this file without installing a package.
//
// The returned tag throws unless it receives a genuine `TemplateStringsArray`
// (which only a real tagged-template call provides, via `.raw`). So a test that
// runs without throwing has proven the call site emitted backtick syntax.
export default (prefix) => (strings, ...values) => {
    if (strings.raw === undefined) {
        throw new Error("tag was not called as a tagged template (no .raw)");
    }
    let result = prefix;
    for (let i = 0; i < values.length; i++) {
        result += strings[i] + "'" + values[i] + "'";
    }
    result += strings[values.length];
    return result;
};
