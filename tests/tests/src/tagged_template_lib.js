export const sql = (strings, ...values) => {
    let result = "";
    for (let i = 0; i < values.length; i++) {
        result += strings[i] + "'" + values[i] + "'";
    }
    result += strings[values.length];
    return result;
};

export const length = (strings, ...values) =>
    strings.reduce((acc, curr) => acc + curr.length, 0) +
        values.reduce((acc, curr) => acc + curr, 0);

// Factory that returns a tag function, mirroring libraries like `postgres`
// whose default export is a factory and the value it returns *is* the tag.
export const makeSql = (prefix) => (strings, ...values) => {
    let result = prefix;
    for (let i = 0; i < values.length; i++) {
        result += strings[i] + "'" + values[i] + "'";
    }
    result += strings[values.length];
    return result;
};

// Reports whether it was invoked as a *real* tagged template. A genuine
// tagged-template call receives a frozen `TemplateStringsArray` with a `.raw`
// property; a plain function call does not. Used to prove the compiler emits
// real tagged-template syntax rather than a variadic function call.
export const rawTag = (strings, ...values) => ({
    hasRaw: strings.raw !== undefined,
    raw: strings.raw ? Array.from(strings.raw) : [],
    cooked: Array.from(strings),
    values,
});
