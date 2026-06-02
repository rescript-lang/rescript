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
