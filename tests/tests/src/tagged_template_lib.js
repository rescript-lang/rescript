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
