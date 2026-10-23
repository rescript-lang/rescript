// A dict literal has side effects only through its values and spreads:
// spreading a dict reads its properties.
let deadNoEffect = dict{"a": 1}
let deadWithValueEffect = dict{"a": Console.log("effect")}
let deadWithSpread = dict{...deadNoEffect, "b": 2}
