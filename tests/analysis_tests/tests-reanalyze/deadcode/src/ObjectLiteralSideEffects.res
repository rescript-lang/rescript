// An object literal's field expressions determine its side effects: the
// first binding must be classified with side effects, the second without.
let deadWithEffect = {"x": Console.log("effect")}
let deadNoEffect = {"x": 1}
