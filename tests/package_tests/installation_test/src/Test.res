Console.log("Hello, world!")

let middleArray = [2, 3]
let endArray = [5]
let array = [1, ...middleArray, 4, ...endArray]

let middleList = list{2, 3}
let endList = list{5}
let list = list{1, ...middleList, 4, ...endList}

assert(array == [1, 2, 3, 4, 5])
assert(list == list{1, 2, 3, 4, 5})
