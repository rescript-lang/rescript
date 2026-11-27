type moduleId = {"name": string}

@module external moduleId: moduleId = "#moduleid"

let f = () => moduleId["name"]
