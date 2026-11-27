@module({from: `./myJson.json`, with: {type_: `j\x73on`, \"some-identifier": `y\x65p`}})
external myJson: JSON.t = "default"

Console.log(myJson)

@module({from: `./myCss.css`, with: {type_: `c\x73s`, \"some-identifier": `y\x65p`}})
external buttonCss: string = "button"

Console.log(buttonCss)
