@module({from: "./myJson.json", with: {type_: "json", \"some-identifier": "yep"}})
external myJson: JSON.t = "default"

Console.log(myJson)

@module({from: "./myCss.css", with: {type_: "css", \"some-identifier": "yep"}})
external buttonCss: string = "button"

Console.log(buttonCss)
