type callback = {run: unit => int}

@val external wrap: callback => callback = "wrap"

let rec callback = {...wrap(callback), run: () => 1}
