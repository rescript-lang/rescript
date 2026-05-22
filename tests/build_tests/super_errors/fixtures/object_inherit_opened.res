type a<'a> = {.."x": int} as 'a

type b = {...a<'b>, "y": int}
