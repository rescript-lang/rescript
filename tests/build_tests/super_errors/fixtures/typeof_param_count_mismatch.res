external fetch: string => promise<'response> = "fetch"

type fetch<'response, 'x> = %typeof(fetch)
