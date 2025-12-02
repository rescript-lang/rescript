external fetch: string => promise<'response> = "fetch"

type fetch<'resp> = %typeof(fetch)
