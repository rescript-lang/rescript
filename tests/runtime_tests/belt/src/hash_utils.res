external seeded_hash_param: (int, int, int, 'a) => int = "%hash"

let hash = x => seeded_hash_param(10, 100, 0, x)
