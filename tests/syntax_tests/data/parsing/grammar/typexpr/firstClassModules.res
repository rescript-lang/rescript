type t = module(Hashmap)
type t = module(Hashmap with type key = string)
type t = module(Hashmap with type key = string and type value = int)

type toValueLikeInstance = t<'a> => module(RxValueLikeInstance.S with type a = 'a)
type t<'a> = module(Test with type a = 'a)
type t = ref<module(Console)>

let devices: Hastbl.t<string, module(DEVICE)> = Hashtbl.creat(17)
