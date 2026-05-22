
#ifdef BROWSER 


let hash_string : string -> int = Hashtbl.hash
let hash_string_int s i = Hashtbl.hash (s,i)
let hash_string_small_int :  string -> int  -> int = hash_string_int
let hash_stamp_and_name (i:int) (s:string) = Hashtbl.hash(i,s)
let hash_int (i:int) = Hashtbl.hash i 
let string_length_based_compare (x : string ) (y : string) = 
  let len1 = String.length x in 
  let len2 = String.length y in 
  if len1 = len2 then String.compare x y 
  else compare (len1:int) len2
let int_unsafe_blit: int array -> int -> int array -> int -> int -> unit = 
  Array.blit

#else
external hash_string :  string -> int = "caml_bs_hash_string" [@@noalloc];;

external hash_string_int :  string -> int  -> int = "caml_bs_hash_string_and_int" [@@noalloc];;

external hash_string_small_int :  string -> int  -> int = "caml_bs_hash_string_and_small_int" [@@noalloc];;

external hash_stamp_and_name : int -> string -> int = "caml_bs_hash_stamp_and_name" [@@noalloc];;

external hash_small_int : int -> int = "caml_bs_hash_small_int" [@@noalloc];;

external hash_int :  int  -> int = "caml_bs_hash_int" [@@noalloc];;

external string_length_based_compare : string -> string -> int  = "caml_string_length_based_compare" [@@noalloc];;

external    
  int_unsafe_blit : 
  int array -> int -> int array -> int -> int -> unit = "caml_int_array_blit" [@@noalloc];;

external set_as_old_file : string -> unit = "caml_stale_file"
#endif


