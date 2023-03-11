
(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Simple S-expression parsing/printing} *)
[@@@warning "a"]
type 'a or_error = [ `Ok of 'a | `Error of string ]
type 'a sequence = ('a -> unit) -> unit
type 'a gen = unit -> 'a option

type t = [
  | `Atom of string
  | `List of t list
]
type sexp = t


(** {2 Serialization (encoding)} *)

(* shall we escape the string because of one of its chars? *)
let _must_escape s =
  try
    for i = 0 to String.length s - 1 do
      let c = String.unsafe_get s i in
      match c with
      | ' ' | ';' | ')' | '(' | '"' | '\\' | '\n' | '\t' -> raise Exit
      | _ when Char.code c > 127 -> raise Exit  (* non-ascii *)
      | _ -> ()
    done;
    false
  with Exit -> true

let rec to_buf b t = match t with
  | `Atom s when _must_escape s -> Buffer.add_string b  ("\"" ^ String.escaped s ^ "\"")
  | `Atom s -> Buffer.add_string b s
  | `List [] -> Buffer.add_string b "()"
  | `List [x] -> Buffer.add_string b "(" ; to_buf b x; Buffer.add_string b ")"
  | `List l ->
    Buffer.add_char b '(';
    List.iteri
      (fun i t' -> (if i > 0 then Buffer.add_char b ' '; to_buf b t'))
      l;
    Buffer.add_char b ')'

let to_string t =
  let b = Buffer.create 128 in
  to_buf b t;
  Buffer.contents b




(** {2 Deserialization (decoding)} *)

module type MONAD = sig
  type 'a t
  val return : 'a -> 'a t
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
end

type 'a parse_result = ['a or_error | `End ]

module MakeDecode(M : MONAD) = struct
  let (>>=) = M.(>>=)

  type t = {
    buf : Bytes.t;
    refill : Bytes.t -> int -> int -> int M.t;
    atom : Buffer.t;
    mutable i : int; (* offset in [buf] *)
    mutable len : int; (* how many bytes of [buf] are usable *)
    mutable line : int;
    mutable col : int;
  }

  let make ?(bufsize=1024) refill =
    let bufsize = min (max bufsize 16) Sys.max_string_length in
    { buf=Bytes.create bufsize;
      refill;
      atom = Buffer.create 32;
      i=0;
      len=0;
      line=1;
      col=1;
    }

  let _is_digit c = Char.code '0' <= Char.code c && Char.code c <= Char.code '9'
  let _digit2i c = Char.code c - Char.code '0'

  (* refill buffer. If it works, call k_succ, otherwise call k_fail *)
  let _refill t k_succ k_fail =
    t.refill t.buf 0 (Bytes.length t.buf)
    >>= fun n ->
    t.i <- 0;
    t.len <- n;
    if n = 0 then k_fail t else k_succ t

  (* get next char, assuming t.i < t.len *)
  let _get t =
    assert (t.i < t.len);
    let c = Bytes.get t.buf t.i in
    t.i <- t.i + 1;
    if c = '\n' then (t.col <- 1; t.line <- t.line + 1) else t.col <- t.col + 1;
    c

  (* return an error *)
  let _error {line;col} (msg : string) =
    let b = Buffer.create 32 in
    Buffer.add_string b ("at " ^ (__unsafe_cast line) ^ ", " ^ (__unsafe_cast col) ^ ": ") ;
    Buffer.add_string b msg;
    let msg' = Buffer.contents b in
    M.return (`Error msg')


  let _error_eof t = _error t "unexpected end of input"

  (* The parsers all take a success continuation, and the decoder as
      last arguments. The continuation is used to minimize the
      number of calls to [>>=] and take two parameters, the next
      char (if not consumed), and the returned expression itself *)

  (* read expression *)
  let rec expr k t =
    if t.i = t.len then _refill t (expr k) _error_eof
    else match _get t with
      | ' ' | '\t' | '\n' -> expr k t
      | c -> expr_starting_with c k t

  and expr_starting_with c k t = match c with
    | ' ' | '\t' | '\n' -> assert false
    | ';' -> skip_comment (fun _ () -> expr k t) t
    | '(' -> expr_list [] k t
    | ')' -> _error t "unexpected ')'"
    | '\\' -> _error t "unexpected '\\'"
    | '"' -> quoted k t
    | c ->
      Buffer.add_char t.atom c;
      atom k t

  (* parse list *)
  and expr_list acc k t =
    if t.i = t.len then _refill t (expr_list acc k) _error_eof
    else match _get t with
      | ' ' | '\t' | '\n' -> expr_list acc k t
      | ')' -> k None (`List (List.rev acc))
      | c ->
        expr_starting_with c
          (fun last e -> match last with
             | Some '(' -> expr_list [] (fun _ l -> expr_list (l::acc) k t) t
             | Some ')' -> k None (`List (List.rev (e::acc)))
             | _ -> expr_list (e::acc) k t
          ) t

  (* return the current atom (last char: c) *)
  and _return_atom last k t =
    let s = Buffer.contents t.atom in
    Buffer.clear t.atom;
    k last (`Atom s)

  (* parse atom *)
  and atom k t =
    if t.i = t.len then _refill t (atom k) (_return_atom None k)
    else match _get t with
      | '\\' -> _error t "unexpected '\\' in non-quoted string"
      | '"' -> _error t "unexpected '\"' in the middle of an atom"
      | (' ' | '\n' | '\t' | '(' | ')') as c ->
        _return_atom (Some c) k t
      | c ->
        Buffer.add_char t.atom c;
        atom k t

  (* quoted string *)
  and quoted k t =
    if t.i = t.len then _refill t (quoted k) _error_eof
    else match _get t with
      | '\\' ->
        (* read escaped char and continue *)
        escaped
          (fun c ->
             Buffer.add_char t.atom c;
             quoted k t
          ) t
      | '"' -> _return_atom None k t
      | c ->
        Buffer.add_char t.atom c;
        quoted k t

  (* read escaped char *)
  and escaped k t =
    if t.i = t.len then _refill t (escaped k) _error_eof
    else match _get t with
      | 'n' -> k '\n'
      | 't' -> k '\t'
      | 'r' -> k '\r'
      | 'b' -> k '\b'
      | '\\' -> k '\\'
      | '"' -> k '"'
      | c when _is_digit c ->
        read2int (_digit2i c) (fun n -> k (Char.chr n)) t
      | c -> _error t ("unexpected escaped char '"  ^ (__unsafe_cast c) ^ "'") 

  and read2int i k t =
    if t.i = t.len then _refill t (read2int i k) _error_eof
    else match _get t with
      | c when _is_digit c -> read1int (10 * i + _digit2i c) k t
      | c -> _error t ("unexpected escaped char '"  ^ (__unsafe_cast c) ^ "' when reading byte")

  and read1int i k t =
    if t.i = t.len then _refill t (read1int i k) _error_eof
    else match _get t with
      | c when _is_digit c -> k (10 * i + _digit2i c)
      | c -> _error t ("unexpected escaped char '"  ^ (__unsafe_cast c) ^ "' when reading byte")

  (* skip until end of line, then call next() *)
  and skip_comment k t =
    if t.i = t.len
    then _refill t (skip_comment k) _error_eof
    else match _get t with
      | '\n' -> k None ()
      | _ -> skip_comment k t

  (* top-level expression *)
  let rec expr_or_end k t =
    if t.i = t.len
    then _refill t (expr_or_end k) (fun _ -> M.return `End)
    else match _get t with
      | ' ' | '\t' | '\n' -> expr_or_end k t
      | c -> expr_starting_with c k t

  (* entry point *)
  let next t : sexp parse_result M.t =
    expr_or_end (fun _ x -> M.return (`Ok x)) t
end

module ID_MONAD = struct
  type 'a t = 'a
  let return x = x
  let (>>=) x f = f x
end

module D = MakeDecode(ID_MONAD)

let parse_string s : t or_error =
  let n = String.length s in
  let stop = ref false in
  let refill bytes i _len =
    if !stop then 0
    else (stop := true; Bytes.blit_string s 0 bytes i n; n)
  in
  let d = D.make ~bufsize:n refill in
  match D.next d with
  | `End -> `Error "unexpected end of file"
  | (`Ok _ | `Error _) as res -> res

(*$T
    CCError.to_opt (parse_string "(abc d/e/f \"hello \\\" () world\" )") <> None
     CCError.to_opt (parse_string "(abc ( d e ffff   ) \"hello/world\")") <> None
*)


(*$Q & ~count:100
     sexp_gen (fun s -> sexp_valid s ==> (to_string s |> parse_string = `Ok s))
*)

