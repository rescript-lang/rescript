type form = Unary | Binary

type lowering =
  | Lower of Lambda.primitive
  | Pass_through  (** the operand is already the result: unary [+] *)

type specialization = {
  int: lowering;
  bool: lowering option;
  float: lowering option;
  bigint: lowering option;
  string: lowering option;
}

type entry = {
  path: string;
  name: string;
  form: form;
  specialization: specialization;
}

val index_by_path : (string, entry) Hashtbl.t

val index_by_name : (string, entry) Hashtbl.t
