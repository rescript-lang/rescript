type form = Unary | Binary

type specialization = {
  int: Lambda.primitive;
  bool: Lambda.primitive option;
  float: Lambda.primitive option;
  bigint: Lambda.primitive option;
  string: Lambda.primitive option;
}

type entry = {
  path: string;
  name: string;
  form: form;
  specialization: specialization;
}

val index_by_path : (string, entry) Hashtbl.t

val index_by_name : (string, entry) Hashtbl.t
