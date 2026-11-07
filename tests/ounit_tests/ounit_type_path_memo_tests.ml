open Types

let ( >:: ), ( >::: ) = OUnit.(( >:: ), ( >::: ))

(* Env memoizes type path lookups. The same path, with the same identifier
   stamps, can name different declarations in different environments of one
   request: typedecl first enters abstract placeholders for a recursive group
   and then the real declarations under the same identifiers, and recursive
   modules are typed against an approximation first. Each lookup must answer
   for the environment it is given, whatever an earlier lookup of the same
   path cached. *)

let declaration manifest =
  {
    type_params = [];
    type_arity = 0;
    type_kind = Type_abstract;
    type_private = Asttypes.Public;
    type_manifest = manifest;
    type_variance = [];
    type_newtype_level = None;
    type_loc = Location.none;
    type_attributes = [];
    type_immediate = false;
    type_representation = Boxed;
    type_shape = Unstated;
    type_inlined_types = [];
  }

let abstract () = declaration None
let int_alias () = declaration (Some (Predef.type_int ()))

let has_manifest decl = decl.type_manifest <> None

let check message condition = OUnit.assert_bool message condition

let suites =
  __FILE__
  >::: [
         ( "an identifier bound to different declarations" >:: fun _ ->
           let id = Ident.create "t" in
           let placeholder = abstract () and real = int_alias () in
           let first = Env.add_type ~check:false id placeholder Env.empty in
           let second = Env.add_type ~check:false id real Env.empty in
           let path = Path.Pident id in
           check "the first environment's declaration"
             (Env.find_type path first == placeholder);
           check "the second environment's declaration"
             (Env.find_type path second == real);
           check "the first environment's declaration again"
             (Env.find_type path first == placeholder) );
         ( "a module bound to different signatures" >:: fun _ ->
           let module_id = Ident.create "M" and type_id = Ident.create "t" in
           let signature decl =
             Mty_signature [Sig_type (type_id, decl, Trec_not)]
           in
           let approximated =
             Env.add_module module_id (signature (abstract ())) Env.empty
           in
           let real =
             Env.add_module module_id (signature (int_alias ())) Env.empty
           in
           let path = Path.Pdot (Path.Pident module_id, "t", 0) in
           check "the approximation is abstract"
             (not (has_manifest (Env.find_type path approximated)));
           check "the real module has the manifest"
             (has_manifest (Env.find_type path real));
           check "the approximation is still abstract"
             (not (has_manifest (Env.find_type path approximated))) );
         ( "a module that is an alias in only one environment" >:: fun _ ->
           let target = Ident.create "Target"
           and module_id = Ident.create "A" in
           let type_id = Ident.create "t" in
           let signature =
             Mty_signature [Sig_type (type_id, int_alias (), Trec_not)]
           in
           let base = Env.add_module target signature Env.empty in
           let plain = Env.add_module module_id signature base in
           let alias =
             Env.add_module module_id
               (Mty_alias (Mta_present, Path.Pident target))
               base
           in
           let path = Path.Pdot (Path.Pident module_id, "t", 0) in
           check "a plain module leaves the path unchanged"
             (Env.normalize_type_path plain path = None);
           (match Env.normalize_type_path alias path with
           | Some normalized ->
             check "an alias normalizes to its target"
               (Path.same normalized (Path.Pdot (Path.Pident target, "t", 0)))
           | None -> OUnit.assert_failure "an alias normalizes the path");
           check "the plain module still leaves the path unchanged"
             (Env.normalize_type_path plain path = None) );
       ]
