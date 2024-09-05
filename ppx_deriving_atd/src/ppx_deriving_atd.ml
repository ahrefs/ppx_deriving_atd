open Ppxlib
open Common
open Printf

(* Some design decisions:
   - ignoring names lookup (in ATD): each deriving is done on a particular type so will not have stubs defined for any non-primitive types. checks on types are done by the OCaml compiler.
   - to generate ATD ASTs from OCaml AST? or emit OCaml functions directly? Generating ATD ASTs is slightly more disciplined and easier.
   - do we need another level of module now? X_j.of_string and X_j.to_string? or stick to top-level x_j_to_string and x_j_of_string. Keeping it top-level for ease of use.

   - another way to avoid global transformation is to only allow deriving atd from a module--this will make sure all the type definitions are processed and then exported in one go
*)

let tmp_file_binding' = "_EXPORT_ATD_FILENAME"
let tmp_file_binding loc = Ast_builder.Default.pvar ~loc tmp_file_binding'

let type_name_binding x loc =
  sprintf "_EXPORT_ATD_%s_STRING" x |> Ast_builder.Default.pvar ~loc

let type_name_binding_re = Re2.create_exn {|_EXPORT_ATD_\S+_STRING|}
let is_type_name_binding = Re2.matches type_name_binding_re

let let_str_generator loc mk_loc_pat s =
  let pat = mk_loc_pat loc in
  let expr = Ast_builder.Default.estring ~loc s in
  [%stri let [%p pat] = [%e expr]]

let print_location { loc_start; loc_end; _ } =
  Atd.Ast.string_of_loc (loc_start, loc_end)

let generate_impl_atd ~ctxt (_rec_flag, type_decls) =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  let type_defs =
    (List.concat_map (Convert.type_def_of_type_declaration loc)) type_decls
  in

  let atd_strings =
    let tmp_file = atd_filename_from_loc loc in
    let type_strs = Export.export_module_body_string type_defs in
    List.concat_map
      (fun (type_name, type_str) ->
        (* better way to do this? have to keep output in code :/ *)
        [
          let_str_generator loc tmp_file_binding tmp_file;
          let_str_generator loc (type_name_binding type_name) type_str;
        ])
      type_strs
  in

  let ml = Convert.ml_string_of_atd_module_items loc type_defs in
  List.concat
    [
      List.map
        (fold_loc_structure_item loc)
        (Parse.implementation (Lexing.from_string ml));
      atd_strings;
    ]

(* Context_free transformation (Derivers) are executed before global transformation phase--so we setup some artifacts for the global derivers so that a file can be exported by the global deriver. *)
let collect_atd_strings_and_export strs =
  let extract_binding_with_string_value =
    Ast_pattern.(
      pstr_value drop
        (value_binding ~pat:(ppat_var __) ~expr:(estring __) ^:: nil))
  in
  let extract loc str =
    Ast_pattern.parse extract_binding_with_string_value loc
      ~on_error:(fun _ -> None)
      str
      (fun k v ->
        if k = tmp_file_binding' || is_type_name_binding k then Some (k, v)
        else None)
  in
  let get_let_bindings =
    object
      inherit [(string * string) list] Ast_traverse.fold as super

      method! structure_item s acc =
        let loc = s.pstr_loc in
        let acc =
          match extract loc s with Some (k, v) -> (k, v) :: acc | None -> acc
        in
        super#structure_item s acc
    end
  in
  let bindings = get_let_bindings#structure strs [] in
  let () =
    match List.assoc_opt tmp_file_binding' bindings with
    | None ->
        ()
        (* if no filename is found, then no export because must be some problem with previous step *)
    | Some tmp_file ->
        let atd_string =
          List.filter_map
            (fun (k, v) -> if k = tmp_file_binding' then None else Some v)
            bindings
          |> String.concat "\n"
        in
        let handler = Stdlib.open_out tmp_file in
        Stdlib.output_string handler atd_string
  in
  strs

let deriver =
  Driver.register_transformation ~impl:collect_atd_strings_and_export
    "collect_atd_strings_and_export";
  Deriving.add "atd_j"
    ~str_type_decl:(Deriving.Generator.V2.make_noarg generate_impl_atd)
