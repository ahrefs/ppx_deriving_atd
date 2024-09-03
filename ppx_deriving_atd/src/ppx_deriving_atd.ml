open Ppxlib
open Common
open Printf

(* Some design decisions:
   - ignoring names lookup: each deriving is done on a particular type so will not have stubs defined for any non-primitive types. checks on types are done by the OCaml compiler.
   - to generate ATD ASTs from OCaml AST? or emit OCaml functions directly? Generating ATD ASTs is slightly more disciplined and easier.
   - do we need another level of module now? X_j.of_string and X_j.to_string? or stick to top-level x_j_to_string and x_j_of_string. Keeping it top-level for ease of use.
*)

let tmp_file_binding = "_EXPORT_ATD_FILENAME"
let type_name_binding = sprintf "_EXPORT_ATD_%s_STRING"
let type_name_binding_re = Re2.create_exn {|_EXPORT_ATD_\S_STRING|}
let is_type_name_binding = Re2.matches type_name_binding_re

let print_location { loc_start; loc_end; _ } =
  Atd.Ast.string_of_loc (loc_start, loc_end)

let generate_impl_atd ~ctxt (_rec_flag, type_decls) =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  let type_defs =
    List.map (Convert.type_def_of_type_declaration loc) type_decls
  in
  let atd_strings =
    (* Export.test (); *)
    (* if Sys.file_exists tmp_file then *)
    (*   let handler = *)
    (*     Stdlib.open_out_gen *)
    (*       [ Open_wronly; Open_append; Open_text ] *)
    (*       0o666 tmp_file *)
    (*   in *)
    let tmp_file = atd_filename_from_loc loc in
    let type_strs = Export.export_module_body_string type_defs in
    let type_strs =
      List.map
        (fun (type_name, type_str) ->
          sprintf
            {|
let %s = %S
          
let %s = %S
|}
            tmp_file_binding tmp_file
            (type_name_binding type_name)
            type_str)
        type_strs
    in
    Parse.implementation (Lexing.from_string (String.concat "\n" type_strs))
    (*   Stdlib.output_string handler type_strs *)
    (* else () *)
  in
  let atd_loc = atd_loc_of_parsetree_loc loc in
  let head, m0 = ((atd_loc, [] (*TODO: annotations*)), type_defs) in
  let m1', original_types =
    Atd.Expand.expand_module_body ~keep_builtins:false ~keep_poly:true m0
  in
  let m1 = Atd.Util.tsort m1' in
  let defs = Atdgen_emit.Oj_mapping.defs_of_atd_modules m1 ~target:Json in
  let ocaml_typedefs =
    Atdgen_emit.Ocaml.ocaml_of_atd ~target:Json ~type_aliases:None (head, m1)
  in
  let ml =
    Atdgen_emit.Oj_emit.make_ml ~header:"" ~opens:[] ~with_typedefs:false
      ~with_create:true ~with_fundefs:true ~std:true ~unknown_field_handler:None
      ~force_defaults:true ~preprocess_input:None ~original_types
      ~ocaml_version:None ocaml_typedefs
      (Atdgen_emit.Mapping.make_deref defs)
      defs
  in
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
        match k with
        | k when k = tmp_file_binding || is_type_name_binding k -> Some (k, v)
        | _ -> None)
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
  let tmp_file =
    match List.assoc_opt tmp_file_binding bindings with
    | None -> failwith "no extract file found!"
    | Some x -> x
  in
  let atd_string =
    List.filter_map
      (fun (k, v) -> if k = tmp_file_binding then None else Some v)
      bindings
    |> String.concat "\n"
  in
  let handler = Stdlib.open_out tmp_file in
  Stdlib.output_string handler atd_string;
  strs

let deriver =
  Driver.register_transformation ~impl:collect_atd_strings_and_export
    "collect_atd_strings_and_export";
  Deriving.add "atd_j"
    ~str_type_decl:(Deriving.Generator.V2.make_noarg generate_impl_atd)

(* need some global transformation:
   - generate strings into unique variables
   - iterate through patterns of unique variables
*)
