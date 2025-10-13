open Ppxlib
open Printf

exception Show_me of Parsetree.type_declaration

let atd_loc_of_parsetree_loc { loc_start; loc_end; _ } = (loc_start, loc_end)

let parsetree_loc_of_atd_loc (loc_start, loc_end) =
  { loc_start; loc_end; loc_ghost = false }

let raise_errorf ?sub ?loc fmt =
  let module Location = Ocaml_common.Location in
  let raise_msg str =
    let err = Location.error ?sub ?loc ("PPX Deriving ATD: " ^ str) in
    raise (Location.Error err)
  in
  ksprintf raise_msg fmt

let unsupported_derivation loc msg =
  raise_errorf ~loc "Unsupported derivation: %s at %s" msg
    (loc |> atd_loc_of_parsetree_loc |> Atd.Ast.string_of_loc)

let illegal_derivation loc msg =
  raise_errorf ~loc "Illegal derivation: %s at %s" msg
    (loc |> atd_loc_of_parsetree_loc |> Atd.Ast.string_of_loc)

(* PARSETREE AST UTILS *)
let string_of_parse_tree ff x =
  let open Format in
  ignore (flush_str_formatter ());
  let f = str_formatter in
  ff f x;
  flush_str_formatter ()

let string_of_core_type = string_of_parse_tree Pprintast.core_type
let string_of_type_decl = string_of_parse_tree Pprintast.type_declaration
let string_of_structure = Pprintast.string_of_structure

let rec fold_loc_exp_desc loc exp_desc =
  match exp_desc with
  | Parsetree.Pexp_ident e -> Pexp_ident { e with loc }
  | Pexp_let (flag, value_bindings, e) ->
      Pexp_let
        (flag, fold_loc_value_bindings loc value_bindings, fold_loc_exp loc e)
  | Pexp_apply (e, args) ->
      Pexp_apply
        ( fold_loc_exp loc e,
          List.map (fun (a, e) -> (a, fold_loc_exp loc e)) args )
  | Pexp_fun (arg, e0, p, e1) ->
      Pexp_fun
        ( arg,
          Option.map (fold_loc_exp loc) e0,
          fold_loc_pat loc p,
          fold_loc_exp loc e1 )
  | Pexp_sequence (e1, e2) ->
      Pexp_sequence (fold_loc_exp loc e1, fold_loc_exp loc e2)
  | _ -> exp_desc

and fold_loc_pat_desc loc = function
  | Ppat_var l -> Ppat_var { l with loc }
  | p -> p

and fold_loc_value_bindings loc =
  List.map (fun v ->
      {
        v with
        pvb_loc = loc;
        pvb_expr = fold_loc_exp loc v.pvb_expr;
        pvb_pat = fold_loc_pat loc v.pvb_pat;
      })

and fold_loc_pat loc pat = { pat with ppat_loc = loc; ppat_loc_stack = [ loc ] }

and fold_loc_exp loc exp =
  {
    exp with
    pexp_loc_stack = loc :: exp.pexp_loc_stack;
    pexp_loc = loc;
    pexp_desc = fold_loc_exp_desc loc exp.pexp_desc;
  }

let fold_loc_structure_item loc x =
  let pstr_desc =
    match x.pstr_desc with
    | Parsetree.Pstr_eval (e, attrs) -> Pstr_eval (fold_loc_exp loc e, attrs)
    | Pstr_value (flag, value_bindings) ->
        Pstr_value (flag, fold_loc_value_bindings loc value_bindings)
    | e -> e
  in
  { pstr_loc = loc; pstr_desc }

let atd_filename_from_loc ?(extension = ".atd") loc =
  Filename.chop_extension loc.loc_start.pos_fname ^ extension

let make_ghost_loc loc = { loc with loc_ghost = true }
