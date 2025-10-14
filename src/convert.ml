open Ppxlib
open Printf
open Atd.Ast
open Common

type attribute_section = OCaml | Json | Www
type atd_loc = position * position

type attribute = {
  section : attribute_section;
  name : string * location;
  loc : atd_loc;
  payload : payload;
}

let ignored_printing_attributes = [ "deriving"; "required"; "optional" ]
let no_arg_internal_attribute = [ "required"; "optional" ]
let extract_single_string = Ast_pattern.(single_expr_payload (estring __))

let extract_default_payload loc p =
  Ast_pattern.parse extract_single_string loc
    ~on_error:(fun () ->
      illegal_derivation loc
        "only stringify default values are accepted (e.g: \"1\" instead \
         of 1) ")
    p Fun.id

let annot_of_attributes attrs =
  let map_filed { section = _; name = name, loc; payload; loc = _ } =
    match List.mem name ignored_printing_attributes with
    | true -> None
    | false ->
        let payload =
          match List.mem name no_arg_internal_attribute with
          | true -> None
          | false -> Some (extract_default_payload loc payload)
        in
        Some (name, (atd_loc_of_parsetree_loc loc, payload))
  in
  let map (section, (fields, loc)) =
    let string_of_section = function
      | OCaml -> "ocaml"
      | Json -> "json"
      | Www -> "www"
    in
    match List.filter_map map_filed fields with
    | [] -> None
    | fields -> Some (string_of_section section, (loc, fields))
  in
  List.filter_map map attrs

let parse_attributes attrs =
  let map { attr_name = { txt; loc }; attr_payload; attr_loc } =
    let atd_loc = atd_loc_of_parsetree_loc attr_loc in
    let with_section section name =
      Some
        {
          section;
          name = name, loc;
          loc = atd_loc;
          payload = attr_payload;
        }
    in
    match String.split_on_char '.' txt with
    | "deriving" :: [] -> None
    | x :: [] -> with_section OCaml x
    | [ "json"; x ] -> with_section Json x
    | [ "www"; x ] -> with_section Www x
    | _ ->
        unsupported_derivation loc
          (sprintf "Unsupported attribute header: %s" txt)
  in
  let with_sections = List.filter_map map attrs in
  let by_sections = Hashtbl.create (List.length with_sections) in
  List.iter
    (fun ({ section; _ } as attr) -> Hashtbl.add by_sections section attr)
    with_sections;
  let by_sections =
    Hashtbl.to_seq_keys by_sections
    |> Seq.map (fun section ->
        let attrs = Hashtbl.find_all by_sections section in
        let attr_locs = List.map (fun { loc; _ } -> loc) attrs in
        match attr_locs with
        | [] -> section, ([], (Lexing.dummy_pos, Lexing.dummy_pos))
        | fst :: _ ->
            let loc =
              List.fold_left
                (fun (start_', end_') (start_, end_) ->
                  let start_' =
                    match compare_lexing_position start_' start_ with
                    | v when v > 0 -> start_
                    | _ -> start_'
                  in
                  let end_' =
                    match compare_lexing_position end_' end_ with
                    | v when v < 0 -> end_
                    | _ -> end_'
                  in
                  start_', end_')
                fst attr_locs
            in
            section, (attrs, loc))
    |> Hashtbl.of_seq
  in
  let find_field attrs name' =
    List.exists (fun { name = name, _; _ } -> name = name') attrs
  in
  let field_type =
    match Hashtbl.find_opt by_sections OCaml with
    | Some (attrs, _) when find_field attrs "default" -> Some With_default
    | Some (attrs, _) when find_field attrs "optional" -> Some Optional
    | Some (attrs, _) when find_field attrs "required" -> Some Required
    | Some _ | None -> None
  in
  let annots =
    annot_of_attributes (List.of_seq (Hashtbl.to_seq by_sections))
  in
  annots, field_type

let rec type_def_of_type_declaration loc type_decl =
  let loc = atd_loc_of_parsetree_loc loc in
  let {
    ptype_params;
    ptype_name = { txt = name; _ };
    ptype_attributes;
    _;
  } =
    type_decl
  in
  let type_expr, extras =
    type_expr_of_type_declaration ~extras:[] type_decl
  in
  let type_params = type_param_of_ptype_param ptype_params in
  let annots, _field_type = parse_attributes ptype_attributes in
  Type (loc, (name, type_params, annots), type_expr) :: extras

and type_param_of_ptype_param ptype_params =
  let map core_type =
    match core_type.ptyp_desc with
    | Ptyp_var x -> sprintf "'%s" x
    | _ ->
        unsupported_derivation core_type.ptyp_loc
          (string_of_core_type core_type)
  in
  List.map
    (fun (core_type, (_variance, _injectivity)) -> map core_type)
    ptype_params

and type_expr_of_type_declaration ~extras type_decl =
  let loc', loc =
    type_decl.ptype_loc, atd_loc_of_parsetree_loc type_decl.ptype_loc
  in
  match type_decl with
  | Parsetree.
      {
        ptype_kind = Ptype_abstract;
        ptype_manifest = Some core_type;
        ptype_attributes;
        _;
      } ->
      type_expr_of_core_type ~extras loc' core_type
  | { ptype_kind = Ptype_record tys; ptype_attributes; _ } ->
      let annots, _field_type = parse_attributes ptype_attributes in
      let extras', type_exprs =
        List.fold_left
          (fun (extras, type_exprs)
               { pld_type; pld_name; pld_attributes; _ } ->
            let loc = atd_loc_of_parsetree_loc pld_name.loc in
            let type_expr, extras' =
              type_expr_of_core_type ~extras loc' pld_type
            in
            let annots, field_type = parse_attributes pld_attributes in
            let field_type =
              match field_type with
              | Some field_type -> field_type
              | None -> (
                  match type_expr with
                  | (Option (_, _, _) : Atd.Ast.type_expr) -> Optional
                  | _ -> Required)
            in
            ( extras',
              `Field (loc, (pld_name.txt, field_type, annots), type_expr)
              :: type_exprs ))
          (extras, []) tys
      in
      Record (loc, List.rev type_exprs, annots), extras'
  | { ptype_kind = Ptype_variant tys; ptype_attributes; _ } as type_decl
    ->
      let annots, _field_type = parse_attributes ptype_attributes in
      let extras', type_exprs =
        List.fold_left
          (fun (extras, type_exprs) -> function
            | { pcd_name; pcd_args = Pcstr_tuple []; pcd_attributes; _ }
              ->
                let annots, _field_type =
                  parse_attributes pcd_attributes
                in
                ( extras,
                  (Variant (loc, (pcd_name.txt, annots), None) : variant)
                  :: type_exprs )
            | { pcd_name; pcd_args = Pcstr_tuple l; pcd_attributes; _ } ->
                let type_expr, extras' =
                  type_expr_of_core_type ~extras loc'
                    Parsetree.
                      {
                        ptyp_desc = Ptyp_tuple l;
                        ptyp_loc = pcd_name.loc;
                        ptyp_loc_stack = [ pcd_name.loc ];
                        ptyp_attributes = [];
                      }
                in
                let annots, _field_type =
                  parse_attributes pcd_attributes
                in
                ( extras' @ extras,
                  Variant (loc, (pcd_name.txt, annots), Some type_expr)
                  :: type_exprs )
            | _ ->
                unsupported_derivation loc'
                  (string_of_type_decl type_decl))
          (extras, []) tys
      in
      Sum (loc, List.rev type_exprs, annots), extras'
  | _ ->
      unsupported_derivation type_decl.ptype_loc
        (string_of_type_decl type_decl)

and type_expr_of_core_type ~extras loc' core_type =
  let loc = atd_loc_of_parsetree_loc loc' in
  let { ptyp_attributes; ptyp_desc; _ } = core_type in
  let annots, _field_type = parse_attributes ptyp_attributes in
  match ptyp_desc with
  | Ptyp_var x -> Tvar (loc, x), extras
  | Ptyp_tuple tys ->
      with_core_tys_extras ~extras ~tys loc' @@ fun ~type_exprs ->
      Tuple (loc, type_exprs, annots)
  | Ptyp_constr
      ( {
          txt = Lident "list" (* TODO: qualified names e.g. Module.x *);
          loc;
        },
        [ ty ] (* only supports 1 type in list*) ) ->
      let loc = atd_loc_of_parsetree_loc loc in
      with_core_ty_extras ~extras ~ty loc' @@ fun ~type_expr ->
      List (loc, type_expr, annots)
  | Ptyp_constr (id, tys) ->
      type_expr_of_ptyp_constr ~extras ~annots loc' core_type (id, tys)
  | _ ->
      (* Shard and Wrap doesn't exist anymore *)
      unsupported_derivation loc' (string_of_core_type core_type)

and type_expr_of_ptyp_constr ~extras ~annots loc' core_type = function
  | ( {
        txt = Lident "option" (* TODO: qualified names e.g. Module.x *);
        loc;
      },
      [ ty ] (* only supports 1 type in option*) ) ->
      let loc = atd_loc_of_parsetree_loc loc in
      with_core_ty_extras ~extras ~ty loc' @@ fun ~type_expr ->
      Option (loc, type_expr, annots)
  | { txt = Lident id (* TODO: qualified names e.g. Module.x *); loc }, _
    when List.mem id [ "list"; "option" ] ->
      (* TODO: support (string, json> list and allow conversion to object *)
      unsupported_derivation loc
        (sprintf "%s: has multiple types for %s (invalid)"
           (string_of_core_type core_type)
           id)
  | { txt = Ldot (prefix, id); loc }, tys ->
      let loc = atd_loc_of_parsetree_loc loc in
      let f_cell (_loc, type_expr, _annots) = type_expr in
      let type_expr, extras =
        with_core_tys_extras ~extras ~tys loc' @@ fun ~type_exprs ->
        Name (loc, (loc, id, List.map f_cell type_exprs), annots)
      in
      let (_head, type_decls), _original_types =
        Atd.Util.load_string
        @@ sprintf {|type %s <ocaml from=%S> = abstract|} id
             (Longident.name prefix)
      in
      type_expr, type_decls @ extras
  | ( { txt = Lident id (* TODO: qualified names e.g. Module.x *); loc },
      tys ) ->
      let loc = atd_loc_of_parsetree_loc loc in
      let f_cell (_loc, type_expr, _annot) = type_expr in
      with_core_tys_extras ~extras ~tys loc' @@ fun ~type_exprs ->
      Name (loc, (loc, id, List.map f_cell type_exprs), annots)
  | _ -> unsupported_derivation loc' (string_of_core_type core_type)

and with_core_tys_extras ~extras ~tys loc' f =
  let loc = atd_loc_of_parsetree_loc loc' in
  let extras', type_exprs =
    List.fold_left
      (fun (extras, type_exprs) core_type ->
        let _annots, _field_type =
          parse_attributes core_type.ptyp_attributes
        in
        let type_expr, extras' =
          type_expr_of_core_type ~extras loc' core_type
        in
        extras' @ extras, (loc, type_expr, []) :: type_exprs)
      (extras, []) tys
  in
  f ~type_exprs:(List.rev type_exprs), extras'

and with_core_ty_extras ~extras ~ty loc' f =
  let type_expr, extras' = type_expr_of_core_type ~extras loc' ty in
  f ~type_expr, extras' @ extras
