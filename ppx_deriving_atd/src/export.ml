open Printf

let rec export_module_item_string = function
  | (Type (_, (name, _, annots), type_expr) : Atd.Ast.module_item) ->
      sprintf "\ntype %s %s = %s\n" name (export_annot_list annots)
        (export_type_expr_string type_expr)

and export_type_expr_string = function
  | Record (_loc, fields, annots) ->
      sprintf "{\n%s\n} %s"
        (String.concat "\n" (List.map export_field_string fields))
        (export_annot_list annots)
  | Sum (_loc, variants, annots) ->
      sprintf "[\n%s\n] %s"
        (String.concat "\n" (List.map export_variant_string variants))
        (export_annot_list annots)
  | Tuple (_loc, cells, annots) ->
      sprintf "( %s ) %s"
        (String.concat ", " @@ List.map export_cell_string cells)
        (export_annot_list annots)
  | List (_loc, type_expr, annots) ->
      sprintf "%s list %s"
        (export_type_expr_string type_expr)
        (export_annot_list annots)
  | Option (_loc, type_expr, annots) ->
      sprintf "%s option %s"
        (export_type_expr_string type_expr)
        (export_annot_list annots)
  | Nullable (_loc, type_expr, annots) ->
      sprintf "%s nullable %s"
        (export_type_expr_string type_expr)
        (export_annot_list annots)
  | Shared (_loc, type_expr, annots) ->
      sprintf "%s shared %s"
        (export_type_expr_string type_expr)
        (export_annot_list annots)
  | Wrap (_loc, type_expr, annots) ->
      sprintf "%s wrap %s"
        (export_type_expr_string type_expr)
        (export_annot_list annots)
  | Tvar (_loc, name) -> name
  | Name (_loc, (_loc2, name, type_exprs), annots) ->
      sprintf "%s %s %s" name
        (String.concat "\n" @@ List.map export_type_expr_string type_exprs)
        (export_annot_list annots)

and export_cell_string (_loc, type_expr, annots) =
  sprintf "%s %s" (export_type_expr_string type_expr) (export_annot_list annots)

and export_field_string = function
  | `Inherit (_loc, type_expr) ->
      (* illegal_derivation  (to use deprecation instead?) *)
      (*   (parsetree_loc_of_atd_loc loc) *)
      (*   {|using "inherit" is an anti-pattern and we do not support this.|} *)
      sprintf "\tinherit %s;" @@ export_type_expr_string type_expr
  | `Field (_, (name, kind, annots), type_expr) ->
      sprintf "\t%s%s %s: %s;"
        (export_field_kind_string kind)
        name (export_annot_list annots)
        (export_type_expr_string type_expr)

and export_variant_string = function
  | Inherit (_loc, type_expr) ->
      sprintf "\t| inherit %s" @@ export_type_expr_string type_expr
  | Variant (_loc, (name, annots), type_expr) ->
      sprintf "\t| %s %s %s" name (export_annot_list annots)
        (match Option.map export_type_expr_string type_expr with
        | None -> ""
        | Some v -> "of " ^ v)

and export_field_kind_string = function
  | Required -> ""
  | Optional -> "?"
  | With_default -> "~"

and export_annot_list annots =
  String.concat " " @@ List.map export_annot_section_list annots

and export_annot_section_list (name, (_, annot_fields)) =
  sprintf "<%s %s>" name
    (String.concat " " @@ List.map export_annot_field_list annot_fields)

and export_annot_field_list (name, (_, value)) =
  match value with None -> name | Some v -> sprintf {|%s=%S|} name v

let get_type_name (Type (_, (name, _, _), _) : Atd.Ast.module_item) = name

let export_module_body_string =
  List.map (fun x -> (get_type_name x, export_module_item_string x))

(* ******************* *)

let type_str =
  {|
type x = {
  ?abc : string option;
  def : int;
  ~ghi <ocaml default="true"> : bool;
  ~jkl <ocaml default="\"abc\"">: string;
}

type y = { x : x }

type z = [
 English
| Other of string
]

type zz = { z : z; y: y}
|}

let test () =
  let exported =
    let (_head, items), _types = Atd.Util.load_string type_str in
    export_module_body_string items
  in
  let exported = List.map snd exported |> String.concat "\n" in
  printf "exported:\n%s\n" exported;
  let reloaded =
    let (_head, items), _types = Atd.Util.load_string exported in
    export_module_body_string items
  in
  let reloaded = List.map snd reloaded |> String.concat "\n" in
  printf "reloaded:\n%s\n" reloaded
