open Ppxlib
open Astlib

open Parsetree

let is_id_field = function
  | { pld_name = { txt = "id"; _ }; _ } -> true
  | _ -> false

let strName_to_longIndent
  : string Asttypes.loc -> Longident.t Asttypes.loc
  = fun loc  ->
    { loc with txt = (Lident loc.txt)}

let map_label_to_pattern
  : Parsetree.label_declaration -> pattern
  = fun declaration ->
    { ppat_desc = Ppat_var (declaration.pld_name)
    ; ppat_loc = declaration.pld_name.loc
    ; ppat_loc_stack = []
    ; ppat_attributes = []
    }

(** Convert a label_declaration into an entry for a record pattern *)
let map_label_to_record_pattern
  : Parsetree.label_declaration -> (Longident.t Asttypes.loc * pattern)
  = fun declaration ->
    ( strName_to_longIndent declaration.pld_name
    , map_label_to_pattern declaration
    )

let map_label_to_expr
  : Parsetree.label_declaration -> expression
  = fun declaration ->
    { pexp_desc = Pexp_ident (strName_to_longIndent declaration.pld_name)
    ; pexp_loc = declaration.pld_name.loc
    ; pexp_loc_stack = []
    ; pexp_attributes = []
    }

let map_label_to_record_expr
  : Parsetree.label_declaration -> (Longident.t Asttypes.loc * expression)
  = fun declaration ->
    ( strName_to_longIndent declaration.pld_name
    , map_label_to_expr declaration
    )

let get_attribute
  : string -> Parsetree.attribute -> string option
  = fun name t  ->
    if String.equal name t.attr_name.txt then
      match t.attr_payload with
      | PStr structure ->
        begin match ListLabels.filter_map structure ~f:(fun f -> match f.pstr_desc with
            | Pstr_eval (expr, _) ->
              begin match expr.pexp_desc with
                | Pexp_constant (Pconst_string (value, _, _)) -> Some value
                | _ -> None
              end
            | _ -> None
          ) with
        | [] -> None
        | value::_ -> Some value
        end
      | _ -> None
    else
      None

let map_label_to_caqti_t
  : Parsetree.label_declaration -> Asttypes.arg_label * expression
  = fun declaration ->

    let empty_expression =
      { pexp_desc = Pexp_ident { declaration.pld_name with txt = (Lident "()")}
      ; pexp_loc = declaration.pld_name.loc
      ; pexp_loc_stack = []
      ; pexp_attributes = []
      }
    in

    let rec map_type_to_caqti
      : Parsetree.core_type -> Parsetree.expression
      = fun core_type ->
        match ListLabels.filter_map declaration.pld_attributes ~f:(get_attribute "type") with
        | att::_ ->
          { empty_expression with
            pexp_desc = Pexp_ident { declaration.pld_name with txt = (Lident att)}
          }
        | [] ->
          match core_type.ptyp_desc with
          | Ptyp_constr (loc, []) ->
            { empty_expression with
              pexp_desc = Pexp_ident loc
            }
          (* When the type is parametric, transform it into a function call
             string option -> (option string)
          *)
          | Ptyp_constr (loc, params) ->
            let parameters = ListLabels.map
                ~f:(fun p -> Asttypes.Nolabel, map_type_to_caqti p)
                params
            in
            { empty_expression with
              pexp_desc = Pexp_apply
                  ( { empty_expression with
                      pexp_desc = Pexp_ident loc }
                  , parameters)
            }
          | _ ->
            (* In last case, try to match with the record name *)
            { empty_expression with
              pexp_desc = Pexp_ident { declaration.pld_name with txt = (Lident declaration.pld_name.txt)}
            }
    in

    let expression =
      (* Match if there is an attribute *)
      map_type_to_caqti declaration.pld_type
    in
    (Nolabel, expression)


let process_decl _rec_flag (decl:type_declaration) =
  let decl_tpe =
    match decl with
    | { ptype_name = { txt; loc }
      ; ptype_kind = Ptype_record fields
      ; _ } ->
      Some
        ( fields
        , txt
        , loc)
    | _ -> None (* ppx_caqti is only defined for record types *)
  in

  match decl_tpe with
  | None -> None
  | Some (fields, type_name, loc) ->

    let empty_pattern = [%pat? _]
    and empty_expression = [%expr () ]
    and empty_type = [%type: unit]
    in

    let db_pattern:Parsetree.pattern =
      { empty_pattern with
        ppat_desc = Parsetree.Ppat_var ({txt = (type_name); loc})
      }
    in
    (** The type we are deriving *)
    let db_type:Parsetree.core_type =
      { empty_type with
        ptyp_desc = Parsetree.Ptyp_constr ({txt = (Lident type_name); loc}, [])
      }

    (* This is the record definition, presented as a pattorn for the function
        "encode" *)
    and encode_patten:Parsetree.pattern =
      { empty_pattern with
        ppat_desc = Ppat_record
            ( ListLabels.map fields ~f:map_label_to_record_pattern
            , Closed )
      }

    and encode_expr:Parsetree.expression =
      { empty_expression with
        pexp_desc = Pexp_tuple (ListLabels.map fields ~f:map_label_to_expr)
      }

    and decode_patten:Parsetree.pattern =
      { empty_pattern with
        ppat_desc = Ppat_tuple ( ListLabels.map fields ~f:map_label_to_pattern )
      }

    and decode_expr:Parsetree.expression =
      { empty_expression with
        pexp_desc = Pexp_record
            ( ListLabels.map fields ~f:map_label_to_record_expr
            , None)
      }

    and caqti_rep:Parsetree.expression =
      let call = ("tup" ^ (Int.to_string (List.length fields))) in
      { empty_expression with
        pexp_desc = Pexp_apply
            ( { empty_expression with
                pexp_desc = Pexp_ident ({loc = loc; txt = Lident call })
              }
            , (ListLabels.map fields ~f:map_label_to_caqti_t)
            )
      }

    in

    Some
      [%stri
         (** Custom type for Caqti *)
        let [%p db_pattern] : [%t db_type] Caqti_type.t =
          let encode [%p encode_patten] =
            Ok [%e encode_expr]
          and decode [%p decode_patten] =
            Ok [%e decode_expr]
          and rep = Custom_caqti_type.([%e caqti_rep])
          in
          Caqti_type.custom ~encode ~decode rep
      ]


let expand_str ~loc:_ ~path:_ (rec_flag, decls) =
  ListLabels.filter_map ~f:(process_decl rec_flag) decls

let str_generator = Ppxlib.Deriving.Generator.make_noarg expand_str

let deriver = Ppxlib.Deriving.add
    ~str_type_decl:str_generator
    "caqti"
