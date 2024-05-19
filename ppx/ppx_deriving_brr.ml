let rec foldr1 f xs =
  match xs with
  | [] -> failwith "foldr1"
  | [x] -> x
  | y :: ys -> f y (foldr1 f ys)

open Ppxlib
module Ast = Ast_builder.Default

let located ~loc e = { loc; txt = e }
let lident ~loc i = { loc; txt = Lident i }
let ppx_name = "brr"

exception Failed of location * string

let error ~loc fmt = Format.ksprintf (fun s -> raise (Failed (loc, s))) fmt
let error_expr ~loc msg = [%expr [%ocaml.error [%e Ast.estring ~loc msg]]]
let error_str ~loc msg = [%str [%ocaml.error [%e Ast.estring ~loc msg]]]

let rec jv_conversion_for loc dir (typ : core_type) =
  let templ =
    match dir with
    | `ToJv -> Format.asprintf "%s_to_jv"
    | `OfJv -> Format.asprintf "%s_of_jv"
  in
  match typ.ptyp_desc with
  | Ptyp_constr ({ txt = Lident name; loc = _ }, args) ->
    (match (name, args, dir) with
    | "int", [], `ToJv -> [%expr Jv.of_int]
    | "int", [], `OfJv -> [%expr Jv.to_int]
    | "string", [], `ToJv -> [%expr Jv.of_string]
    | "string", [], `OfJv -> [%expr Jv.to_string]
    | "float", [], `ToJv -> [%expr Jv.of_float]
    | "float", [], `OfJv -> [%expr Jv.to_float]
    | "bool", [], `ToJv -> [%expr Jv.of_bool]
    | "bool", [], `OfJv -> [%expr Jv.to_bool]
    | _ ->
      let fn =
        if String.equal name "t" then
          Ast.evar ~loc (match dir with `ToJv -> "to_jv" | `OfJv -> "of_jv")
        else Ast.evar ~loc (templ name)
      in
      let args =
        List.map (jv_conversion_for loc dir) args
        |> List.map (fun a -> (Nolabel, a))
      in
      Ast.pexp_apply ~loc fn args)
  | Ptyp_var name ->
    (* refer to a parameter *)
    let fn = Ast.evar ~loc (templ name) in
    fn
  | Ptyp_poly (_, _) -> failwith "poly"
  | Ptyp_any
  | Ptyp_arrow (_, _, _)
  | Ptyp_tuple _
  | Ptyp_constr (_, _)
  | Ptyp_object (_, _)
  | Ptyp_class (_, _)
  | Ptyp_alias (_, _)
  | Ptyp_variant (_, _, _)
  | Ptyp_package _ | Ptyp_extension _ ->
    failwith (Format.asprintf "unknown kind of type %a" Pprintast.core_type typ)

let is_type_decl_recursive t1 =
  let name = t1.ptype_name.txt in
  let rec refers_to_name t =
    match t.ptyp_desc with
    | Ptyp_constr ({ txt = Lident n; loc = _ }, a) ->
      if String.equal n name then true else List.exists refers_to_name a
    | Ptyp_var _ -> false
    | _ -> failwith "unhandled"
  in
  match t1.ptype_kind with
  | Ptype_variant cs ->
    List.exists
      (fun c ->
        match c.pcd_args with
        | Pcstr_tuple ts -> List.exists refers_to_name ts
        | Pcstr_record _ -> failwith "nyi")
      cs
  | Ptype_record _ -> false
  | Ptype_open | Ptype_abstract -> failwith "nyi"

let make_binding_with_params loc is_rec name type_params templ final_fn_body =
  (* add printers for type parameters *)
  let fn_with_params =
    List.fold_right
      (fun c t -> Ast.pexp_fun ~loc Nolabel None (Ast.pvar ~loc (templ c)) t)
      type_params final_fn_body
  in
  let binding =
    Ast.pstr_value ~loc is_rec
      [
        Ast.value_binding ~loc
          ~pat:(Ast.pvar ~loc (templ name))
          ~expr:fn_with_params;
      ]
  in
  binding

let generate_record loc is_rec type_params name fields =
  let generate_of () =
    (*
      let a_to_jv {f1} =
        Jv.obj [| "f1", f1t_to_jv f1 |]
    *)
    let param =
      Ast.ppat_record ~loc
        (List.map
           (fun f ->
             let name = f.pld_name.txt in
             (lident ~loc name, Ast.pvar ~loc name))
           fields)
        Closed
    in
    let body =
      [%expr
        Jv.obj
          [%e
            Ast.pexp_array ~loc
              (List.map
                 (fun f ->
                   let name = f.pld_name.txt in
                   let arg = Ast.evar ~loc name in
                   Ast.pexp_tuple ~loc
                     [
                       Ast.estring ~loc name;
                       [%expr
                         [%e jv_conversion_for loc `ToJv f.pld_type] [%e arg]];
                     ])
                 fields)]]
    in
    (* the function which takes the value to be converted *)
    let final_fn_body = Ast.pexp_fun ~loc Nolabel None param body in
    make_binding_with_params loc is_rec name type_params
      (fun t ->
        match t with "t" -> "to_jv" | _ -> Format.asprintf "%s_to_jv" t)
      final_fn_body
  in
  let generate_to () =
    (*
      let a_of_jv t =
        { f1=jv_to_f1t (Jv.get t "f1") }
    *)
    let param = Ast.pvar ~loc "j" in
    let body =
      Ast.pexp_record ~loc
        (List.map
           (fun f ->
             let name = f.pld_name.txt in
             let typ = f.pld_type in
             ( lident ~loc name,
               Ast.pexp_apply ~loc
                 (jv_conversion_for loc `OfJv typ)
                 [(Nolabel, [%expr Jv.get j [%e Ast.estring ~loc name]])] ))
           fields)
        None
    in
    (* the function which takes the value to be converted *)
    let final_fn_body = Ast.pexp_fun ~loc Nolabel None param body in
    make_binding_with_params loc is_rec name type_params
      (fun t ->
        match t with "t" -> "of_jv" | _ -> Format.asprintf "%s_of_jv" t)
      final_fn_body
  in
  [generate_of (); generate_to ()]

let generate_variant loc is_rec type_params name cases =
  (*
    let jv_of_a t =
      match t with
      | A x -> ["A", jv_of_xt x]
  *)
  let generate_of () =
    let body =
      Ast.pexp_match ~loc [%expr t]
        (List.map
           (fun c ->
             let name = c.pcd_name.txt in
             let args =
               match c.pcd_args with
               | Pcstr_tuple ts ->
                 List.mapi
                   (fun i _ta -> Ast.pvar ~loc (Format.asprintf "v%d" i))
                   ts
               | Pcstr_record _ -> failwith "nyi"
             in
             let rhs =
               (*
                 let a = Jv.Jarray.create 2 in
                 Jv.Jarray.set a 0 (jv_of_arg1t arg1);
                 Jv.Jarray.set a 1 (jv_of_arg2t arg2)
               *)
               let len = Ast.eint ~loc (List.length args + 1) in
               let sets =
                 match c.pcd_args with
                 | Pcstr_tuple ts ->
                   List.mapi
                     (fun i typ ->
                       let v =
                         [%expr
                           [%e jv_conversion_for loc `ToJv typ]
                             [%e Ast.evar ~loc (Format.asprintf "v%d" i)]]
                       in
                       let idx = Ast.eint ~loc (i + 1) in
                       [%expr Jv.Jarray.set a [%e idx] [%e v]])
                     ts
                 | Pcstr_record _ -> failwith "nyi"
               in
               let sets =
                 match sets with
                 | [] -> Ast.eunit ~loc
                 | _ -> foldr1 (fun c t -> Ast.pexp_sequence ~loc c t) sets
               in
               let tag = [%expr Jv.of_string [%e Ast.estring ~loc name]] in
               [%expr
                 let a = Jv.Jarray.create [%e len] in
                 Jv.Jarray.set a 0 [%e tag];
                 [%e sets];
                 a]
             in
             Ast.case
               ~lhs:
                 (Ast.ppat_construct ~loc (lident ~loc name)
                    (match args with
                    | [] -> None
                    | _ -> Some (Ast.ppat_tuple ~loc args)))
               ~guard:None ~rhs)
           cases)
    in
    (* final function which acts on thing to be converted *)
    let of_final_fn = Ast.pexp_fun ~loc Nolabel None (Ast.pvar ~loc "t") body in
    make_binding_with_params loc is_rec name type_params
      (fun t ->
        match t with "t" -> "to_jv" | _ -> Format.asprintf "%s_to_jv" t)
      of_final_fn
  in
  (*
    let jv_to_a j =
      match t with
      | ["A"; x] -> A (jv_of_xt x)
  *)
  let generate_to () =
    let body =
      Ast.pexp_match ~loc
        [%expr Jv.to_string (Jv.Jarray.get t 0)]
        (List.map
           (fun c ->
             let name = c.pcd_name.txt in
             let args =
               match c.pcd_args with
               | Pcstr_tuple ts ->
                 List.mapi
                   (fun i ta ->
                     Ast.pexp_apply ~loc
                       (jv_conversion_for loc `OfJv ta)
                       [
                         ( Nolabel,
                           [%expr Jv.Jarray.get t [%e Ast.eint ~loc (i + 1)]] );
                       ])
                   ts
               | Pcstr_record _ -> failwith "nyi"
             in
             let rhs =
               Ast.pexp_construct ~loc (lident ~loc name)
                 (match args with
                 | [] -> None
                 | [a] -> Some a
                 | _ -> Some (Ast.pexp_tuple ~loc args))
             in
             Ast.case
               ~lhs:(Ast.ppat_constant ~loc (Pconst_string (name, loc, None)))
               ~guard:None ~rhs)
           cases)
    in
    (* silence inexhaustiveness warning *)
    let body =
      {
        body with
        pexp_attributes =
          [
            Ast.attribute ~loc ~name:(located ~loc "warning")
              ~payload:(PStr [Ast.pstr_eval ~loc (Ast.estring ~loc "-8") []]);
          ];
      }
    in
    (* final function which acts on thing to be converted *)
    let of_final_fn = Ast.pexp_fun ~loc Nolabel None (Ast.pvar ~loc "t") body in
    make_binding_with_params loc is_rec name type_params
      (fun t ->
        match t with "t" -> "of_jv" | _ -> Format.asprintf "%s_of_jv" t)
      of_final_fn
  in
  [generate_of (); generate_to ()]

let generate_type_decl tdecl =
  let td =
    (* TODO mutually recursive types *)
    List.hd tdecl
  in
  let is_rec = if is_type_decl_recursive td then Recursive else Nonrecursive in
  let { loc; txt = name } = td.ptype_name in
  let type_params =
    List.map
      (fun (p, _) ->
        match p.ptyp_desc with
        | Ptyp_var a -> a
        | _ ->
          failwith
            (Format.asprintf "unknown kind of type %a" Pprintast.core_type p))
      td.ptype_params
  in
  match td.ptype_kind with
  | Ptype_record fields -> generate_record loc is_rec type_params name fields
  | Ptype_variant cases -> generate_variant loc is_rec type_params name cases
  | Ptype_abstract ->
    (* begin
         match td.ptype_manifest with
         | None -> error ~loc "cannot generate for abstract type"
         | Some typ -> [generate_printer_type ~loc name typ]
       end *)
    []
  | Ptype_open -> failwith "open"

let str_gen ~loc:_ ~path:_ (_rec, t) =
  try
    let extra = generate_type_decl t in
    extra
  with Failed (loc, s) -> error_str ~loc s

let sig_gen ~loc ~path:_ (_rec, _t) =
  let (module Ast) = Ast_builder.make loc in
  (* we are silently dropping mutually recursive definitions to keep things
     brief *)
  (* let t = List.hd t in
     let name = module_name_of_type t in
     let type_ =
       let sig_ =
         [%sig:
           val path : string
           val name : string]
       in
       Ast.pmty_signature sig_
     in
     Ast.module_declaration ~name ~type_ |> Ast.psig_module |> fun a -> [a] *)
  []

let () =
  let str_type_decl =
    Deriving.Generator.make
      Deriving.Args.(
        empty
        (* +> arg "between" (estring __)
           +> arg "fn" (estring __)
           +> flag "latex" *))
      str_gen
  in
  (* let sig_type_decl = Deriving.Generator.make_noarg sig_gen in *)
  Deriving.add ppx_name ~str_type_decl
  (* ~sig_type_decl *)
  |> Deriving.ignore
