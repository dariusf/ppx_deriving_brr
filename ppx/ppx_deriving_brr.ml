let rec unsnoc xs =
  match xs with
  | [] -> failwith "unsnoc"
  | [x] -> ([], x)
  | x :: xs ->
    let xs1, last = unsnoc xs in
    (x :: xs1, last)

let foldr1 f xs =
  match xs with
  | [] -> failwith "foldr1"
  | _ ->
    let xs, last = unsnoc xs in
    List.fold_right f xs last

open Ppxlib
module Ast = Ast_builder.Default

let ppx_name = "brr"

exception Failed of location * string

let error ~loc fmt = Format.ksprintf (fun s -> raise (Failed (loc, s))) fmt
let error_expr ~loc msg = [%expr [%ocaml.error [%e Ast.estring ~loc msg]]]
let error_str ~loc msg = [%str [%ocaml.error [%e Ast.estring ~loc msg]]]

let rec jv_conversion_for loc (typ : core_type) =
  match typ.ptyp_desc with
  | Ptyp_constr ({ txt = Lident name; loc = _ }, args) ->
    (match (name, args) with
    | "int", [] -> [%expr Jv.of_int]
    | "string", [] -> [%expr Jv.of_string]
    | "float", [] -> [%expr Jv.of_float]
    | "bool", [] -> [%expr Jv.of_bool]
    | _, _ ->
      let fn = Ast.evar ~loc (Format.asprintf "jv_of_%s" name) in
      let args =
        List.map (jv_conversion_for loc) args
        |> List.map (fun a -> (Nolabel, a))
      in
      Ast.pexp_apply ~loc fn args)
  | Ptyp_var name ->
    (* refer to a parameter *)
    let fn = Ast.evar ~loc (Format.asprintf "jv_of_%s" name) in
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
  let (module Ast) = Ast_builder.make loc in
  match td.ptype_kind with
  | Ptype_record fields ->
    (*
       let jv_of_a {f1} =
         Jv.obj [| "f1", jv_of_f1t f1 |]
    *)
    let param =
      Ast.ppat_record
        (List.map
           (fun f ->
             let name = f.pld_name.txt in
             ({ loc; txt = Lident name }, Ast.pvar name))
           fields)
        Closed
    in
    let body =
      [%expr
        Jv.obj
          [%e
            Ast.pexp_array
              (List.map
                 (fun f ->
                   let name = f.pld_name.txt in
                   let arg = Ast.pexp_ident { loc; txt = Lident name } in
                   Ast.pexp_tuple
                     [
                       Ast.estring name;
                       [%expr [%e jv_conversion_for loc f.pld_type] [%e arg]];
                     ])
                 fields)]]
    in
    (* the function which takes the value to be converted *)
    let of_final_fn = Ast.pexp_fun Nolabel None param body in
    (* add printers for type parameters *)
    let expr =
      List.fold_right
        (fun c t ->
          Ast.pexp_fun Nolabel None (Ast.pvar (Format.asprintf "jv_of_%s" c)) t)
        type_params of_final_fn
    in
    [
      Ast.pstr_value is_rec
        [
          Ast.value_binding
            ~pat:(Ast.pvar (Format.asprintf "jv_of_%s" name))
            ~expr;
        ];
    ]
  | Ptype_variant cases ->
    (*
      let jv_of_a t =
        match t with
        | A x -> ["A", jv_of_xt x]
    *)
    let body =
      Ast.pexp_match [%expr t]
        (List.map
           (fun c ->
             let name = c.pcd_name.txt in
             let args =
               match c.pcd_args with
               | Pcstr_tuple ts ->
                 List.mapi (fun i _ta -> Ast.pvar (Format.asprintf "v%d" i)) ts
               | Pcstr_record _ -> failwith "nyi"
             in
             let rhs =
               (*
                  let a = Jv.Jarray.create 2 in
                  Jv.Jarray.set a 0 (jv_of_arg1t arg1);
                  Jv.Jarray.set a 1 (jv_of_arg2t arg2)
               *)
               let len = Ast.eint (List.length args + 1) in
               let sets =
                 match c.pcd_args with
                 | Pcstr_tuple ts ->
                   List.mapi
                     (fun i typ ->
                       let v =
                         [%expr
                           [%e jv_conversion_for loc typ]
                             [%e Ast.evar (Format.asprintf "v%d" i)]]
                       in
                       let idx = Ast.eint (i + 1) in
                       [%expr Jv.Jarray.set a [%e idx] [%e v]])
                     ts
                 | Pcstr_record _ -> failwith "nyi"
               in
               let sets =
                 match sets with
                 | [] -> Ast.eunit
                 | _ -> foldr1 (fun c t -> Ast.pexp_sequence c t) sets
               in
               let tag = [%expr Jv.of_string [%e Ast.estring name]] in
               [%expr
                 let a = Jv.Jarray.create [%e len] in
                 Jv.Jarray.set a 0 [%e tag];
                 [%e sets];
                 a]
             in

             Ast.case
               ~lhs:
                 (Ast.ppat_construct { loc; txt = Lident name }
                    (match args with
                    | [] -> None
                    | _ -> Some (Ast.ppat_tuple args)))
               ~guard:None ~rhs)
           cases)
    in
    (* final function which acts on thing to be converted *)
    let of_final_fn =
      Ast.pexp_fun Nolabel None (Ast.ppat_var { loc; txt = "t" }) body
    in
    (* add printers for type parameters *)
    let expr =
      List.fold_right
        (fun c t ->
          Ast.pexp_fun Nolabel None (Ast.pvar (Format.asprintf "jv_of_%s" c)) t)
        type_params of_final_fn
    in
    [
      Ast.pstr_value is_rec
        [
          Ast.value_binding
            ~pat:(Ast.pvar (Format.asprintf "jv_of_%s" name))
            ~expr;
        ];
    ]
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
