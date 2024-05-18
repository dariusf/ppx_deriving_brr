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

let generate_type_decl t =
  (* TODO mutually recursive types *)
  let td = List.hd t in
  let { loc; txt = name } = td.ptype_name in
  let (module Ast) = Ast_builder.make loc in
  let jv_conversion_for loc typ_name expr =
    match typ_name with
    | "int" -> [%expr Jv.of_int [%e expr]]
    | "string" -> [%expr Jv.of_string [%e expr]]
    | "float" -> [%expr Jv.of_float [%e expr]]
    | "bool" -> [%expr Jv.of_bool [%e expr]]
    | _ ->
      let fn =
        Ast.pexp_ident
          { loc; txt = Lident (Format.asprintf "jv_of_%s" typ_name) }
      in
      [%expr [%e fn] [%e expr]]
  in
  match td.ptype_kind with
  | Ptype_record fields ->
    (*
       let jv_of_a {f1} =
         Jv.obj [| "f1", jv_of_f1t f1 |]
    *)
    let pat =
      Ast.ppat_record
        (List.map
           (fun f ->
             let name = f.pld_name.txt in
             (* let typ = f.pld_type in *)
             ({ loc; txt = Lident name }, Ast.ppat_var { loc; txt = name }))
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
                   let typ_name =
                     match f.pld_type.ptyp_desc with
                     | Ptyp_constr ({ txt = Lident l; loc = _ }, _args) -> l
                     | _ -> failwith "unknown kind of type"
                   in
                   let arg = Ast.pexp_ident { loc; txt = Lident name } in
                   Ast.pexp_tuple
                     [Ast.estring name; jv_conversion_for loc typ_name arg])
                 fields)]]
    in
    [
      Ast.pstr_value Nonrecursive
        [
          Ast.value_binding
            ~pat:(Ast.ppat_var { loc; txt = Format.asprintf "jv_of_%s" name })
            ~expr:(Ast.pexp_fun Nolabel None pat body);
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
                     (fun i f ->
                       let arg_name =
                         match f.ptyp_desc with
                         | Ptyp_constr ({ txt = Lident l; loc = _ }, _) -> l
                         | _ -> failwith "nyi"
                       in
                       let f = Ast.evar (Format.asprintf "v%d" i) in
                       let v = jv_conversion_for loc arg_name f in
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
    [
      Ast.pstr_value Nonrecursive
        [
          Ast.value_binding
            ~pat:(Ast.ppat_var { loc; txt = Format.asprintf "jv_of_%s" name })
              (* ~expr:(Ast.pexp_constant (Pconst_integer ("1", None))); *)
            ~expr:
              (Ast.pexp_fun Nolabel None (Ast.ppat_var { loc; txt = "t" }) body);
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
