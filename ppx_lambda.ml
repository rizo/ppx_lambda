
open Ast_mapper
open Ast_helper
open Asttypes
open Parsetree
open Longident
open Location

exception Error of Location.t

let () =
  Location.register_error_of_exn (fun exn ->
    match exn with
    | Error loc ->
      Some (error ~loc "[%getenv] accepts a string, e.g. [%getenv \"USER\"]")
    | _ -> None)

let mkpat_unit loc =
  let pat_id = mkloc (Lident "()") loc in
  Pat.mk ~loc (Ppat_construct(pat_id, None))

let mkpat_txt (txt : string) (loc : Location.t) =
  Pat.mk ~loc (Ppat_var { txt; loc })



let create_apply_expr ident exprs =
  Pexp_apply
    ({pexp_desc = Pexp_ident {txt = Lident ident; loc = Location.none};
      pexp_loc = Location.none;
      pexp_attributes = []},

      List.map (fun e -> ("", e)) exprs)


let lambda_mapper =
  fun mapper expr -> match expr with

  (*
   * 1-ary Lambda Mapper
   *)

  (*
   * ~op_l0:(fn_e0 => fn_e1) && ~op_l1:op_e1
   *)
  | {pexp_desc =
      Pexp_apply ({pexp_desc = Pexp_ident {txt = Lident op}},
       [(op_l0,
         {pexp_desc =
           Pexp_apply ({pexp_desc = Pexp_ident {txt = Lident "=>"}},
            [("", {pexp_desc = Pexp_ident {txt = Lident fn_e0}});
             ("", fn_e1)])});
        (op_l1, op_e1)])} when (op = "&&" || op = "&" || op = "||" || op = "or")
    ->
    let op_ident = (Exp.ident (mknoloc (Lident op))) in
    let lambda_body = (Exp.apply op_ident [("", fn_e1); ("", op_e1)]) in
    Exp.fun_ "" None (Pat.var (mknoloc fn_e0)) lambda_body

  (* Pass `expr` to default mapper. *)
  | x -> default_mapper.expr mapper x


let getenv_mapper argv = { default_mapper with expr = lambda_mapper }

let () = run_main getenv_mapper

