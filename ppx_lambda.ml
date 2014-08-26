
open Ast_mapper
open Ast_helper
open Asttypes
open Parsetree
open Longident
open Location


let make_1_ary_fun var loc expr =
  let var_pat = Pat.var (mkloc var loc) in
  Exp.fun_ "" None var_pat expr


let make_0_ary_fun loc expr =
  let unit_id = mkloc (Lident "()") loc in
  let unit_pat = Pat.construct unit_id None in
  Exp.fun_ "" None unit_pat expr


let rec make_n_ary_fun args body =
  match args with
  | ((l0, {pexp_desc = Pexp_ident {txt = Lident arg; loc = arg_loc}})::[])
    -> make_1_ary_fun arg arg_loc body

  | ((l0, {pexp_desc = Pexp_ident {txt = Lident arg; loc = arg_loc}})::rest)
    -> make_1_ary_fun arg arg_loc (make_n_ary_fun rest body)

  | [] -> make_0_ary_fun Location.none body

  | _ -> raise (Failure "Bad arguments for function construction.")


let lambda_mapper argv =
  let super = default_mapper in
  let lambda_expr this e =
    match e.pexp_desc with
    (*
     * 0-ary lambda mapper: `() => e0` -> `fun () -> e0`
     *)
    | Pexp_apply ({pexp_desc = Pexp_ident {txt = Lident "=>"}},
       [("", {pexp_desc =
          Pexp_construct ({txt = Lident "()"; loc = unit_loc}, None)});
        ("", e0)]) ->
      make_0_ary_fun unit_loc e0

    (*
     * 1-ary lambda mapper: `arg0 => e0` -> `fun arg0 -> e0`
     *)
    | Pexp_apply ({pexp_desc = Pexp_ident {txt = Lident "=>"}},
        [("", {pexp_desc = Pexp_ident {txt = Lident arg0; loc = arg0_loc}});
         ("", e0)]) ->
      make_1_ary_fun arg0 arg0_loc e0

    (*
     * n-ary lambda mapper: `arg0 ... argN => e0` -> `fun arg0 ... argN -> e0`
     *)
    | Pexp_apply ({pexp_desc = Pexp_ident {txt = Lident "=>"}},
        [("", {pexp_desc = Pexp_apply (arg0_no_lab, arg_rest)});
         ("", e0)]) ->
      make_n_ary_fun (("", arg0_no_lab)::arg_rest) e0

    (* default expr *)
    | _ -> super.expr this e

    in { super with expr = lambda_expr }

let () =
  Ast_mapper.register "ppx_lambda" lambda_mapper
