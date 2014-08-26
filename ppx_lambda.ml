
open Ast_mapper
open Ast_helper
open Asttypes
open Parsetree
open Longident
open Location


let lambda_mapper argv =
  let super = default_mapper in
  let lambda_expr this e =
    match e.pexp_desc with
    (*
     * Unit mapper: `() => e0` -> `fun () -> e0`
     *)
    | Pexp_apply ({pexp_desc = Pexp_ident {txt = Lident "=>"}},
       [("", {pexp_desc = Pexp_construct ({txt = Lident "()"} as id, None)});
        ("", e0)]) ->
      let unit_pat = Pat.construct id None in
      Exp.fun_ "" None unit_pat e0

    (*
     * Apply mapper: `arg0 => e0` -> `fun arg0 -> e0`
     *)
    | Pexp_apply ({pexp_desc = Pexp_ident {txt = Lident "=>"}},
        [("", {pexp_desc = Pexp_ident {txt = Lident arg0; loc = arg0_loc}});
         ("", e0)]) ->
      let arg0_pat = Pat.var (mkloc arg0 arg0_loc) in
      Exp.fun_ "" None arg0_pat e0

    (* Sequence mapper:
     *   `arg0 => e0; e1; ... ; en` -> `fun arg0 -> e0; e1; ...; en`
     *)
    | Pexp_sequence ({pexp_desc =
        Pexp_apply ({pexp_desc = Pexp_ident {txt = Lident "=>"}},
        [("", {pexp_desc = Pexp_ident {txt = Lident arg0; loc = arg0_loc}});
          ("", e0)])}, e1) ->
      let arg0_pat = Pat.var (mkloc arg0 arg0_loc) in
      let seq_expr = Exp.sequence e0 e1 in
      Exp.fun_ "" None arg0_pat seq_expr

    (* default expr *)
    | _ -> super.expr this e

    in { super with expr = lambda_expr }

let () =
  Ast_mapper.register "ppx_lambda" lambda_mapper
