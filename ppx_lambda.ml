
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


let getenv_mapper argv =
  { default_mapper with
    expr = fun mapper expr ->
      match expr with
      | {
          pexp_desc = Pexp_apply
          (
            { pexp_desc = Pexp_ident { txt = Lident "=>" } },
            ("", { pexp_desc = Pexp_ident { txt = Lident v0 } }) ::
            ("", body) :: rest
          )
        }
        -> print_endline ("** Pexp_apply/Pexp_ident.txt = \"=>\": matched\n" ^
                          "** v0 = " ^ v0);

           Exp.function_ ~loc:Location.none ~attrs:[]
            [{ pc_lhs = mkpat_txt v0 Location.none;
               pc_guard = None;
               pc_rhs = body;
            }]
           (* Exp.constant (Const_string ("hello :D", None)) *)

      (* | { pexp_desc = Pexp_apply
          ({ pexp_desc = Pexp_ident { txt = Lident "~>" } }, [_, body])}
        ->
          let {pexp_desc; pexp_loc} = body in
          
          Exp.function_ ~loc:pexp_loc ~attrs:[]
            [{ pc_lhs = mkpat_unit pexp_loc;
               pc_guard = None;
               pc_rhs = body; }]
        (* -> Exp.constant (Const_string ("hello :D", None)) *) *)
      | x -> default_mapper.expr mapper x;
  }

let () = run_main getenv_mapper

