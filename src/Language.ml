(* Opening a library for generic programming (https://github.com/dboulytchev/GT).
   The library provides "@type ..." syntax extension and plugins like show, etc.
*)
open GT

(* Opening a library for combinator-based syntax analysis *)
open Ostap.Combinators

(* Simple expressions: syntax and semantics *)
module Expr =
  struct

    (* The type for expressions. Note, in regular OCaml there is no "@type..."
       notation, it came from GT.
    *)
    @type t =
    (* integer constant *) | Const of int
    (* variable         *) | Var   of string
    (* binary operator  *) | Binop of string * t * t with show

    (* Available binary operators:
        !!                   --- disjunction
        &&                   --- conjunction
        ==, !=, <=, <, >=, > --- comparisons
        +, -                 --- addition, subtraction
        *, /, %              --- multiplication, division, reminder
    *)

    (* State: a partial map from variables to integer values. *)
    type state = string -> int

    (* Empty state: maps every variable into nothing. *)
    let empty = fun x -> failwith (Printf.sprintf "Undefined variable %s" x)

    (* Update: non-destructively "modifies" the state s by binding the variable x
      to value v and returns the new state.
    *)
    let update x v s = fun y -> if x = y then v else s y

    let intToBool = (<>) 0
    let boolToInt v = if v then 1 else 0

    let ($$) f g x y = f (g x) (g y)

    let parseOperation op = match op with
      | "+" -> (+)
      | "-" -> (-)
      | "*" -> ( * )
      | "/" -> (/)
      | "%" -> (mod)
      | _   ->
        let parseBooleanOp = match op with
          | "==" -> (=)
          | "!=" -> (<>)
          | "<=" -> (<=)
          | "<"  -> (<)
          | ">=" -> (>=)
          | ">"  -> (>)
          | "!!" -> (||) $$ intToBool
          | "&&" -> (&&) $$ intToBool
          | _    -> failwith "Unsupported operator"
        in fun x y -> boolToInt @@ parseBooleanOp x y



    (* Expression evaluator

          val eval : state -> t -> int

       Takes a state and an expression, and returns the value of the expression in
       the given state.
    *)
    let eval st e =
      let rec eval' st e = match e with
        | Const c         -> c
        | Var name        -> st name
        | Binop(op, x, y) ->
          let x = eval' st x and y = eval' st y
          in (parseOperation op) x y
      in eval' st e

    (* Expression parser. You can use the following terminals:

         IDENT   --- a non-empty identifier a-zA-Z[a-zA-Z0-9_]* as a string
         DECIMAL --- a decimal constant [0-9]+ as a string

    *)

    let opList l = List.map (fun s -> (ostap ($(s)), fun x y -> Binop(s, x, y))) l

    ostap (
      primary: x:IDENT {Var x} | x:DECIMAL {Const x} | -"(" parse -")";
      parse: !(Ostap.Util.expr
                (fun x -> x)
                [|
                  `Lefta, opList ["!!"];
                  `Lefta, opList ["&&"];
                  `Nona,  opList [">="; ">"; "<="; "<"; "=="; "!="];
                  `Lefta, opList ["+"; "-"];
                  `Lefta, opList ["*"; "/"; "%"]
                |]
                primary
              )
    )

  end

(* Simple statements: syntax and sematics *)
module Stmt =
  struct
    open List

    (* The type for statements *)
    @type t =
    (* read into the variable           *) | Read   of string
    (* write the value of an expression *) | Write  of Expr.t
    (* assignment                       *) | Assign of string * Expr.t
    (* composition                      *) | Seq    of t * t with show

    (* The type of configuration: a state, an input stream, an output stream *)
    type config = Expr.state * int list * int list

    (* Statement evaluator

          val eval : config -> t -> config

       Takes a configuration and a statement, and returns another configuration
    *)
    let rec eval (state, is, os) = function
      | Read var -> Expr.update var (hd is) state, tl is, os
      | Write expr -> state, is, os @ [Expr.eval state expr]
      | Assign (var, expr) -> Expr.update var (Expr.eval state expr) state, is, os
      | Seq (expr1, expr2) -> eval (eval (state, is, os) expr1) expr2


    (* Statement parser *)
    ostap (
      primary: -"read"  -"(" v:IDENT -")" { Read v }
             | -"write" -"(" e:!(Expr.parse) -")" { Write e }
             | v:IDENT -":=" e:!(Expr.parse) { Assign (v, e) };
      parse: !(Ostap.Util.expr
                (fun x -> x)
                [|
                  `Righta, [ostap (";"), fun x y -> Seq(x, y)]
                |]
                primary
              )
    )

  end

(* The top-level definitions *)

(* The top-level syntax category is statement *)
type t = Stmt.t

(* Top-level evaluator

     eval : t -> int list -> int list

   Takes a program and its input stream, and returns the output stream
*)
let eval p i =
  let _, _, o = Stmt.eval (Expr.empty, i, []) p in o

(* Top-level parser *)
let parse = Stmt.parse
