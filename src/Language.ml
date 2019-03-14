(* Opening a library for generic programming (https://github.com/dboulytchev/GT).
   The library provides "@type ..." syntax extension and plugins like show, etc.
*)
open GT
open List

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

    (* Expression evaluator
          val eval : state -> t -> int
 
       Takes a state and an expression, and returns the value of the expression in 
       the given state.
     *)                                                       
    let rec eval st ex = 
      match ex with
      | Const c -> c
      | Var x -> st x
      | Binop (operate, left_exp, right_exp) -> oper operate (eval st left_exp) (eval st right_exp)
      and oper operate left right =
        match operate with
        | "!!" -> from_bool_to_int ((from_int_to_bool left) || (from_int_to_bool right))
        | "&&" -> from_bool_to_int ((from_int_to_bool left) && (from_int_to_bool right))
        | "==" -> from_bool_to_int (left == right)
        | "!=" -> from_bool_to_int (left != right)
        | "<=" -> from_bool_to_int (left <= right)
        | "<" -> from_bool_to_int (left < right)
        | ">=" -> from_bool_to_int (left >= right)
        | ">" -> from_bool_to_int (left > right)
        | "+" -> left + right
        | "-" -> left - right
        | "*" -> left * right
        | "/" -> left / right
        | "%" -> left mod right
        | _ -> failwith "Error operation"
      and from_int_to_bool x = x != 0
      and from_bool_to_int x = if x then 1 else 0

    (* Expression parser. You can use the following terminals:
         IDENT   --- a non-empty identifier a-zA-Z[a-zA-Z0-9_]* as a string
         DECIMAL --- a decimal constant [0-9]+ as a string
                                                                                                                  
    *)
    ostap (
      expr:
      !(Ostap.Util.expr
        (fun x -> x)
        (Array.map (fun (assoc, ops) -> assoc, List.map (fun op -> ostap (- $(op)), fun a b -> Binop (op, a, b)) ops) [|
          `Lefta, ["!!"];
          `Lefta, ["&&"];
          `Nona, ["<="; "<" ; ">="; ">"; "=="; "!="];
          `Lefta, ["+"; "-"];
          `Lefta, ["*"; "/"; "%"];
        |])
        primary
      );
      primary: x:IDENT {Var x} | n:DECIMAL {Const n} | -"(" expr -")"
    )

  end
                    
(* Simple statements: syntax and sematics *)
module Stmt =
  struct

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
    let rec eval (state, input, output) stat = 
      match stat with
      | Read x -> (Expr.update x (hd input) state, tl input, output)
      | Write x -> (state, input, Expr.eval state x :: output)
      | Assign (x, y) -> (Expr.update x (Expr.eval state y) state, input, output)
      | Seq (t1, t2) -> eval (eval (state, input, output) t1) t2

    (* Statement parser *)
    ostap (
      statement:
        "read" "(" x:IDENT ")" {Read x}
        | "write" "(" e:!(Expr.expr) ")" {Write e}
        | x:IDENT ":=" e:!(Expr.expr) {Assign (x, e)};

      parse: line:statement ";" tail:parse {Seq (line, tail)} | statement
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