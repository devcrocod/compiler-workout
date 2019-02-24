open GT
open List  
       
(* The type for the stack machine instructions *)
@type insn =
(* binary operator                 *) | BINOP of string
(* put a constant on the stack     *) | CONST of int                 
(* read to stack                   *) | READ
(* write from stack                *) | WRITE
(* load a variable to the stack    *) | LD    of string
(* store a variable from the stack *) | ST    of string with show

(* The type for the stack machine program *)                                                               
type prg = insn list

(* The type for the stack machine configuration: a stack and a configuration from statement
   interpreter
 *)
type config = int list * Syntax.Stmt.config

(* Stack machine interpreter

     val eval : config -> prg -> config

   Takes a configuration and a program, and returns a configuration as a result
 *)
let rec eval conf prg = 
	match prg with
	| x :: xl -> eval (step conf x) xl
	| [] -> conf
	and step (stack, (state, input, output)) instr = 
		match instr with
		| BINOP operate -> ((Syntax.Expr.oper operate) (hd (tl stack)) (hd stack) :: (tl (tl stack)), (state, input, output))
		| CONST c -> (c :: stack, (state, input, output))
		| READ -> (hd input :: stack, (state, tl input, output))
		| WRITE -> (tl stack, (state, input, output @ [hd stack]))
		| LD x -> (state x :: stack, (state, input, output))
		| ST x -> (tl stack, (Syntax.Expr.update x (hd stack) state, input, output))


(* Top-level evaluation

     val run : int list -> prg -> int list

   Takes an input stream, a program, and returns an output stream this program calculates
*)
let run i p = let (_, (_, _, o)) = eval ([], (Syntax.Expr.empty, i, [])) p in o

(* Stack machine compiler

     val compile : Syntax.Stmt.t -> prg

   Takes a program in the source language and returns an equivalent program for the
   stack machine
 *)

let compile _ = failwith "Not yet implemented"
let rec compile stmt = 
	match stmt with
		| Syntax.Stmt.Read x -> [READ; ST x]
		| Syntax.Stmt.Write x -> comp_expr x @ [WRITE]
		| Syntax.Stmt.Assign (x, y) -> comp_expr y @ [ST x]
		| Syntax.Stmt.Seq (t1, t2) -> compile t1 @ compile t2
	and comp_expr ex = 
		match ex with
		| Syntax.Expr.Const c -> [CONST c]
		| Syntax.Expr.Var x -> [LD x]
		| Syntax.Expr.Binop (operate, left, right) -> comp_expr left @ comp_expr right @ [BINOP operate]
