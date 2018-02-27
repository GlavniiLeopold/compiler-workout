(* Opening a library for generic programming (https://github.com/dboulytchev/GT).
   The library provides "@type ..." syntax extension and plugins like show, etc.
*)
open GT 
    
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
    let rec eval_ex op x y = 
    let toBool x = 
		if x = 0 then false else true
    and toInt op_bool x y =
		if op_bool x y then 1 else 0
    in
    match op with
    | "+" -> x + y
    | "-" -> x - y
    | "*" -> x * y
    | "/" -> x / y
    | "%" -> x mod y
    | "<" -> toInt (<) x y
    | "<=" -> toInt (<=) x y
    | ">" -> toInt (>) x y
    | ">=" -> toInt (>=) x y
    | "==" -> toInt (==) x y
    | "!=" -> toInt (<>) x y
    | "&&" -> toInt (&&) (toBool x) (toBool y)
    | "!!" -> toInt (||) (toBool x) (toBool y)

let rec eval st ex = 
    match ex with
    | Const (value) -> value
    | Var (value) -> st value
    | Binop (oper, ex1, ex2) ->
    let val1 = eval st ex1 
    and val2 = eval st ex2 
in eval_ex op val1 val2

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
let rec eval ((s, i, o) : config) stmt =
        match stmt with
        Read (x) -> 
        (match i with 
        [] -> failwith "Empty stream"
        | num :: tail -> ((Expr.update x num s), tail, o))
        | Write (expr) -> (s, i, o @ [(Expr.eval s expr)])
        | Assign (x, expr) -> ((Expr.update x (Expr.eval s expr) s), i, o)
        | Seq (stmt_fst, stmt_snd) -> 
        let value_fst = eval (s, i, o) stmt_fst in
eval value_fst stmt_snd;;
                                                         
  end

(* The top-level definitions *)

(* The top-level syntax category is statement *)
type t = Stmt.t    

(* Top-level evaluator

     eval : int list -> t -> int list

   Takes a program and its input stream, and returns the output stream
*)
let eval i p =
  let _, _, o = Stmt.eval (Expr.empty, i, []) p in o
