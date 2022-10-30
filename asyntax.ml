(**
 * expression
 *	Abstract synatx tree (AST) definiton and utilities
*)

let l_intappl = ["-" ; "+" ; "float"]
let l_intop = ["+" ; "-" ; "*" ; "/" ; "%"]
let l_floatappl = ["-" ; "+" ; "int"]
let l_floatop = ["+." ; "-." ; "*." ]


exception Error of string

type exp =
  Int of int                (*Entier*)
  |Float of float           (*Flottant*)
  |Appl of string * exp     (*Apllication*)
  |Op of string * exp * exp (*Opération*)


let rec good_typing exp = (*vérifie le bon typage de l'arbre donné en entrée et renvoie lé type correspondant (entier ou flottant)*)
  match exp with
  Int _ -> "int";
  | Float _ -> "float";
  | Op(s , x, y ) when (List.mem s l_intop ) -> (if (good_typing x = "float" || good_typing y = "float") then raise (Error (s ^ " operator does not match with type float")) else "int")
  | Op(s , x , y) when (List.mem s l_floatop) -> (if (good_typing x = "int") || (good_typing y = "int" ) then raise (Error(s ^ " operator does not match with type int")) else "float")
  | Appl("int", x) -> (if good_typing x = "int" then raise (Error("int function does not match with type int")) else "int")
  | Appl("float", x) -> (if good_typing x = "float" then raise (Error("float function does not match with type float")) else "float")
  | Appl(s, x) when (List.mem s l_intappl) -> (if (good_typing x = "float") 
                                                then raise (Error(s ^ " function does not match with type float"))
                                                      else "int")
  | Appl(s, x) when (List.mem s l_floatappl) -> (if (good_typing x = "int") 
                                                      then raise (Error(s ^ " function does not match with type int"))
                                                      else "float")
  | Appl("()", x) -> good_typing x
  | _ -> raise (Error("Bah non"))
