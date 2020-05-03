(*
Question 1
Définir les types OCaml, pour les types et les expressions du langage mini-ML.
 *)
type t_type =
  T_Int 
| T_Char
| T_Bool
| T_Fun of t_type * t_type     (*  t_type -> t_type   *)
| T_Paire of t_type * t_type   (*   (t_type , t_type)   *)
;;

 (*Constantes
	les constantes ne sont pas limités au petit ensemble du projet
*)
type t_const =
  Bool of bool
| Char of char
| Int of int
;;

(*operations*)
type t_operation =
  Plus                          (* int * int -> int *)
| Minus                         (* int * int -> int *)
| Greater                      (* int * int -> bool *)
| Lower                         (* int * int -> bool *)
| If_then_else     (* if_then_else bool * (int * int) -> int *)
;;

(*type expretions*)
type t_expr =
  |  Variable of char                          (*Variable*)
  | Const of t_const                         (*  Constantes  *)
  | Op of t_operation                        (*  Operation   *)
  | App of t_expr * t_expr                    (* application de fonction*)
  | Fun of char * t_type * t_expr           (*   Abstraction de fonction   Fun x : t_type -> t_expr *)
  | Pair of t_expr * t_expr                   (*   (expr , expr )    *)
  | Let of char * t_type * t_expr * t_expr           (*    let x : t_type = t_expr in  t_expr *)
;;

(*
2) Définir une fonction "d’affichage" des expressions mini-ML qui prend une expression et retourne
l’expression sous la forme d’une chaîne de caractères, écrite de manière habituelle *)
open Pervasives;;

let rec printType (t : t_type) : string =
  match t with    
    T_Int -> "int"
  | T_Char -> "char"
  | T_Bool -> "bool" 
  | T_Fun (left, right) -> printType left ^ " -> " ^ printType right
  | T_Paire (left, right) -> "(" ^ printType left ^ " * " ^ printType right ^ ")"
;;

let rec print (expr : t_expr ) : string =
  match expr with
  | Variable x -> String.make 1 x
  | Const const ->
     (
       match const with
         Bool b -> string_of_bool b
       | Int i -> string_of_int i
       | Char c -> String.make 1 c
     )
  | Op operation ->
     (
       match operation with
         Plus -> " + "
       | Minus -> " - "
       | Greater -> " > "
       | Lower -> " < "
       | If_then_else -> " if_then_else "
     )
  | App (a1, a2) -> print a1 ^ " " ^ print a2
  | Fun ( arg , typeArg, corps ) -> " fun " ^ (String.make 1 arg) ^ " : " ^ printType typeArg ^ " -> " ^ print corps
  | Pair(left,right) -> "( " ^ print left ^ " , " ^ print right ^ " )"
  | Let (var, typeVar, equals , dans ) -> " let " ^ (String.make 1 var) ^ " : " ^ printType typeVar ^ " = " ^ print equals ^ " in " ^ print dans
;;
(*
Question 3
Nous allons utiliser les listes d’associations OCaml pour représenter les environnements.
Définir les environnements Ec et Eop comprenant respectivement les constantes et les opérations
primitives du langage mini-ML.
 *)

type t_env = ( t_expr * t_type) list;;

(*    pas besoin l'environnement de Constantes, car le type des constantes peut contenir tous les contanstantes de int, bool, char.
      Sa definition n'est pas limité au ensemble du projet
let (envC : t_env  ) =
  [
    ( Const(Int 1) , T_Int);
    ( Const(Int 2) , T_Int);
    ( Const(Int 3) , T_Int);
    ( Const(Int 4) , T_Int);
    ( Const(Int 5) , T_Int);
    ( Const(Bool true) , T_Bool);
    ( Const(Bool false) , T_Bool);
    ( Const(Char 'a') , T_Char);
    ( Const(Char 'b') , T_Char);
    ( Const(Char 'c') , T_Char);
    ( Const(Char 'd') , T_Char);
  ];;
*)
let (envOp : t_env ) =
  [
    (Op Plus  , T_Fun ( T_Paire(T_Int, T_Int ), T_Int))  ;        
    (Op Minus , T_Fun( T_Paire(T_Int, T_Int ), T_Int)) ;
    (Op Greater , T_Fun( T_Paire(T_Int, T_Int ), T_Bool)) ;
    (Op Lower  , T_Fun( T_Paire(T_Int, T_Int ), T_Bool)) ;
    (Op If_then_else , T_Fun ( T_Paire ( T_Bool , T_Paire(T_Int, T_Int ) ), T_Int) ) ;
  ];;


(* environnement general
 cet environnement est limité aux operations et variables
 car l'environnement de constantes et inclu par defaut dans la definition du type t_expr
*)
let (environnement : t_env) = envOp;;

(*
Question 4
Définir une fonction de vérification de type, qui prend en argument un environnement et une
expression mini-ML et retourne le type de l’expression si l’expression est bien typée et lève une
erreur sinon. Cette fonction mettra en oeuvre les règles d’inférence précédentes.
*)

let rec verif (env : t_env) ( expr : t_expr ) : t_type =
  match expr with
  | Variable x -> List.assoc (Variable x) env
  | Const c ->
     (
       match c with
         Bool _  -> T_Bool
       | Char _ -> T_Char
       | Int _ -> T_Int
     )
  | Pair (left, right) -> T_Paire(verif env left, verif env right )
  | Op operation -> List.assoc (Op operation) env 
  | App (a1, a2) ->
     (
       let typeA1 = verif env a1
       and typeA2 = verif env a2 in
       
       match typeA1 with
       | T_Fun(argsType, resType) ->
          (
            if typeA2 =  argsType
            then
             resType
            else
              failwith "a2 n'est pas de bon type"
          )
       | _ -> failwith "Error d'application des fonction"
     )
  | Let (var, typeVar, a1, a2) ->
     (
       let typeA1 = verif env a1 in
       if typeA1 <> typeVar
       then failwith "a1 n'est pas de meme type que la declaration"
       else
         (
           let newEnv = (Variable var, typeVar ) :: env in
           let typeA2 = verif newEnv a2 in
           typeA2
         )   
     )
  | Fun(arg, typeArg, corps ) ->
     (
       let newEnv = (Variable arg, typeArg) :: env in
       let typeCorps = verif newEnv corps in
       T_Fun(typeArg, typeCorps)
     )
;;
