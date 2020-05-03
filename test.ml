(*================= exemples d'utilisation printType ========================== *)
printType (T_Fun ( T_Int , T_Int ));;
printType (T_Fun ( T_Fun(T_Int , T_Char) , T_Bool ));;
printType ( T_Paire( T_Int, T_Bool) );;




(*===============   exemples d'utilisation print ===========================*)
print (Let ('r' , T_Bool , Const (Bool true) ,
            (App( Op ( If_then_else), ( Pair( Variable 'r' , Pair( Const( Int 3) ,Const( Int 4))  ) )  )) ));;
print (App( Op ( If_then_else), ( Pair( Const (Bool true) , Pair( Const( Int 9) ,Const( Int 2))  ) )  ));;
print (App( Op( Greater ) , Pair(Const (Int 3) , Const (Int 4) )));;




(*====================   TEST Verification de types  =================================*)

let testFun = (Fun('f', T_Int, App(Op Greater, Pair(Variable 'f', Const(Int 0)) ) ));;
print testFun;;
verif environnement testFun;;

(*equivalent à : *)
fun (f:int) -> f > 0;;


let anotherFun = Fun('x', T_Int, (App(Op Plus, Pair(Const (Int 4 ), Variable 'x'))) );;
print anotherFun;;
verif environnement anotherFun;;

(*equivalent à : *)
fun (x:int) -> 4+x;;


let testLet = Let('x', T_Int, Const(Int 4),  App( Op(Plus), Pair(Const (Int 4), Variable('x') ) ) ) ;;
print testLet;;
verif environnement testLet;;
(*equivalent à :*)
let x : int = 4 in 4+x;;


let anotherLet = Let('a', T_Bool, Const(Bool true), Const(Int 2));;
print anotherLet;;
verif environnement anotherLet;;
(*equivalent à*)
let another : bool = true in 2;;

let appTest = App (Op Greater, Pair(Const (Int 3), Const(Int 4)) );;
print appTest;;
verif environnement appTest;;

let anotherApp = App (Op If_then_else, Pair( (App (Op Greater, Pair(Const (Int 3), Const(Int 4)) )) , Pair (Const (Int 3), Const(Int 4))));;
print anotherApp;;
verif environnement anotherApp;;

(*type if then else *)
let operationCondition = (Op If_then_else);;
let typeCondition =  verif environnement operationCondition;;
printType typeCondition;;

(*another tests*)
verif environnement (Pair(Const (Int 3), Const (Bool true) ));;
verif environnement (Pair(Const (Int 8), Const (Bool true) ));;

verif environnement (App( Op ( Plus), (Pair(Const (Int 3), Const (Bool true) )))) ;; (*la somme d'un int et bool n'est pas definie*)
verif environnement (App( Op ( Plus), (Pair(Const (Int 3), Const (Int 23) )))) ;;

verif environnement (Const (Int 1) );;

(*         ==============================             BONUS         ==================================================*)
fun (x : char) -> let succ : int -> int = fun x : int -> x+1 in (succ 1, x);;

let bonus = (Fun('x', T_Char, ( Let ('s', T_Fun(T_Int, T_Int),
                        Fun('x', T_Int, App(Op Plus, Pair(Variable('x'), Const (Int(1)) ) )) ,
                        Pair(App(Variable 's' , Const (Int 1) ) , Variable 'x' )
                     )
                 )
  ));;
print bonus;;
let typeBonus = verif environnement bonus;;
printType typeBonus;;

fun (x : char) -> let succ : int -> int = fun y : int -> y+1 in (succ 1, x);;
let bonus2 =(Fun('x', T_Char, ( Let ('s', T_Fun(T_Int, T_Int),
                        Fun('y', T_Int, App(Op Plus, Pair(Variable('y'), Const (Int(1)) ) )) ,
                        Pair(App(Variable 's' , Const (Int 1) ) , Variable 'x' )
                     )
                 )
  ));;
print bonus2;;
let typeBonus2 = verif environnement bonus2;;
printType typeBonus2;;
