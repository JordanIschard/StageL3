
type expr =
  Var of string
| Abs of string * expr
| App of expr * expr
| Const of int
| Op of string * (expr list)
;;

type context = Expr of expr | Trou ;;

type  pile = context list;;

type machineCC = Machine of expr * pile ;;

exception Nombre_De_Param_Erreur;;
exception Etat_Anormal;;
exception Pile_Vide;;
exception Operande_Inconnu;;

let estVariable expr =
  match expr with
    Var _ -> true
  | Abs _ -> true
  | Const _ -> true
  | _ -> false
;;

let machine = Machine ( Var "ug", []);;

let calcul op liste =
  if (String.equal op "++")
  then match liste with
            [o] -> (Const (o + 1))
          | _ -> raise Nombre_De_Param_Erreur
  else
    if (String.equal op "+")
    then match liste with
           [o1;o2] -> (Const (o1 + o2))
         | _ ->  raise Nombre_De_Param_Erreur
    else raise Operande_Inconnu
;;

let rec liste_var liste =
  match liste with
  |[] -> true
  |(Var h)::t -> liste_var t
  |_ -> false
;;

let rec liste_const liste =
  match liste with
  |[] -> true
  |(Const h)::t -> liste_const t
  |_ -> false
;;

let evalcc expression =
  let machine = Machine ( expression , []) in (eval machine);;

(* En cours *)
let rec eval machine =
  let aux machine =
    match machine with
      (App(expr1,expr2),pile) -> if (estVariable expr1)
                                 then if (estVariable expr2)
                                      then raise Etat_Anormal
                                      else (expr1,(App(Trou,expr2))::pile)
                                       
                                 else  (expr2,(App(expr1,Trou))::pile)

    | (Op(operateur,liste),pile) -> if (liste_const liste)
                                    then calcul operateur liste
                                    else
                                      if (liste_var liste)
                                      then raise Etat_Anormal
                                      else  (*Construire le couple avec le
                                                         premier terme non var extrait*)

    | (App(Abs( var,expr1),expr2),pile) -> ((beta_red (App(Abs( var,expr1),expr2))),pile)

                                       
    | (expr1,(App(expr2,Trou))::pile) -> if (estVariable expr1)
                                         then (App(expr2,expr1),pile)
                                         else (expr1,(App(expr2,Trou))::pile)


    | (expr1,(App(Trou,expr2))::pile) -> if (estVariable expr1)
                                         then (App(expr1,expr2),pile)
                                         else (expr1,(App(Trou,expr2))::pile)

                                       

    | (expr1,(Op(operateur,liste))::pile) -> if (estVariable expr1)
                                              then (* Reconstruire la liste *)
                                              else (expr1,(Op(operateur,liste))::pile)
                                 

  in
  if estReduit machine
  then fst machine
  else eval machine

  ;;
                                                    
                                                    
