(* Type terme qui est soit une variable Var,
   soit un niveau astraction Abs,
   soit une application entre deux termes
   Les variables sont représentées par des chaines caractères 
 *)
type term = Var of string | Abs of string * term  | App of term * term;;

(* retourne le première élément *)
let vrai = Abs("x",Abs("y",Var "x"));;

(* retourne le second élément *)
let faux =  Abs("x",Abs("y",Var "y"));;

(* v est le booléen, t la solution choisie si vrai et f la solution choisie si false *)
let si = Abs("v",Abs("t",Abs("f",App(App(Var "v",Var "t"),Var "f"))));;


(* Donne l'ensemble des variables liées du terme *)
let rec lie term =
  match term with
    Var var -> []
   |Abs (el,t) -> el::lie t
   |App (t1,t2) -> lie t1;lie t2
;;

(* test *)
let termtest = Abs("coucou",(App(Var "patrick" , Abs("tu" ,Var "coucou"))));;
lie termtest;;

(* Affiche une liste de string (Provisoire)*)
let rec afficherList liste =
  match liste with
    [] -> Printf.printf "\n"
  | e::t -> Printf.printf "%s\n" e ; afficherList t
;;

(* test *)
afficherList ["coucou";"liho";"kugiu"];;

(* Donne l'ensemble des variables libres du terme *)
let libre term =
  let varlie = lie term in
  let rec aux term =
    match term with
      Var var -> if (List.mem var varlie) then [] else [var]
     |Abs (el,t) -> aux t
     |App (t1,t2) -> List.append (aux t1) (aux t2)
  in aux term
;;

(* test *)
libre termtest;;

(* Donne le terme avec ZERO ou UNE êta réduction faite sur le terme *)
let rec eta_red terme =
  match terme with
    Var var -> Var var
   |App (t1,t2) -> App((eta_red t1),(eta_red t2))
   |Abs (el,t) -> match t with
                  | App(t1,Var var) -> if ((String.equal el var) && (not(List.mem el (libre t1))))
                                         then (eta_red t1)
                                          else Abs(el,(eta_red t))
                  | _ -> Abs(el,(eta_red t))
;;

(* test *)
let termeta = App(Abs("x",App(Abs("z",Var "z"),Var "x")),(Abs("x",Var "x")));;
eta_red (eta_red termeta);;

(* A faire 
 béta-réduction
 aplha-réduction
 parser
*)
