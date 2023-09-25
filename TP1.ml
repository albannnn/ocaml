(* EXERCICE 1*)

let norme a b = sqrt(a**2. +. b ** 2.) ;;

let div_euclidienne a b = (a / b, a mod b) ;; 

let rec puissance (x:int) n = match n with
| 0 -> 1 ;
| n -> puissance x (n-1) ;;

let rec fib n = match n with
|0 -> 1 ;
|1 -> 1 ; 
|n -> fib (n-1) + fib(n - 2) ;; 

(* Faire version opti de fibo *)
 
let rec u a b n  =
  match n with
  | 0 -> 0. ;
  | n -> a *. u a b (n-1) +. b ;;
print_float(u 3. 5. 8) 

 
(* EXERCICE 2 *)
let double_premier list = 
  match list with 
  | [] -> [] ;
  | tete :: queue -> [tete * 2] @ queue ;; 


let rec print_int_list list = 
  match list with
  |[] -> ();
  |tete::queue -> print_int tete; 
                  print_string " -> ";
                  print_int_list queue ;;
let rec somme_liste list= 
  match list with
  |[] -> 0 ;
  |tete :: queue -> tete + somme_liste queue ;;

let rec longueur list =
  match list with
  | [] -> 0 ;
  | tete :: queue -> 1 + longueur queue ;;

(* head -> Renvoie la tete de la liste en args *)
let head list = 
  match list with
  |[] -> [] ;
  |head :: tail -> head ;;
(* tail -> renvoie la queue de la liste en args*)
  let tail list =
  match list with 
  |[] -> [] ;
  |head :: tail -> tail ;;

(* Func moyenne avec 2 parcours de liste *)
let moyenne_list list = float_of_int(somme_liste list) /. float_of_int(longueur list) ;;

(* Func moyenne avec 1 seul parcours de la liste à faire*)
let rec creer_nouvelle_liste l1 longueur = 
  match longueur with 
  | 0 -> [] ;
  | others -> head (l1) :: creer_nouvelle_liste(tail (l1)) (longueur - 1) ;;

let rec get_elt list position = 
  match position with 
  |0 -> head list ;
  |autres -> get_elt(tail (list)) (position - 1) ;;

let rec concatener (l1) (l2) = 
  match l1 with
  |[] -> l2
  |l1 -> concatener(creer_nouvelle_liste(l1) (longueur (l1) - 1)) 
                   (get_elt (l1) (longueur (l1) - 1) :: l2) ;; 

let rec miroir (l) = 
  match l with
  | [] -> [] ;
  | [x] -> x :: [] ;
  | tete :: queue ->  concatener (miroir(queue)) (tete :: []) ;;

let l1 = [1;2;3;4;5] ;;
let l2 = [6;7;8;9;10];;


