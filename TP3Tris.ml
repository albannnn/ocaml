(* Quelques algos de tris *)
(* Exercice 9 *)
let l1 = [] ;;
let l2 = [5] ;;
let l3 = [2;4;6;8;10] ;;

let l3bis = [1;3;5;7;9;11];;
let l4 = [5;4;3;2;1] ;; 
let l5 = [5;5;5;5;5] ;;
let l6 = [5;6;4;9;5] ;; 
let l7 = [9;3;2;7;0] ;;

(* TRI PAR SELECTION *)
let min a b = if a < b then a else b ;;
let rec minList liste = 
  (* Renvoie le minimum de l *)
  match liste with
  |[] -> failwith "une liste vide n'a pas de min gros ?!?!??"; 
  |[x] -> x ;
  |head :: queue -> min (head) (minList queue) ;;

let rec retireElt liste elt =
  (* retire la premiere occurence de elt dans l*)
  match liste with 
  |[] -> [] ;
  |head :: queue when head = elt -> queue ;
  |head :: mid :: queue when mid = elt -> head :: queue ;
  |head :: queue -> head :: (retireElt queue elt) ;; 
  
let rec triSelection liste = 
  match liste with 
  |[] -> [];
  |liste -> 
    let min =  minList liste in 
    let listeBis = retireElt liste min in 
    min :: triSelection listeBis ;; 
    
(* TRI FUSION *)
let rec division liste = 
  match liste with
  |[] -> ([] , []) ; 
  |[a] -> [a], [] ; 
  |head :: mid :: queue -> 
    let t1, t2 = division queue in
    (head :: t1, mid :: t2) ;;

let rec fusion l1 l2 = 
  (*renvoie la fusion de deuxlistes triées par ordre croissant*)
  (* Fonctionne correctement uniquement si les 2 listes sont triées *)
  match l1, l2 with 
  |[], l2 -> l2 ;
  |l1, [] -> l1; 
  |head1 :: queue1, head2 :: queue2 ->
    if head1 < head2 then
      head1 :: (fusion queue1 l2 ) 
    else 
      head2 :: (fusion l1 queue2 );; 
let rec triFusion l = 
  match l with
  |[] -> [];
  |head :: [] -> l ; 
  |l -> 
    let l1, l2 = division(l) in 
    fusion (triFusion l1) (triFusion l2)  ;; 
