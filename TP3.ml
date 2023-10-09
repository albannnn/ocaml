(* Exercice 1 *)
type coloration = 
|Cyan 
|Magenta 
|Jaune
|Melange of coloration * coloration ;; 

let rouge = Melange(Magenta, Jaune) ;;
let orange = Melange(rouge, Jaune) ;;

(* Exercice 2 *)
type 'a liste = 
  |Nil 
  |TeteEtQueue of 'a * 'a liste
;;
let liste1 = TeteEtQueue(1, TeteEtQueue(2, TeteEtQueue(3, Nil))) ;; 
let cons tete queue = 
  TeteEtQueue (tete, queue) ;;

let liste2 =  cons(0, liste1) ;; 

let tete liste = 
  match liste with
  |Nil -> failwith "Ah non mais la tu deconnes complet, t'as pas compris, mais c'est simple pourtant !!!!!!" ;
  |TeteEtQueue(tete, queue) -> tete ;;

  let queue liste = 
    match liste with
    |Nil -> failwith "Ah non mais la tu deconnes complet, t'as pas compris, mais c'est simple pourtant !!!!!!" ;
    |TeteEtQueue(tete, queue) -> queue ;;
  
let rec taille liste = 
  match liste with
  |Nil -> 0 ;
  |TeteEtQueue(tete, queue) -> 1 + taille(queue) ;; 

let rec appartient liste elt = 
  match liste with 
  |Nil -> false;
  |TeteEtQueue(tete, _) when tete = elt -> true ;
  |TeteEtQueue(_, queue) -> appartient queue elt ;;

let rec supprime liste elt =
  match liste with
  |Nil -> Nil ;
  |TeteEtQueue(tete, queue) -> 
    if tete = elt then 
      supprime queue elt 
    else
      TeteEtQueue(tete, supprime(queue)(elt)) ;;

let rec element_no liste no =
  match no with 
  |n when n < 0 -> failwith "Depuis quand une liste est plus petite que l'index gros????"
  |0 -> tete liste ;
  |n -> element_no (queue liste) (n - 1);;

  

