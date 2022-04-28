(*Eliott LEBOEUF*)
type 'a t = (int * 'a) Heap.t;;

let make (n: int)  (valeur : (int * 'a))   = 
  Heap.make n valeur   
;;

let length file =
  Heap.length file
;;

let add valeur priorite file = 
  Heap.add (priorite, valeur) file
;;

let take_full file =
  Heap.take file
;;


let take (file : 'a t) : 'a =
  let (priorite, valeur) = Heap.take file in 
  valeur
;;

let peek file =
  let (priorite, valeur) = Heap.peek file in 
  valeur
;;
