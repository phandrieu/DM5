open Heap;;

type 'a t = (int * 'a) Heap.t

let make i el = 
        Heap.make i el
;;

let length file = 
        Heap.length file
;;

(*Fonction aux : comparaison de deux éléments (prio, el) pour pouvoir utiliser le tri*)
let comparaison (prio1, el1) (prio2, el2) = 
        if prio1 > prio2 then 1
        else if prio1 < prio2 then -1
        else 0  (*si égaux*)
;;


let add el p file = 
        Heap.add (p, el) file
;;

let take_full file = 
        Heap.take file
;;

let take file = 
        let full = take_full file in
        match full with
        |(p, v) -> v
;;

let peek file = 
        let full = Heap.peek file in
        match full with
        |(p, v) -> v
;;

