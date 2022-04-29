open Heap;;

type 'a t = (int * 'a) Heap.t

let make i el = 
        Heap.make i el
;;

let length file = 
        Heap.length file
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

