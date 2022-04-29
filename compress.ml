open FileIO;;
open HuffmanTree;;

let stats (fn:string) : int array = 
        let tableau_frequences = Array.make 256 0 in
        FileIO.input_byte_iter (fun n -> tableau_frequences.(n) <- tableau_frequences.(n) +1) fn;
        tableau_frequences
;;

let output_code (out : FileIO.out_channel_bits) (l : bool list) =
        List.iter (fun b -> output_bit out b) l
;;


let compress (fn : string) =
        let tab = stats fn in
        let tree = HuffmanTree.build_tree tab in
        let codes = HuffmanTree.codes tree in
        let out = FileIO.open_out_bits (fn ^ ".huffman") in
        HuffmanTree.output_tree out tree;
        FileIO.input_byte_iter (fun x -> output_code out (codes.(x))) fn;
        close_out_bits out
;;
