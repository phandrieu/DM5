open FileIO;;
open HuffmanTree;;

let rec input_code (inp : FileIO.in_channel_bits) (arb : HuffmanTree.tree )=
        match arb with
        |Char c -> c
        |Node(l,r) -> 
                if(FileIO.input_bit inp)
                        then input_code inp r
                        else input_code inp l
;;

let decompress (fn : string) =
        let out = open_out_bits (String.sub fn 0 (String.length fn -8)) in
        let inp = open_in_bits fn in
        let arbre = HuffmanTree.input_tree inp in
        try 
                while true do
                        FileIO.output_byte out (input_code inp arbre);
                        done;
        with End_of_file -> ();
        close_in_bits inp;
        close_out_bits out
;;