open FileIO;;
open HuffmanTree;;

let int_to_bool = function 0 -> false | 1 -> true | a -> print_int a;  failwith "?" ;;

let bool_liste_of_int (nbr_ : int) =
  let nbr = ref nbr_ in
  let res = ref [] in

  while !nbr <> 0 do
    res := (int_to_bool (!nbr mod 2)) :: (!res);
    nbr := (!nbr) / 2;
  done;
  !res
;;


let rec input_code (inp : FileIO.in_channel_bits) (arb : HuffmanTree.tree )=
        match arb with
        |Char c -> c
        |Node(l,r) -> 
                if(FileIO.input_bit inp)
                        then input_code inp r
                        else input_code inp l
                ;;


(*let __input_code (inp : FileIO.in_channel_bits) (arb : HuffmanTree.tree) =

        let codes = HuffmanTree.codes arb in
        let i = ref 0 in
        while (!i < 256) && (codes.(!i) <> octets) do
                i := !i +1;
        done;
        !i
;;*)

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

