open Compress;;
open Decompress;;

let main () =
        if Sys.argv.(1) = "comp"
        then 
                begin
                        Compress.compress Sys.argv.(2);
                end
        else if Sys.argv.(1) = "decomp"
        then Decompress.decompress Sys.argv.(2)
        else Printf.printf "Usage: huffman comp/decomp fichier.ext \n"
;;

main();;