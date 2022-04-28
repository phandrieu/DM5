# Correction DM 05 : compression de Huffman
#    MP2I au Lycée Kléber, Lilian Besson
#    Avril 2022
#

all:	huffman.exe clean check_compress_and_decompress_proust

huffman.exe:
	ocamlc -o huffman.exe heap.ml priorityQueue.ml fileIO.ml huffmanTree.ml compress.ml decompress.ml huffman.ml

clean:
	rm -vf *.cmo *.cmi

compress_proust:	huffman.exe
	time ./huffman.exe comp proust.txt

decompress_proust:	huffman.exe
	cp proust.txt.huffman proust_decompressed.txt.huffman
	time ./huffman.exe decomp proust_decompressed.txt.huffman

check_compress_and_decompress_proust:	huffman.exe
	make compress_proust
	make decompress_proust
	md5sum proust.txt
	md5sum proust_decompressed.txt
