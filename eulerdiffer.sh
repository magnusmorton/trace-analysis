#!/usr/bin/env zsh
sizes=(1 2 4 8 16 32 64 128 256 512 1024 2048 4096)
lengths=(1000 2000 4000)

DIR=trans_benchmarks

for length in $lengths
do
    echo $length
    #seq
    python differ.py ${DIR}/eulerseqwu${length} ${DIR}/eulerseqtask${length}

    #transformed
    for size in $sizes
    do
	echo "${size} chunk"
	python differ.py ${DIR}/eulerchunkwu${length}x${size} ${DIR}/eulerchunktask${length}x${length}

	echo "${size} stride"
	python differ.py ${DIR}/eulerstridewu${length}x${size} ${DIR}/eulerstridetask${length}x${length}
    done
done
