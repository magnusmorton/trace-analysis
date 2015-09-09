sizes=(1 2 4 6 8 10 12 16 32 50 64 100 125 250 333 500 1000 2000)
dims=(1000 2000)

DIR=trans_benchmarks

for dim in $dims
do
    echo "seq ${dim}"
    #seq
    python differ.py ${DIR}/mandelseqwu${dim} ${DIR}/mandelseqtask${dim}

    #transformed
    for size in $sizes
    do
	if [[ $size -gt $dim ]]; then
	    break
	fi
	echo "${dim}x${size}"
	python differ.py "${DIR}/mandeltranswu${dim}x${size}" "${DIR}/mandeltranstask${dim}x${size}"
    done
done
