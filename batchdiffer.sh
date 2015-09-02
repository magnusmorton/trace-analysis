sizes=(1 2 4 6 8 10 12 16 32 50 64 100 125 250 333 500 1000)
dims=(100 500 1000)

DIR=trans_benchmarks

for dim in $dims
do
    echo $dim
    #seq
    python differ.py ${DIR}/matmulseqwu${dim} ${DIR}/matmulseqtask${dim}

    #transformed
    for size in $sizes
    do
	echo $size
	python differ.py "${DIR}/matmultranswu${dim}x${size}" "${DIR}/matmultranstask${dim}x${size}"
    done
done
