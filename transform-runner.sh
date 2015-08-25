sizes=(1 10 20 50 83 100 125 200 250 333 500 1000)

# untransformed
#warmup

../pycket/pycket-c  matmulfl.rkt -c 0 > trans_benchmarks/matmulseqwu

#task
for i in {1..10}
do
    ../pycket/pycket-c  matmulfl.rkt  -c 0 -t >> trans_benchmarks/matmulseqtask
done	 

#transformed 
for size in ${sizes[*]}
do

    echo $size
    #warmup
    ../pycket/pycket-c  matmulfl.rkt -c $size > "trans_benchmarks/matmultranswu${size}"
    #task
    for i in {1..10}
    do
	echo $i
	../pycket/pycket-c  matmulfl.rkt  -c $size -t >> "trans_benchmarks/matmultranstask${size}"
    done	 
done
