#sizes = (1 50 100 150 200 250 300 350 400 450 500 550 600 650 750 800 850 900 950 1000)
sizes = (1 10 20 50 83 100 125 200 250 333 500 1000)

# untransformed
#warmup

../pycket/pycket-c  matmulfl.rkt -c 0 > trans_benchmarks/matmulseqwu

#task
for i in {1..10}
do
    ../pycket/pycket-c  matmulfl.rkt -c 0 >> trans_benchmarks/matmulseqtask
done	 

#transformed 
for size in sizes
do
    #warmup
    ../pycket/pycket-c  matmulfl.rkt -c 0 > trans_benchmarks/matmultranswu${size} 
    #task
    for i in {1..10}
    do
	../pycket/pycket-c  matmulfl.rkt -c 0 >> trans_benchmarks/matmultranstask${size}
    done	 
   
    
done
