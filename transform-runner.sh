sizes = (1 50 100 150 200 250 300 350 400 450 500 550 600 650 750 800 850 900 950 1000)

# untransformed
#warmup

../pycket/pycket-c  matmulfl.rkt -c 0 > matmulseqwu

#task
for i in {1..10}
do
    ../pycket/pycket-c  matmulfl.rkt -c 0 >> matmulseqtask
done	 

#transformed 
for size in sizes
do
    #warmup
    ../pycket/pycket-c  matmulfl.rkt -c 0 > matmultranswu${size} 
    #task
    for i in {1..10}
    do
	../pycket/pycket-c  matmulfl.rkt -c 0 >> matmultranstask${size}
    done	 
   
    
done
