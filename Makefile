path1=GA
path2=tqfunc.F90
path3=tqfunc_prog.F90

all:
	mpirun -np 3 python parallel_feni.py

single:
	clear
	gfortran -fPIC -o $(path1) $(path3) liboctq.o liboceq.a
	./$(path1)

getoc:
	git clone https://github.com/sundmanbo/opencalphad.git ../oc
	cd ../oc && git checkout -b version3-final origin/version3-final
	cp Makefileoc ../oc/Makefile
	cd ../oc && make
	cp ../oc/liboceq.a ../oc/liboceqplus.mod .

oc:
	clear
	cd ../oc && make clean
	cd ../oc && make
	cp ../oc/liboceq.a ../oc/liboceqplus.mod .

compoc:
	cd ../oc && make
	cp ../oc/liboceq.a ../oc/liboceqplus.mod .

interface:
	gfortran -c -fPIC liboctq.F90
	f2py -c --fcompiler=gnu95 --f90flags=-fPIC liboctq.o liboceq.a -m liboctqpy liboctqpy.f90
	#gfortran -shared -O2 liboctqpy.f90 -o liboctqpy.so -fPIC
	#gfortran -c -fPIC liboctqpy.so liboctqpy.f90	

example:
	f2py -c --fcompiler=gnu95 --f90flags=-fPIC liboctq.o liboceq.a -m tqfunc $(path2) 
