comp = gfortran
pattern = *.f95
source = ${wildcard ${pattern}}
obj = ${patsubst %.f95, %.o, ${source}}


main: ${obj}
	${comp} $^ -o $@

%.mod %.o: %.f95	# touch $@
	${comp} -c $<

TDMA.o: mymod_tdma.mod 


run: main
	./main
result: main input
	./main<input>result
clear:
	DEL -f *.o *.out *.mod* main.exe