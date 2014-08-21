FILES = \
	test_m \
	test_arc \
	test_smp \
	calc_m \
	calc_arc \
	calc_smp \

all: $(FILES)

test_m: test.m
	gcc -o test_m test.m -framework Foundation -fobjc-arc

test_arc: test_arc.cpp
	g++ -o test_arc test_arc.cpp

test_smp: test_smp.cpp
	g++ -o test_smp test_smp.cpp


calc_m: calc.m
	gcc -o calc_m calc.m -framework Foundation -fobjc-arc

calc_arc: calc_arc.cpp
	g++ -o calc_arc calc_arc.cpp

calc_smp: calc_smp.cpp
	g++ -o calc_smp calc_smp.cpp

clean:
	rm -rf $(FILES)
