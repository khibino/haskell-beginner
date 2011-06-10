
HC = ghc

%.o: %.hs
	$(HC) -Wall -c $<

%: %.hs
	ghc -Wall --make $@

programs = number0 number1 number2 rpncalc0 rpncalc1 rpncalc2

targets = \
	Stack.o \
	$(programs)

all: $(targets)

test: all
	./number0 < ../Community-Okabe2011/Projects/number/testdata/en.in
	./number1 < ../Community-Okabe2011/Projects/number/testdata/en.in

clean:
	$(RM) *.hi *.o
	$(RM) $(programs)
