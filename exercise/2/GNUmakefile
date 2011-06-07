
HC = ghc

%.o: %.hs
	$(HC) -Wall -c $<

%: %.hs
	ghc --make $@

all: number0 number1

clean:
	$(RM) *.hi *.o
