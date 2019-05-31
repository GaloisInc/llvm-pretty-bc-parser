
GHC = ghc

all:

%.bc: %.ll
	llvm-as -o $@ $<

%.s: %.bc
	llc -o $@ $<

all: factorial
factorial: factorial.o test.o
	$(CC) -o $@ $^

factorial.ll: Factorial.hs
	$(GHC) -i../src Factorial.hs -e factorialDoc > $@

clean: clean-factorial
clean-factorial:
	$(RM) factorial factorial.ll factorial.o test.o

