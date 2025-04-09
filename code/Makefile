all: scanner

scanner: scanner.l y.tab.h
	flex scanner.l
	gcc lex.yy.c -o scanner -lfl

clean:
	rm -f lex.yy.c scanner
