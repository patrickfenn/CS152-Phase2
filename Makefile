parser: miniL.lex
	bison -v -d --file-prefix=y miniL.y
	flex miniL.lex
	gcc -o miniL y.tab.c lex.yy.c -lfl

clean:
	rm -f lex.yy.c y.tab.* y.output *.o parser