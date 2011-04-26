all: lac

%.yy.c: %.l
	${LEX} --header-file=$*.yy.h -o $@ $^

%.tab.c: %.y
	${YACC} -d -b $* $^

lac: sexpr.tab.c laconic.c env.c atoms.yy.c ints.c strings.c map.c
	$(CC) -g -O3 -Wall -lgc $^ -o lac

clean:
	-rm sexpr.tab.[ch] atoms.yy.[ch] lac
