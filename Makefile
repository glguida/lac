all: lac lac.debug

SRCS= sexpr.tab.c laconic.c env.c atoms.yy.c ints.c strings.c map.c

%.yy.c: %.l
	${LEX} --header-file=$*.yy.h -o $@ $^

%.tab.c: %.y
	${YACC} -d -b $* $^

lac: ${SRCS}
	$(CC) -g -O3 -DNO_ASSERT -Wall -lgc $^ -o $@

lac.debug: ${SRCS}
	$(CC) -g -O3 -Wall -lgc $^ -o $@

clean:
	-rm sexpr.tab.[ch] atoms.yy.[ch] lac
