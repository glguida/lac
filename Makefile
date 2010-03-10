all: lac

%.yy.c: %.l
	flex -o $@ --header-file=$*.yy.h $^

%.tab.c: %.y
	bison --defines $^

lac: sexpr.tab.c laconic.c env.c atoms.yy.c ints.c strings.c map.c
	$(CC) -O3 -lgc $^ -o lac

clean:
	-rm sexpr.tab.[ch] atoms.yy.[ch] lac