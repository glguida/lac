all: lac lac.debug

MODULES_LEX+= ty_string.l.inc
MODULES_SRC+= ty_string.c

MODULES_LEX+= ty_int.l.inc
MODULES_SRC+= ty_int.c 

MODULES_SRC+=map.c

SRCS= sexpr.tab.c laconic.c ext_types.c env.c lexer.yy.c ${MODULES_SRC}

lexer.l: lexer-top.l lexer-btm.l $(MODULES_LEX)
	cat lexer-top.l $(MODULES_LEX) lexer-btm.l > lexer.l

%.yy.c: %.l
	${LEX} --header-file=$*.yy.h -o $@ $^

%.tab.c: %.y
	$(YACC) -d -b $* $^

lac: ${SRCS} 
	$(CC) -g -O3 -DNO_ASSERT -Wall -lgc -lsigsegv -lreadline $^ -o $@

lac.debug: ${SRCS}
	$(CC) -g -O0 -Wall -lgc -lsigsegv -lreadline $^ -o $@

clean:
	-rm sexpr.tab.[ch] lexer.yy.[ch] lac lac.debug lexer.l
