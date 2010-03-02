%{
#include <stdio.h>
#include <string.h>
#include <gc/gc.h>
#include "laconic.h"

#define YYSTYPE lreg_t

%}

%pure_parser
%parse-param { lreg_t *result }
%token ATOM
%token DELIMITER

%%

input: /* EMPTY */
       | statement input;
;

statement: sexp { $$ = $1;
                  *result = $$;
		  return 2;
}

sexp:  ATOM { $$ = $1; }
       | '\'' sexp { $$ = cons(sym_quote, cons($2, NIL)); }
| '`' sexp { $$ = cons(sym_quasiquote, cons($2, NIL)); }
       | ',' sexp { $$ = cons(sym_unquote, cons($2, NIL)); }
       | '(' sexp '.' sexp ')' { $$ = cons($2,$4); }
       | '(' ')' { $$ = NIL; }
       | list { $$ = $1; };
;

list: '(' sexp listelem ')' { $$ = cons($2,$3); }
;
listelem: /*EMPTY*/ { $$ = NIL; }
          | sexp listelem { $$ = cons($1,$2); }

%%

yyerror (s)  /* Called by yyparse on error */
char *s;
{
  printf ("Syntax Error\n");
}
