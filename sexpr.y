/*
   lac -- a laconic lisp interpreter
   Copyright (C) 2010 Gianluca Guida

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License along
   with this program; if not, write to the Free Software Foundation, Inc.,
   51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
*/

%{
#include <stdio.h>
#include <string.h>
#include <gc/gc.h>
#include "laconic.h"

int yylex(lreg_t *);
void yyerror();
#define YYSTYPE lreg_t

%}

%pure_parser
%parse-param { lreg_t *result }
%token ATOM
%token DELIMITER
%token COMMA_AT

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
       | COMMA_AT sexp { $$ = cons(sym_splice, cons($2, NIL)); }
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

void yyerror ()  /* Called by yyparse on error */
{
  printf ("Syntax Error\n");
}
