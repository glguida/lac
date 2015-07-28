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
#include "parser.h" 

int sexpr_lex(lreg_t *, void *);

void sexpr_error (lreg_t *lr, void *scanner, const char *msgp)  /* Called by yyparse on error */
{
}

%}

%defines
%define api.prefix {sexpr_}
%define api.pure full
%define api.value.type { lreg_t }
%parse-param { lreg_t *result }
%param { void *scan }

%token ENDOFFILE
%token ATOM
%token DELIMITER
%token COMMA_AT

%%

input: | input ENDOFFILE { return -1;}
       | input sexpr     { *result = $2; YYACCEPT;};

sexpr:  ATOM ;
       | '\'' sexpr { $$ = cons(sym_quote, cons($2, NIL)); }
       | '`' sexpr { $$ = cons(sym_quasiquote, cons($2, NIL)); }
       | COMMA_AT sexpr { $$ = cons(sym_splice, cons($2, NIL)); }
       | ',' sexpr { $$ = cons(sym_unquote, cons($2, NIL)); }
       | '(' sexpr '.' sexpr ')' { $$ = cons($2,$4); }
       | '(' ')' { $$ = NIL; }
       | list { $$ = $1; }


list: '(' sexpr listelem ')' { $$ = cons($2,$3); }
;
listelem: /*EMPTY*/ { $$ = NIL; }
          | sexpr listelem { $$ = cons($1,$2); }

%%
