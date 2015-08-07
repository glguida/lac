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
#include "sexpr_parse.h"

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
%token NIHIL
%token STRING
%token INTEGER
%token SYMBOL
%token DELIMITER
%token COMMA_AT

%%

input: | input ENDOFFILE { return -1;}
       | input sexpr     { *result = $2; YYACCEPT;};

sexpr: atom ;
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

atom: SYMBOL | STRING | INTEGER | NIHIL

%%

void sexpr_lex_init(void **);
void sexpr_set_in(FILE *, void *);
void sexpr_lex_destroy(void *);


/*
 * Read
 */

void
sexpr_read_start(FILE *f, void **yyscan)
{

    sexpr_lex_init(yyscan);
    sexpr_set_in(f, *yyscan);
}

int
sexpr_read(lreg_t *res, void *yyscan)
{
    int r;

    r = sexpr_parse(res, yyscan);
    switch (r) {
    case 0:
	return 1;
    case -1:
	return 0;
    case 1: /* Syntax Error */
    case 2: /* Memory Exhaustion */
    default: /* Unknown */
	raise_exception("parser error", NIL);
	/* Not reached. */
	return 0;
    }
}

void
sexpr_read_stop(void *yyscan)
{

    sexpr_lex_destroy(yyscan);
}


/*
 * Print
 */

static void sexpr_print_cons(FILE *f, lreg_t lr)
{
  lreg_t a = car(lr);
  lreg_t d = cdr(lr);
  sexpr_print(f, a);

  if (d == NIL)
    {
      fprintf(f, ") ");
      return;
    }

  if (lreg_raw_type(d) == LREG_CONS)
    sexpr_print_cons(f, d);
  else
    {
      fprintf(f, ". ");
      sexpr_print(f, cdr(lr));
      fprintf(f, ") ");
    }
}

void sexpr_print(FILE *f, lreg_t lr)
{
  switch ( lreg_type(lr) )
    {
    case LREG_NIL:
      fprintf(f, "() ");
      break;
    case LREG_SYMBOL:
      fprintf(f, "%s ", (char *)lreg_raw_ptr(lr));
      break;
    case LREG_CONS:
      fprintf(f, "( ");
      sexpr_print_cons(f, lr);
      break;
    case LREG_MACRO:
      fprintf(f, "<#MACRO> ");
      break;
    case LREG_LAMBDA:
      fprintf(f, "<#LAMBDA> ");
      break;
    case LREG_LLPROC:
      fprintf(f, "<#LLPROC> ");
      break;
    default:
      if ( !lac_extty_print(f, lr) )
	fprintf(f, "<??? %d>",(int)lreg_type(lr));
    }
  return;
}
