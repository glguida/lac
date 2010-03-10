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
#define YYSTYPE lreg_t
#define YY_DECL int yylex (lreg_t *lvalp)
#include <gc/gc.h>
#include <strings.h>
#include "laconic.h"
#include "sexpr.tab.h"
%}

%option noyywrap

WHITESPACE [\ \t\n]
DIGIT [0-9]
HEXDIGIT [0-9a-fa-z]
ALPHA [A-Za-z]
SP_INIT [#\*/:<=>?^_~!$%&+-]
SPECIAL [\*/:<=>?^_~!$%&+-.] 
SIGN [+|-]

%%

;(.)*
{WHITESPACE}

{SIGN}?{DIGIT}+ { long *n = GC_MALLOC(sizeof(unsigned long));
                  *n = strtol(yytext, (char **)NULL, 10);
                  *lvalp = LREG(n, LREG_INTEGER);
                  return ATOM;
}

#x{HEXDIGIT}+ { long *n = GC_MALLOC(sizeof(unsigned long));
                *n = strtol(yytext+2, (char **)NULL, 16);
                *lvalp = LREG(n, LREG_INTEGER);
                return ATOM;
}

\"[^\"]*\" { char *s = GC_MALLOC(strlen(yytext) - 1);
             memcpy(s, yytext+1, strlen(yytext) - 2);
             *lvalp = LREG(s, LREG_STRING);
             return ATOM;
}

(({DIGIT}+({SPECIAL}|{ALPHA}))|{ALPHA}|{SP_INIT})({SPECIAL}|{ALPHA}|{DIGIT})* {
  int i;;
  char *s = GC_MALLOC(strlen(yytext) + 1);
  for (i = 0; i < strlen(yytext); i++)
    *(s+i) = toupper(*(yytext+i));
  *lvalp = intern_symbol(s);
  return ATOM;
};

,@ { return COMMA_AT; }

. { return *yytext; }


%%


