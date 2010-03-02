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
HEXDIGIT [0-9a-f]
ALPHA [A-Z]
SP_INIT [/:<=>?^_~!$%&+-]
SPECIAL [/:<=>?^_~!$%&+-.@] 
SIGN [+|-]

%%

;(.)*
{WHITESPACE}

{SIGN}?{DIGIT}+ { long *n = GC_MALLOC(sizeof(unsigned long));
           *n = strtol(yytext, (char **)NULL, 10);
           *lvalp = LREG(LREG_PTR(n), LREG_INTEGER);
           return ATOM;
}

#x{HEXDIGIT}+ { int *n = GC_MALLOC(sizeof(unsigned long));
             *n = strtol(yytext+2, (char **)NULL, 16);
             *lvalp = LREG(LREG_PTR(n), LREG_INTEGER);
             return ATOM;
}

(({DIGIT}+({SPECIAL}|{ALPHA}))|{ALPHA}|{SP_INIT})({SPECIAL}|{ALPHA}|{DIGIT})* {
  int i;
  char *s = GC_MALLOC(strlen(yytext) + 1);
  for (i = 0; i < strlen(yytext); i++)
    *(s+i) = toupper(*(yytext+i));
  *lvalp = intern_symbol(s);
  return ATOM;
};



. { return *yytext; }


%%


