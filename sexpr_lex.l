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
#define YY_DECL int yylex(YYSTYPE *lvalp, void *yyscanner)
#define YY_NO_INPUT
#include <ctype.h>
#include <strings.h>
#include <readline/readline.h>
#include <readline/history.h>
#include <gmp.h>
#include "laconic.h"
#include "sexpr_parse.h"

static int readline_getc(void)
{
  static int curchar = 0;
  static char *buf = NULL;

  /* First run */
  if ( buf == NULL ) {
    buf = readline("LAC>");
    if ( buf != NULL && *buf != '\0' )
	add_history(buf);
    curchar = 0;
  }

  if ( buf == NULL )
    return EOF;
  if ( buf[curchar] == '\0' ) {
    free(buf);
    buf = NULL;
    return '\n';  
  }

  return buf[curchar++];
}

#define YY_INPUT(buf,result,max_size) \
	if ( YY_CURRENT_BUFFER_LVALUE->yy_is_interactive ) \
		{ \
		int c = '*'; \
		size_t n; \
		for ( n = 0; n < max_size && \
			     (c = readline_getc())!=EOF && c!='\n'; ++n ) \
			buf[n] = (char) c; \
		if ( c == '\n' ) \
			buf[n++] = (char) c; \
		if ( c == EOF && ferror( yyin ) ) \
			YY_FATAL_ERROR( "input in flex scanner failed" ); \
		result = n; \
		} \
	else \
		{ \
		errno=0; \
		while ( (result = fread(buf, 1, max_size, yyin))==0 && ferror(yyin)) \
			{ \
			if( errno != EINTR) \
				{ \
				YY_FATAL_ERROR( "input in flex scanner failed" ); \
				break; \
				} \
			errno=0; \
			clearerr(yyin); \
			} \
		}\

%}

%option noyywrap
%option reentrant
%option prefix="sexpr_"

WHITESPACE [\ \t\n]
DIGIT [0-9]
HEXDIGIT [0-9a-fA-F]
ALPHA [A-Za-z]
SP_INIT [#\*/:<=>?^_~!$%&+-]
SPECIAL [\*/:<=>?^_~!$%&+-.] 
SIGN [+|-]

%%

;(.)*
{WHITESPACE}

{SIGN}?{DIGIT}+ {
	intptr_t n;

	errno = 0;
	n = strtol(yytext, (char **)NULL, 10);
	if ( errno == ERANGE )
		raise_exception("Integer overflow in input", NIL);
        *lvalp = lac_extty_box(LREG_INTEGER, (void *)n, 0);
	return INTEGER;
}

(#x|0x){HEXDIGIT}+ {
	intptr_t n;

	errno = 0;
	n = strtol(yytext+2, (char **)NULL, 16);
	if ( errno == ERANGE )
		raise_exception("Integer overflow in input", NIL);
	*lvalp = lac_extty_box(LREG_INTEGER, (void *)n, 0);
	return INTEGER;
}

(NIL|nil) {
	*lvalp = NIL;
	return NIHIL;
}

\"[^\"]*\" {
	size_t len = strlen(yytext);
	char *s = lac_alloc(len - 1);
	memcpy(s, yytext+1, len - 2);
	*lvalp = lreg_raw(s, LREG_STRING);
	return STRING;
}

(({DIGIT}+({SPECIAL}|{ALPHA}))|{ALPHA}|{SP_INIT})({SPECIAL}|{ALPHA}|{DIGIT})* {
	int i;
	size_t len = strlen(yytext);
	char *s = lac_alloc(len + 1);
	for (i = 0; i < len; i++)
		*(s+i) = toupper((int)*(yytext+i));
	*lvalp = intern_symbol(s);
	return SYMBOL;
};

,@ { return COMMA_AT; }

<<EOF>> { return ENDOFFILE; }

. { return *yytext; }


%%
