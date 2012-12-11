

{SIGN}?{DIGIT}+ { 

		  errno = 0;
		  unsigned long n = strtol(yytext, (char **)NULL, 10);
		  if ( errno == ERANGE )
			lac_error("Integer overflow in input", NIL);
		  *lvalp = extty_box(LREG_INTEGER, &n, sizeof(n));
                  return ATOM;
}

#x{HEXDIGIT}+ { 
		  errno = 0;
		  unsigned long n = strtol(yytext+2, (char **)NULL, 16);
		  if ( errno == ERANGE )
			lac_error("Integer overflow in input", NIL);
		  *lvalp = extty_box(LREG_INTEGER, &n, sizeof(n));
                  return ATOM;
}


