

{SIGN}?{DIGIT}+ { 
		  errno = 0;
		  unsigned long n = strtol(yytext, (char **)NULL, 10);
		  if ( errno == ERANGE )
			lac_error("Integer overflow in input", NIL);
                  *lvalp = LREG((void *)n, LREG_INTEGER);
                  return ATOM;
}

#x{HEXDIGIT}+ { 
		  unsigned long n = strtol(yytext+2, (char **)NULL, 16);
                  *lvalp = LREG((void *)n, LREG_INTEGER);
                  return ATOM;
}


