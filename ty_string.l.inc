\"[^\"]*\" { char *s = GC_MALLOC(strlen(yytext) - 1);
             memcpy(s, yytext+1, strlen(yytext) - 2);
             *lvalp = LREG(s, LREG_STRING);
             return ATOM;
}


