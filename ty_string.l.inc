\"[^\"]*\" {
	     char *s = GC_MALLOC(strlen(yytext) - 1);
             memcpy(s, yytext+1, strlen(yytext) - 2);
             *lvalp = extty_box(LREG_STRING, &s, sizeof(char *));
             return ATOM;
}


