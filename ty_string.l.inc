\"[^\"]*\" {
	     char *s = GC_MALLOC(strlen(yytext) - 1);
             memcpy(s, yytext+1, strlen(yytext) - 2);
             *lvalp = lac_extty_box(LREG_STRING, &s, sizeof(char *));
             return ATOM;
}


