
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


