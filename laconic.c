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

#include <stdio.h>
#include <search.h>
#include <string.h>
#include <gc/gc.h>
#include "laconic.h"

/*
 * System environment
 */
lreg_t null_env = NIL;


/*
 * System symbols
 */
lreg_t sym_true;
lreg_t sym_false;
lreg_t sym_quote;
lreg_t sym_quasiquote;
lreg_t sym_unquote;
lreg_t sym_splice;
lreg_t sym_rest;


/*
 * Error handling.
 */
void lac_error(const char *arg)
{
  /* TODO VA ARGS */
  fprintf(stderr, "%s\n", arg);
}


/*
 * External type handling.
 */
static ext_type_t *ext_types[LREG_TYPES];

int ext_type_register(int typeno, ext_type_t *extty)
{
  if ( typeno >= LREG_TYPES )
    return -1;
  ext_types[typeno] = extty;
  return 0;
}


/*
 * Read
 */

#include "atoms.yy.h"
extern int yyparse(lreg_t *);

int lac_read(FILE *fd, lreg_t *res)
{
  int r;
  lreg_t statement;
  
  if (yyin != fd)
      yy_switch_to_buffer(yy_create_buffer(fd, YY_BUF_SIZE));

  r = yyparse(&statement);
  if ( r >= 0 )
    *res = statement;
  return r;
}


/*
 * Print
 */

void lac_print(FILE *fd, lreg_t);

void lac_print_cons(FILE *fd, lreg_t lr)
{
  lreg_t a = car(lr);
  lreg_t d = cdr(lr);
  lac_print(fd, a);

  if (d == NIL)
    {
      fprintf(fd, ") ");
      return;
    }

  if (LREG_TYPE(d) == LREG_CONS)
    lac_print_cons(fd, d);
  else
    {
      fprintf(fd, ". ");
      lac_print(fd, cdr(lr));
      fprintf(fd, ") ");
    }
}

void lac_print(FILE *fd, lreg_t lr)
{
  switch ( LREG_TYPE(lr) )
    {
    case LREG_NIL:
      fprintf(fd, "() ");
      break;
    case LREG_SYMBOL:
      fprintf(fd, "%s ", (char *)LREG_PTR(lr));
      break;
    case LREG_CONS:
      fprintf(fd, "( ");
      lac_print_cons(fd, lr);
      break;
    case LREG_MACRO:
      fprintf(fd, "<#MACRO> ");
      break;
    case LREG_LAMBDA:
      fprintf(fd, "<#LAMBDA> ");
      break;
    case LREG_LLPROC:
      fprintf(fd, "<#LLPROC> ");
      break;
    case LREG_SFORM:
      fprintf(fd, "<#SFORM> ");
      break;
    default:
      if ( ext_types[LREG_TYPE(lr)] != NULL )
	ext_types[LREG_TYPE(lr)]->print(fd, lr);
      else
	fprintf(fd, "<??? %d>",(int)LREG_TYPE(lr));
    }
  return;
}


/*
 * Basic procedures.
 */

/* Get symbol from string and intern it if new. */
lreg_t intern_symbol(char *s)
{
  lreg_t c;
  ENTRY e = { .key = s }, *r;

  /* Assert that the char pointer is actually aligned. If not it means
     that we're interning a symbol from a string not allocated by the
     GC, and this is against the code rules of this thing. */
  assert(((uintptr_t)s & LREG_TYPE_MASK) == 0);

  r = hsearch(e, ENTER);
  return LREG(LREG_PTR(r->key),LREG_SYMBOL);
}

/* Bind a value to a *global* variable. */
void bind_symbol(lreg_t sym, lreg_t val)
{
  null_env = cons(cons(sym, val), null_env);
}

lreg_t cons(lreg_t a, lreg_t d)
{
  lreg_t r;
  cons_t *c = GC_malloc(sizeof(cons_t));
  c->a = a;
  c->d = d;
  return LREG(c, LREG_CONS);
}

lreg_t car(lreg_t lr)
{
  cons_t *cons = (cons_t *)LREG_PTR(lr);
  assert(is_cons(lr));
  
  return cons->a;
}

lreg_t cdr(lreg_t lr)
{
  cons_t *cons = (cons_t *)LREG_PTR(lr);
  assert(is_cons(lr));
  
  return cons->d;
}

/* Ret values: < 0 => error, 0 => found, 1 not found */
int assq(lreg_t key, lreg_t alist, lreg_t *res)
{
  if (!is_cons(alist) && alist != NIL)
    {
      lac_error("Invalid alist");
      return -1;
    }

  for (; alist != NIL; alist = cdr(alist))
    {
      lreg_t a = car(alist);
      
      if (!is_cons(a))
	{
	  lac_error("Invalid alist");
	  return -1;
	}

      if (key == car(a))
	{
	  *res = a;
	  return 0;
	}
    }

  /* Not found */
  *res = NIL;
  return 1;
}


/*
 * Eval/Apply
 */

int evlist(lreg_t list, lreg_t *env, lreg_t *res)
{
  int r;
  for (; list != NIL; list = cdr(list))
      if ( (r = eval(car(list), env, res)) < 0 )
	return -1;
  return 0;
}

int evargs(lreg_t list, lreg_t *env, lreg_t *res)
{
  int r;
  lreg_t evfirst, evrest;

  if ( list == NIL )
    {
      *res = NIL;
      return 0;
    }

  if (!is_cons(list))
    {
      lac_error("evlist: invalid arguments");
      return -1;
    }

  r = eval(car(list), env, &evfirst);
  if ( r != 0 )
    goto err;

  r = evargs(cdr(list), env, &evrest);
  if ( r != 0 )
    goto err;

  *res = cons(evfirst, evrest);
  return 0;

 err:
  return -1;
}

int evbind(lreg_t binds, lreg_t args, lreg_t *env, lreg_t *res)
{
  lreg_t lenv = *env;
  for ( ; binds != NIL; binds = cdr(binds), args = cdr(args) )
    {
      if ( !is_cons(binds) )
	{
	  lac_error("Incorrect binding list");
	  return -1;
	}
      if ( car(binds) == sym_rest )
	{
	  if ( !is_cons(cdr(binds)) )
	    {
	      lac_error("Expected binding after &rest");
	      return -1;
	    }
	  lenv = cons(cons(car(cdr(binds)), args), lenv);
	  args = NIL;
	  if ( cdr(cdr(binds)) != NIL )
	    {
	      lac_error("Syntax error in bindings\n");
	      return -1;
	    }
	  break;
	}
      if ( args == NIL )
	{
	  lac_error("Not enough arguments to function.");
	  return -1;
	}
      lenv = cons(cons(car(binds), car(args)), lenv);
    }

  if ( args != NIL )
    {
      lac_error("Too many arguments to function.");
      return -1;
    }

  *res = lenv;
  return 0;
}

int apply(lreg_t proc, lreg_t args, lreg_t *env, lreg_t *res)
{
  int r;
  lreg_t newenv;
  lreg_t evd = args;

  switch ( LREG_TYPE(proc) )
    {
    case LREG_LLPROC:
      r = evargs(args, env, &evd);
      if( r != 0 )
	return r;
      /* Passthrough */
    case LREG_SFORM:
      r = lreg_to_llproc(proc)(evd, env, res);
      break;
    case LREG_LAMBDA:
      {
	lreg_t lenv = get_closure_env(proc);
	lreg_t lproc = get_closure_proc(proc);
	r = evargs(args, env, &evd);
	if ( r != 0 )
	  return r;
	r = evbind(get_proc_binds(lproc), evd, &lenv, &newenv);
	if ( r != 0 )
	  return r;
	r = evlist(get_proc_evlist(lproc), &newenv, res);
      }
      break;
    case LREG_MACRO:
      {
	lreg_t unevald;
	lreg_t lenv = get_closure_env(proc);
	lreg_t lproc = get_closure_proc(proc);

	/*
	 * Macro expand
	 */
	r = evbind(get_proc_binds(lproc), evd, &lenv, &newenv);
	if ( r != 0 )
	  return r;
	r = evlist(get_proc_evlist(lproc), &newenv, &unevald);
	if ( r != 0 )
	  return r;

	/*
	 * Macro expand hook?
	 */
	r = eval(unevald, env, res);
      }
      break;
    default:
      fprintf(stderr, "Not a procedure: "); lac_print(stderr, proc); fprintf(stderr, "\n");
      r = -1;
    }

  return r;
}

static int eval_sym(lreg_t sym, lreg_t *env, lreg_t *res)
{
  int r;
  lreg_t rassq;
  if (!is_symbol(sym))
    {
      lac_error("Not a symbol.");
      return -1;
    }

  r = assq(sym, *env, &rassq);
  if (r != 0)
    {
      if (r == 1)
	{
	  fprintf(stderr, "Symbol not defined: "); lac_print(stderr, sym); printf("\n");
	}
      return -1;
    }

  *res = cdr(rassq);
  return 0;
}

static int eval_cons(lreg_t cons, lreg_t *env, lreg_t *res)
{
  int r;
  lreg_t tmp;
  cons_t *c = get_cons(cons);

  r = eval(c->a, env, &tmp);
  if (r < 0)
    return r;

  r = apply(tmp, c->d, env, res);
  return r;
}

int eval(lreg_t sexp, lreg_t *env, lreg_t *res)
{
  int r = 0;

  switch (LREG_TYPE(sexp))
    {
    case LREG_NIL:
      *res = sexp;
      break;
    case LREG_SYMBOL:
      r = eval_sym(sexp, env, res);
      break;
    case LREG_CONS:
      r = eval_cons(sexp, env, res);
      break;

    default:
      if ( ext_types[LREG_TYPE(sexp)] != NULL )
	r = ext_types[LREG_TYPE(sexp)]->eval(sexp, res);
      else
	lac_error("Uh?");
    }
  return r;
}

/*
 * Embedded  Procedures
 */

/* Special Form */
LAC_API static int proc_quote(lreg_t args, lreg_t *env, lreg_t *res)
{
  _EXPECT_ARGS(args, 1);

  *res = car(args);
  return 0;
}

static int _qquote(lreg_t sexp, lreg_t *env, lreg_t *first, lreg_t *last)
{
  int r;
  switch ( LREG_TYPE(sexp) )
    {
    case LREG_CONS:
      if ( car(sexp) == sym_unquote )
	{
	  r = eval(car(cdr(sexp)), env, first);
	  if ( r < 0 )
	    return r;
	  /* * first written by eval */
	}
      else if ( car(sexp) == sym_splice )
	{
	  lreg_t tosplice;
	  if ( last == NULL )
	    {
	      fprintf(stderr, "SPLICE expected on car only.\n");
	      return -1;
	    }

	  r = eval(car(cdr(sexp)), env, &tosplice);
	  if ( r < 0 )
	    return r;

	  switch( LREG_TYPE(tosplice) )
	    {
	      lreg_t tail = NIL;
	    case LREG_CONS:
	      *first = tail = tosplice;
	      for ( ; tosplice != NIL && is_cons(cdr(tosplice)); 
		    tosplice = cdr(tosplice) );
	      *last = tosplice;
	      break;

	    default:
	      *first = tosplice;
	      break;
	    }
	}
      else
	{
	  lreg_t qqa, qqd, qqalast = NIL;

	  r = _qquote(car(sexp), env, &qqa, &qqalast);
	  if ( r < 0 )
	    return r;

	  r = _qquote(cdr(sexp), env, &qqd, NULL);
	  if ( r < 0 )
	    return r;

	  if ( qqalast != NIL )
	    {
	      if ( cdr(qqalast) == NIL  )
		get_cons(qqalast)->d = qqd;
	      else if ( qqd != NIL )
		{
		  fprintf(stderr, "Dotted pairs in spliced list can be"
			  " present only when splicing is at end of a list\n");
		  return -1;
		}

	      *first = qqa;
	    }
	  else
	      *first = cons(qqa, qqd);
	}
      break;
    default:
      *first = sexp;
    }
       
  return 0;
}

/* Special Form */
LAC_API static int proc_quasiquote(lreg_t args, lreg_t *env, lreg_t *res)
{
  _EXPECT_ARGS(args, 1);
  return _qquote(car(args), env, res, NULL);
}

LAC_API static int proc_car(lreg_t args, lreg_t *env, lreg_t *res)
{
  _EXPECT_ARGS(args, 1);
  lreg_t argument = car(args);

  /* LISP-specific! */
  if (argument == NIL)
    {
      *res = NIL;
      return 0;
    }

  if (!is_cons(argument))
    _ERROR_AND_RET("%s: argument is not cons\n", __func__);
  
  *res = car(argument);
  return 0;
}

LAC_API static int proc_cdr(lreg_t args, lreg_t *env, lreg_t *res)
{
  _EXPECT_ARGS(args, 1);
  lreg_t argument = car(args);

  /* LISP-specific!
     If I really want to keep this spec I should change cdr() and
     car() to return NIL on NIL and remove these checks. */
  if (argument == NIL)
    {
      *res = NIL;
      return 0;
    }

  if (!is_cons(argument))
    _ERROR_AND_RET("%s: argument is not cons\n", __func__);
  
  *res = cdr(argument);
  return 0;
}

LAC_API static int proc_cons(lreg_t args, lreg_t *env, lreg_t *res)
{
  _EXPECT_ARGS(args, 2);
  *res = cons(car(args), car(cdr(args)));
  return 0;
}

LAC_API static int proc_rplaca(lreg_t args, lreg_t *env, lreg_t *res)
{
  _EXPECT_ARGS(args, 2);
  lreg_t cons = car(args);
  if ( !is_cons(car(args)) )
    _ERROR_AND_RET("%s: argument is not cons\n", __func__);
  get_cons(cons)->a = car(cdr(args));
  *res = cons;
  return 0;
}

LAC_API static int proc_rplacd(lreg_t args, lreg_t *env, lreg_t *res)
{
  _EXPECT_ARGS(args, 2);
  lreg_t cons = car(args);
  if ( !is_cons(car(args)) )
    _ERROR_AND_RET("%s: argument is not cons\n", __func__);
  get_cons(cons)->d = car(cdr(args));
  *res = cons;
  return 0;
}

LAC_API static int proc_eq(lreg_t args, lreg_t *env, lreg_t *res)
{
  _EXPECT_ARGS(args, 2);
  lreg_t ans = sym_false;
  lreg_t arg1 = car(args);
  lreg_t arg2 = car(cdr(args));

  if ( arg1 == arg2 )
    ans = sym_true;
  else
    {
      /* Special type. We don't use memory tagging but pointer
	 tagging, so this is a necessary evil. */
      if ( LREG_TYPE(arg1) == LREG_TYPE(arg2)
	   && ext_types[LREG_TYPE(arg1)] != NULL )
	ext_types[LREG_TYPE(arg1)]->eq(arg1, arg2, &ans);
    }

  *res = ans;
  return 0;
}

/* Special Form */
LAC_API static int proc_cond(lreg_t args, lreg_t *env, lreg_t *res)
{
  /* Undefinite number of arguments */
  int r;
  lreg_t cond = NIL;

  while ( args != NIL )
    {
      lreg_t test = car(args);
      if ( !is_cons(test) )
	_ERROR_AND_RET("Syntax error in cond\n");

      if ( (r = eval(car(test), env, &cond)) < 0 )
	return r;
      /* LISP-specific! Scheme (as for R5RS) checks for #t, though guile doesn't.  */
      if ( cond != NIL )
	{
	  if ( cdr(test) == NIL )
	    {
	      *res = cond;
	      return 0;
	    }
	  else
	    return evlist(cdr(test), env, res);
	}
      args = cdr(args);
    }

  *res = NIL;
  return 0;
}

LAC_API static int proc_labels(lreg_t args, lreg_t *env, lreg_t *res)
{
  /* At least 3 arguments required. */
  _EXPECT_MIN_ARGS(args, 3);
  lreg_t lbl = car(args);
  lreg_t binds = car(cdr(args));
  lreg_t selfbind;

  if ( !is_cons(binds) && binds != NIL )
    _ERROR_AND_RET("Syntax error in labels");

  selfbind = cons(lbl, 0);
  *res = LREG(LREG_PTR(cons(cdr(args), cons(selfbind, *env))), LREG_LAMBDA);
  get_cons(selfbind)->d = *res;
  return 0;
}

/* Special Form */
LAC_API static int proc_lambda(lreg_t args, lreg_t *env, lreg_t *res)
{
  /* At least 2 arguments required. */
  _EXPECT_MIN_ARGS(args, 2);
  lreg_t binds = car(args);

  if ( !is_cons(binds) && binds != NIL )
    _ERROR_AND_RET("Syntax error in lambda\n");

  *res = LREG(LREG_PTR(cons(args, *env)), LREG_LAMBDA);
  return 0;
}

/* Special Form */
LAC_API static int proc_macro(lreg_t args, lreg_t *env, lreg_t *res)
{
  /* At least 2 arguments required. */
  _EXPECT_MIN_ARGS(args, 2);
  lreg_t binds = car(args);

  if ( !is_cons(binds) && binds != NIL )
    _ERROR_AND_RET("Syntax error in macro\n");

  *res = LREG(LREG_PTR(cons(args, *env)), LREG_MACRO);
  return 0;
}

/* Special Form */
LAC_API static int proc_define(lreg_t args, lreg_t *env, lreg_t *res)
{
  int r;
  lreg_t defd;
  _EXPECT_ARGS(args, 2);

  if ( !is_symbol(car(args)) )
    _ERROR_AND_RET("Syntax error in define\n");

  r = eval(car(cdr(args)), env, &defd);
  if ( r != 0 )
    return r;

  *env = cons(cons(car(args), defd), *env);  
  *res = defd;
  return 0;
}

LAC_API static int proc_set(lreg_t args, lreg_t *env, lreg_t *res)
{
  int r;
  lreg_t set, alist = *env;
  _EXPECT_ARGS(args, 2);

  if ( !is_symbol(car(args)) )
    _ERROR_AND_RET("Syntax error in set\n");

  set = car(cdr(args));

  for ( ; alist != NIL; alist = cdr(alist) )
    {
      lreg_t a;
      if ( !is_cons(alist) )
	_ERROR_AND_RET("Invalid env\n");

      a = car(alist);
      if ( !is_cons(a) )
	_ERROR_AND_RET("Invalid env\n");

      if ( car(args) == car(a) )
	{
	  get_cons(a)->d = set;
	  *res = set;
	  return 0;
	}
    }

  /* Not defined */
  lac_error("Not defined");
  *res = NIL;
  return 0;
}

LAC_DEFINE_TYPE_PFUNC(cons, LREG_CONS);
LAC_DEFINE_TYPE_PFUNC(symbol, LREG_SYMBOL);

LAC_API static int proc_gensym(lreg_t args, lreg_t *env, lreg_t *res)
{
  #define GENSYM "#GSYM"
  static int id = 0;
  int len;
  char *s, *s1;
  _EXPECT_ARGS(args, 0);
  asprintf(&s1, "%s-%08x", GENSYM, id);
  len = strlen(s1);
  s = GC_malloc(len);
  memcpy(s, s1, len);
  free(s1);
  *res = intern_symbol(s);
  id++;
  return 0;
}

static void repl(FILE *fd);
LAC_API static int proc_load(lreg_t args, lreg_t *env, lreg_t *res)
{
  int r;
  _EXPECT_ARGS(args, 1);

  if ( LREG_TYPE(car(args)) != LREG_STRING )
    _ERROR_AND_RET("Syntax error in load");

  FILE *fd = fopen((char *)LREG_PTR(car(args)), "r");
  if ( fd == NULL )
    _ERROR_AND_RET("Could not open file");

  repl(fd); // XXX: ret value?
  fclose(fd);
  *res = sym_true;
  return 0;
}


/*
 * Initialization Functions
 */
lreg_t register_symbol(const char *s)
{
  unsigned len = strlen(s) + 1;
  char *gcs = GC_malloc(len);
  strncpy(gcs, s, len);
  return intern_symbol(gcs);
}

static void machine_init(void)
{
  char *s;

  /* Init symtab. */
  hcreate(50);

  /* LISP-style booleans.
     Can be changed into Scheme-scheme. */
  sym_false = NIL;
  sym_true = register_symbol("T");
  bind_symbol(sym_true, sym_true); /* Tautology. */

  sym_quote = register_symbol("QUOTE");
  bind_symbol(sym_quote, sform_to_lreg(proc_quote));
  bind_symbol(register_symbol("COND"), sform_to_lreg(proc_cond));
  bind_symbol(register_symbol("LAMBDA"), sform_to_lreg(proc_lambda));
  bind_symbol(register_symbol("DEFINE"), sform_to_lreg(proc_define));
  bind_symbol(register_symbol("MACRO"), sform_to_lreg(proc_macro));
  bind_symbol(register_symbol("LABELS"), sform_to_lreg(proc_labels));

  bind_symbol(register_symbol("CONS"), llproc_to_lreg(proc_cons));
  bind_symbol(register_symbol("CAR"), llproc_to_lreg(proc_car));
  bind_symbol(register_symbol("CDR"), llproc_to_lreg(proc_cdr));
  bind_symbol(register_symbol("RPLACA"), llproc_to_lreg(proc_rplaca));
  bind_symbol(register_symbol("RPLACD"), llproc_to_lreg(proc_rplacd));
  bind_symbol(register_symbol("EQ"), llproc_to_lreg(proc_eq));
  bind_symbol(register_symbol("LOAD"), llproc_to_lreg(proc_load));
  bind_symbol(register_symbol("SET"), llproc_to_lreg(proc_set));
  bind_symbol(register_symbol("GENSYM"), llproc_to_lreg(proc_gensym));
  bind_symbol(register_symbol("CONSP"), llproc_to_lreg(LAC_TYPE_PFUNC(cons)));
  bind_symbol(register_symbol("SYMBOLP"), llproc_to_lreg(LAC_TYPE_PFUNC(symbol)));

  sym_quasiquote = register_symbol("QUASIQUOTE");
  bind_symbol(sym_quasiquote, sform_to_lreg(proc_quasiquote));
  sym_unquote = register_symbol("UNQUOTE");
  sym_splice = register_symbol("SPLICE");
  sym_rest = register_symbol("&REST");
}

static void repl(FILE *fd)
{
  lreg_t res;
  int quit = 0;
  struct timeval t1, t2;

  for(;;)
    {
      int r;

      if ( isatty(fileno(fd)) )
	printf("lac> ");

      /* REPL :-) */
      switch ( lac_read(fd, &res) )
	{
	case 0: /* EOF */
	  quit = 1;
	  break;
	case 1: /* Syntax Error */
	  break;
	case 2: /* EVAL and PRINT */
	  gettimeofday(&t1, NULL);
	  r = eval(res, &null_env, &res);
	  gettimeofday(&t2, NULL);
	  if (r == 0)
	    {
	      if ( isatty(fileno(fd)) )
		{
		  printf("=> "); 
		  lac_print(stdout, res); 
		  printf("\n");
		  printf("Evaluation took %ld seconds and %ld microseconds.\n",
			 t2.tv_sec - t1.tv_sec, 
			 t2.tv_usec >= t1.tv_usec ? t2.tv_usec - t1.tv_usec : t2.tv_usec + 1000000L - t1.tv_usec);
		}
	    }
	  break;
	}
      if (quit)
	break;
    }
}

static void library_init(void)
{
  FILE *fd = fopen("sys.lac", "r");
  if ( fd == NULL )
    {
      fprintf(stderr, "(ERROR: SYSTEM LIBRARY NOT FOUND)\n");
      return;
    }
  repl(fd);
  fclose(fd);
}

int main()
{
  library_init();
  repl(stdin);
  printf("\ngoodbye!\n");
}

LAC_INITF(lac_init)
{
  GC_init();
  machine_init();
}
