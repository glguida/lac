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
#include <sys/time.h>
#include <gc/gc.h>
#include "laconic.h"

/*
 * System environment
 */
lenv_t null_env;


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
  ENTRY e = { .key = s }, *r;

  /* Assert that the char pointer is actually aligned. If not it means
     that we're interning a symbol from a string not allocated by the
     GC, and this is against the code rules of this thing. */
  assert(((uintptr_t)s & LREG_TYPE_MASK) == 0);

  r = hsearch(e, ENTER);
  return LREG(LREG_PTR((lreg_t)r->key),LREG_SYMBOL);
}

/* Bind a value to a *global* variable. */
void bind_symbol(lreg_t sym, lreg_t val)
{
  (void)env_define(&null_env, sym, val);
}

lreg_t cons(lreg_t a, lreg_t d)
{
  cons_t *c = GC_malloc(sizeof(cons_t));
  c->a = a;
  c->d = d;
  return LREG(c, LREG_CONS);
}

lreg_t car(lreg_t lr)
{
  cons_t *cons = (cons_t *)LREG_PTR(lr);
  assert(is_cons(lr) | (lr == NIL) );
  if ( lr == NIL )
    return NIL;
  return cons->a;
}

lreg_t cdr(lreg_t lr)
{
  cons_t *cons = (cons_t *)LREG_PTR(lr);
  assert(is_cons(lr) | (lr == NIL));
  if ( lr == NIL )
    return NIL;
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

int evlist(lreg_t list, lenv_t *env, lreg_t *res)
{
  int r;
  for (; list != NIL; list = cdr(list))
      if ( (r = eval(car(list), env, res)) < 0 )
	return -1;
  return 0;
}

int evargs(lreg_t list, lenv_t *env, lreg_t *res)
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

int evbind(lreg_t binds, lreg_t args, lenv_t *defenv, lenv_t *evenv)
{
  lreg_t evd;
  for ( ; binds != NIL; binds = cdr(binds), args = cdr(args) )
    {
      if ( !is_cons(binds) )
	{
	  lac_error("Incorrect binding list");
	  return -1;
	}
      if ( car(binds) == sym_rest )
	{
          evd = args;
	  if ( !is_cons(cdr(binds)) )
	    {
	      lac_error("Expected binding after &rest");
	      return -1;
	    }
          if ( evenv != NULL )
            evargs(args, evenv, &evd);
	  env_define(defenv, car(cdr(binds)), evd);
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
      evd = car(args);
      if ( evenv != NULL )
        eval(car(args), evenv, &evd);
      env_define(defenv, car(binds), evd);
    }

  if ( args != NIL )
    {
      lac_error("Too many arguments to function.");
      return -1;
    }
  return 0;
}

static int _apply(lreg_t proc, lreg_t evd, lenv_t *env, int doeval, lreg_t *res)
{
  int r;
  switch ( LREG_TYPE(proc) )
    {
    case LREG_LLPROC:
    case LREG_SFORM:
      r = lreg_to_llproc(proc)(evd, env, res);
      break;
    case LREG_LAMBDA:
      {
	lenv_t lenv;
	lenv_t *procenv = get_closure_env(proc);
	lreg_t lproc = get_closure_proc(proc);
	env_pushnew(procenv, &lenv);
	r = evbind(get_proc_binds(lproc), evd, &lenv, doeval ? env : NULL);
	if ( r != 0 )
	  return r;
	r = evlist(get_proc_evlist(lproc), &lenv, res);
      }
      break;
    case LREG_MACRO:
      {
	lenv_t lenv;
	lreg_t unevald;
	lenv_t *procenv = get_closure_env(proc);
	lreg_t lproc = get_closure_proc(proc);

	/*
	 * Macro expand
	 */
	env_pushnew(procenv, &lenv);
	r = evbind(get_proc_binds(lproc), evd, &lenv, doeval ? env : NULL);
	if ( r != 0 )
	  return r;
	r = evlist(get_proc_evlist(lproc), &lenv, &unevald);
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
      break;
    }

  return r;
}

int apply(lreg_t proc, lreg_t args, lenv_t *env, lreg_t *res)
{
  int r;
  int doeval = 0;
  switch ( LREG_TYPE(proc) )
    {
    case LREG_LLPROC:
    case LREG_LAMBDA:
      doeval = 1;
      break;
    case LREG_SFORM:
    case LREG_MACRO:
      break;
    default:
      fprintf(stderr, "Not a procedure: "); lac_print(stderr, proc); fprintf(stderr, "\n");
      return -1;
    }
  r = _apply(proc, args, env, doeval, res);
  return r;
}

static int eval_sym(lreg_t sym, lenv_t *env, lreg_t *res)
{
  int r;
  if (!is_symbol(sym))
    {
      lac_error("Not a symbol.");
      return -1;
    }

  r = env_lookup(env, sym, res);
  if (r != 0)
    {
      if (r == 1)
	  fprintf(stderr, "Symbol not defined: "); lac_print(stderr, sym); printf("\n");
      return -1;
    }
  return 0;
}

static int eval_cons(lreg_t cons, lenv_t *env, lreg_t *res)
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

int eval(lreg_t sexp, lenv_t *env, lreg_t *res)
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
LAC_API static int proc_quote(lreg_t args, lenv_t *env, lreg_t *res)
{
  _EXPECT_ARGS(args, 1);

  *res = car(args);
  return 0;
}

static int _qquote(lreg_t sexp, lenv_t *env, lreg_t *first, lreg_t *last, int nested)
{
  int r;
  switch ( LREG_TYPE(sexp) )
    {
    case LREG_CONS:
      if ( car(sexp) == sym_quasiquote )
	{
	  lreg_t qqd;
	  _qquote(cdr(sexp), env, &qqd, NULL, nested+1);
	  *first = cons(sym_quasiquote, qqd);
	}
      else if ( (car(sexp) == sym_unquote) )
	{
	  if ( nested == 0 )
	    {
	      r = eval(car(cdr(sexp)), env, first);
	      if ( r < 0 )
		return r;
	      /* * first written by eval */
	    }
	  else
	    {
	      lreg_t qqd;
	      _qquote(cdr(sexp), env, &qqd, NULL, nested - 1);
	      *first = cons(sym_unquote, qqd);
	    }
	}
      else if ( car(sexp) == sym_splice )
	{
	  if ( nested == 0 )
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
	      lreg_t qqd;
	      _qquote(cdr(sexp), env, &qqd, NULL, nested - 1);
	      *first = cons(sym_splice, qqd);
	    }
	}
      else
	{
	  lreg_t qqa, qqd, qqalast = NIL;

	  r = _qquote(car(sexp), env, &qqa, &qqalast, nested);
	  if ( r < 0 )
	    return r;

	  r = _qquote(cdr(sexp), env, &qqd, NULL, nested);
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
LAC_API static int proc_quasiquote(lreg_t args, lenv_t *env, lreg_t *res)
{
  _EXPECT_ARGS(args, 1);
  return _qquote(car(args), env, res, NULL, 0);
}

LAC_API static int proc_car(lreg_t args, lenv_t *env, lreg_t *res)
{
  _EXPECT_ARGS(args, 1);
  lreg_t arg1;
  eval(car(args), env, &arg1);

  /* LISP-specific! */
  if (arg1 == NIL)
    {
      *res = NIL;
      return 0;
    }

  if ( !is_cons(arg1) )
    _ERROR_AND_RET("%s: argument is not cons\n", __func__);
  
  *res = car(arg1);
  return 0;
}

LAC_API static int proc_cdr(lreg_t args, lenv_t *env, lreg_t *res)
{
  _EXPECT_ARGS(args, 1);
  lreg_t arg1;
  eval(car(args), env, &arg1);

  /* LISP-specific!
     If I really want to keep this spec I should change cdr() and
     car() to return NIL on NIL and remove these checks. */
  if (arg1 == NIL)
    {
      *res = NIL;
      return 0;
    }

  if (!is_cons(arg1))
    _ERROR_AND_RET("%s: argument is not cons\n", __func__);
  
  *res = cdr(arg1);
  return 0;
}

LAC_API static int proc_cons(lreg_t args, lenv_t *env, lreg_t *res)
{
  _EXPECT_ARGS(args, 2);
  lreg_t arg1, arg2;
  eval(car(args), env, &arg1);
  eval(car(cdr(args)), env, &arg2);
  *res = cons(arg1, arg2);
  return 0;
}

LAC_API static int proc_rplaca(lreg_t args, lenv_t *env, lreg_t *res)
{
  _EXPECT_ARGS(args, 2);
  lreg_t arg1, arg2;
  eval(car(args), env, &arg1);
  eval(car(cdr(args)), env, &arg2);

  lreg_t cons = arg1;
  if ( !is_cons(arg1) )
    _ERROR_AND_RET("%s: argument is not cons\n", __func__);
  get_cons(cons)->a = arg2;
  *res = cons;
  return 0;
}

LAC_API static int proc_rplacd(lreg_t args, lenv_t *env, lreg_t *res)
{
  _EXPECT_ARGS(args, 2);
  lreg_t arg1, arg2;
  eval(car(args), env, &arg1);
  eval(car(cdr(args)), env, &arg2);

  lreg_t cons = arg1;
  if ( !is_cons(arg1) )
    _ERROR_AND_RET("%s: argument is not cons\n", __func__);
  get_cons(cons)->d = arg2;
  *res = cons;
  return 0;
}

LAC_API static int proc_eq(lreg_t args, lenv_t *env, lreg_t *res)
{
  _EXPECT_ARGS(args, 2);
  lreg_t ans = sym_false;
  lreg_t arg1;
  lreg_t arg2;
  eval(car(args), env, &arg1);
  eval(car(cdr(args)), env, &arg2);

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

LAC_API static int proc_apply(lreg_t args, lenv_t *env, lreg_t *res)
{
  _EXPECT_ARGS(args, 2);
  int r;
  lreg_t arg1, arg2;
  eval(car(args), env, &arg1);
  eval(car(cdr(args)), env, &arg2);


  r = _apply(arg1, arg2, env, 0, res);
  if ( r != 0 )
    return r;

  return 0;
}

/* Special Form */
LAC_API static int proc_cond(lreg_t args, lenv_t *env, lreg_t *res)
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

LAC_API static int proc_labels(lreg_t args, lenv_t *env, lreg_t *res)
{
  /* At least 3 arguments required. */
  _EXPECT_MIN_ARGS(args, 3);
  lenv_t *penv = GC_malloc(sizeof(lenv_t));
  lreg_t lbl = car(args);
  lreg_t binds = car(cdr(args));

  if ( !is_cons(binds) && binds != NIL )
    _ERROR_AND_RET("Syntax error in labels");

  env_pushnew(env, penv);
  *res = LREG(LREG_PTR(cons(cdr(args), LREG(penv, LREG_NIL))), LREG_LAMBDA);
  env_define(penv, lbl, *res);
  return 0;
}

/* Special Form */
LAC_API static int proc_lambda(lreg_t args, lenv_t *env, lreg_t *res)
{
  /* At least 2 arguments required. */
  _EXPECT_MIN_ARGS(args, 2);
  lenv_t *penv = GC_malloc(sizeof(lenv_t));
  lreg_t binds = car(args);

  if ( !is_cons(binds) && binds != NIL )
    _ERROR_AND_RET("Syntax error in lambda\n");

  env_pushnew(env, penv);
  *res = LREG(LREG_PTR(cons(args, LREG(penv, LREG_NIL))), LREG_LAMBDA);
  return 0;
}

/* Special Form */
LAC_API static int proc_macro(lreg_t args, lenv_t *env, lreg_t *res)
{
  /* At least 2 arguments required. */
  _EXPECT_MIN_ARGS(args, 2);
  lenv_t *penv = GC_malloc(sizeof(lenv_t));
  lreg_t binds = car(args);

  if ( !is_cons(binds) && binds != NIL )
    _ERROR_AND_RET("Syntax error in macro\n");

  env_pushnew(env, penv);
  *res = LREG(LREG_PTR(cons(args, LREG(penv, LREG_NIL))), LREG_MACRO);
  return 0;
}

/* Special Form */
LAC_API static int proc_define(lreg_t args, lenv_t *env, lreg_t *res)
{
  int r;
  lreg_t defd;
  _EXPECT_ARGS(args, 2);

  if ( !is_symbol(car(args)) )
    _ERROR_AND_RET("Syntax error in define\n");

  r = eval(car(cdr(args)), env, &defd);
  if ( r != 0 )
    return r;

  r = env_define(env, car(args), defd);
  if ( r < 0 )
    return r;
  *res = defd;
  return 0;
}

LAC_API static int proc_set(lreg_t args, lenv_t *env, lreg_t *res)
{
  int r;
  lreg_t arg1, arg2;
  _EXPECT_ARGS(args, 2);
  eval(car(args), env, &arg1);
  eval(car(cdr(args)), env, &arg2);

  if ( !is_symbol(arg1) )
    _ERROR_AND_RET("Syntax error in set\n");

  r = env_set(env, arg1, arg2);
  if ( r < 0 )
    return r;

  if ( r == 0 )
    {
      *res = arg2;
      return 0;
    }

  /* Not defined */
  fprintf(stderr, "Not defined: "); lac_print(stderr, arg1); fprintf(stderr, "\n");
  *res = NIL;
  return 0;
}

LAC_DEFINE_TYPE_PFUNC(cons, LREG_CONS);
LAC_DEFINE_TYPE_PFUNC(symbol, LREG_SYMBOL);

LAC_API static int proc_gensym(lreg_t args, lenv_t *env, lreg_t *res)
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
LAC_API static int proc_load(lreg_t args, lenv_t *env, lreg_t *res)
{
  _EXPECT_ARGS(args, 1);
  lreg_t arg1;
  eval(car(args), env, &arg1);

  if ( LREG_TYPE(arg1) != LREG_STRING )
    _ERROR_AND_RET("Syntax error in load");

  FILE *fd = fopen((char *)LREG_PTR(arg1), "r");
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
  /* Init symtab. */
  hcreate(500);

  /* Init Null Env */
  env_pushnew(NULL, &null_env);

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
  bind_symbol(register_symbol("APPLY"), llproc_to_lreg(proc_apply));
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
			 t2.tv_usec >= t1.tv_usec ? t2.tv_sec - t1.tv_sec : t2.tv_sec - t1.tv_sec - 1, 
			 t2.tv_usec >= t1.tv_usec ? t2.tv_usec - t1.tv_usec : t2.tv_usec + 1000000L - t1.tv_usec);
		}
	    }
	  break;
	}
      if (quit)
	break;
    }
}

void map_init(void);
void int_init(void);
void string_init(void);
static void modules_init()
{
  int_init();
  string_init();
  map_init();
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
  GC_init();
  machine_init();
  modules_init();
  library_init();
  repl(stdin);
  printf("\ngoodbye!\n");
  return 0;
}
