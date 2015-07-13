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

#define _GNU_SOURCE
#define _LAC_INTERNAL
#include <stdio.h>
#include <search.h>
#include <string.h>
#include <setjmp.h>
#include <sys/time.h>
#include <gc/gc.h>
#include <signal.h>
#include <sigsegv.h>
#include "laconic.h"


/*
 * System environment
 */
lenv_t null_env;
char *lac_errmsg;
lreg_t lac_errlreg;

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
jmp_buf lac_error_jmp;
void lac_print(FILE *fd, lreg_t);


void lac_error(char *arg, lreg_t errlr)
{
  lac_errmsg = arg;
  lac_errlreg = errlr;
  _longjmp(lac_error_jmp, 1);
}

void lac_error_print(FILE *f)
{
  fprintf(f, "(*LAC-ERROR* \"%s", lac_errmsg);
  if (lac_errlreg != NIL) 
    {
      fprintf(f, ": ");
      lac_print(f, lac_errlreg);
    }
  fprintf(f, "\")");
}


/*
 * Stack Overflow handling.
 */

static sigset_t mainsigset;
static char extra_stack[16384];

static void stackovf_continuation(void *arg1, void *arg2, void *arg3)
{
  lac_error(arg1, NIL);
}

static void stackovf_handler()
{
  sigprocmask(SIG_SETMASK, &mainsigset, NULL);
  sigsegv_leave_handler(stackovf_continuation, "STACK OVERFLOW", NULL, NULL);
}


/*
 * Read
 */

#include "lexer.yy.h"
extern int yyparse(lreg_t *);

int lac_read(FILE *fd, lreg_t *res)
{
  int r;
  lreg_t statement;
  
  if (yyin != fd)
      yy_switch_to_buffer(yy_create_buffer(fd, YY_BUF_SIZE));

  r = yyparse(&statement);

  switch ( r ) {
  case 0xf00: /* Statement */
    *res = statement;
    return 1;

  case 0: /* EOF */
    *res = statement;
    return 0; 

  case 1: /* Syntax Error */
  case 2: /* Memory Exhaustion */
  default: /* Unknown */
    lac_error("parser error", NIL);
    /* Not reached. */
    return 0;
  }
}


/*
 * Print
 */


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
      fprintf(fd, "%s ", (char *)lreg_raw_ptr(lr));
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
      if ( !lacint_extty_print(fd, lr) )
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
  return lreg_raw(lreg_raw_ptr((lreg_t)r->key),LREG_SYMBOL);
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
  return lreg_raw(c, LREG_CONS);
}

lreg_t car(lreg_t lr)
{
  cons_t *cons = (cons_t *)lreg_raw_ptr(lr);
  assert(is_cons(lr) | (lr == NIL) );
  if ( lr == NIL )
    return NIL;
  return cons->a;
}

lreg_t cdr(lreg_t lr)
{
  cons_t *cons = (cons_t *)lreg_raw_ptr(lr);
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
      lac_error("Invalid alist", alist);
      /* Not reached. */
    }

  for (; alist != NIL; alist = cdr(alist))
    {
      lreg_t a = car(alist);
      
      if (!is_cons(a))
	{
	  lac_error("Invalid alist", a);
          /* Not reached. */
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

lreg_t evlist(lreg_t list, lenv_t *env)
{
  lreg_t ret = NIL;
  for (; list != NIL; list = cdr(list))
      ret = eval(car(list), env);
  return ret;
}

lreg_t evargs(lreg_t list, lenv_t *env)
{
  if ( list == NIL )
    return NIL;

  if (!is_cons(list))
    {
      lac_error("evargs: invalid arguments", list);
      /* Not reached. */
    }
  return cons(eval(car(list), env), evargs(cdr(list), env));
}

void evbind(lreg_t binds, lreg_t args, lenv_t *defenv, lenv_t *evenv)
{
  lreg_t evd;
  for ( ; binds != NIL; binds = cdr(binds), args = cdr(args) )
    {
      if ( !is_cons(binds) )
	{
	  lac_error("Incorrect binding list", binds);
          /* Not reached. */
	}
      if ( car(binds) == sym_rest )
	{
          evd = args;
	  if ( !is_cons(cdr(binds)) )
	    {
	      lac_error("Expected binding after &rest", cdr(binds));
              /* Not reached. */
	    }
          if ( evenv != NULL )
            evd = evargs(args, evenv);
	  env_define(defenv, car(cdr(binds)), evd);
	  args = NIL;
	  if ( cdr(cdr(binds)) != NIL )
	    {
	      lac_error("Syntax error in bindings", cdr(cdr(binds)));
              /* Not reached. */
	    }
	  break;
	}
      if ( args == NIL )
	{
	  lac_error("Not enough arguments to function.", NIL);
          /* Not reached. */
	}
      evd = car(args);
      if ( evenv != NULL )
        evd = eval(car(args), evenv);
      env_define(defenv, car(binds), evd);
    }

  if ( args != NIL )
    {
      lac_error("Too many arguments to function.", args);
      /* Not reached. */
    }
}

static lreg_t _apply(lreg_t proc, lreg_t evd, lenv_t *env, int doeval)
{
  lreg_t ret;
  switch ( LREG_TYPE(proc) )
    {
    case LREG_LLPROC:
    case LREG_SFORM:
      ret = lreg_to_llproc(proc)(evd, env);
      break;
    case LREG_LAMBDA:
      {
	lenv_t lenv;
	lreg_t lproc = get_closure_proc(proc);
	env_pushnew(get_closure_env(proc), &lenv);
	evbind(get_proc_binds(lproc), evd, &lenv, doeval ? env : NULL);
	ret = evlist(get_proc_evlist(lproc), &lenv);
      }
      break;
    case LREG_MACRO:
      {
	lenv_t lenv;
	lreg_t unevald;
	lreg_t lproc = get_closure_proc(proc);

	/*
	 * Macro expand
	 */
	env_pushnew(get_closure_env(proc), &lenv);
	evbind(get_proc_binds(lproc), evd, &lenv, doeval ? env : NULL);
	unevald = evlist(get_proc_evlist(lproc), &lenv);

	/*
	 * Macro expand hook?
	 */
	ret = eval(unevald, env);
      }
      break;
    default:
      lac_error("Not a procedure", proc);
      /* Not reached. */
      break;
    }
  return ret;
}

lreg_t apply(lreg_t proc, lreg_t args, lenv_t *env)
{
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
      lac_error("Not a procedure", proc);
    }
  return _apply(proc, args, env, doeval);
}

static lreg_t eval_sym(lreg_t sym, lenv_t *env)
{
  int r;
  lreg_t ret;
  if (!is_symbol(sym))
    {
      lac_error("Not a symbol", sym);
      /* Not reached. */
    }

  r = env_lookup(env, sym, &ret);
  if (r != 0)
    {
      if (r == 1)
          lac_error("Symbol not defined", sym);
      lac_error("Env lookup failure.", NIL);
    }
  return ret;
}

static lreg_t eval_cons(lreg_t cons, lenv_t *env)
{
  return apply(eval(car(cons), env), cdr(cons), env);
}

lreg_t eval(lreg_t sexp, lenv_t *env)
{
  lreg_t ans = NIL;
  switch (LREG_TYPE(sexp))
    {
    case LREG_NIL:
      return NIL;
    case LREG_SYMBOL:
      return eval_sym(sexp, env);
    case LREG_CONS:
      return eval_cons(sexp, env);
    default:
      if ( !lacint_extty_eval(sexp, &ans) )
        lac_error("Undefined Extension Type! This is a serious LAC BUG().", NIL);
        /* Not reached. */
    }
  return ans;
}

/*
 * Embedded  Procedures
 */

/* Special Form */
LAC_API static lreg_t proc_quote(lreg_t args, lenv_t *env)
{
  _EXPECT_ARGS(args, 1);
  return car(args);
}

static void _qquote(lreg_t sexp, lenv_t *env, lreg_t *first, lreg_t *last, int nested)
{
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
	      *first = eval(car(cdr(sexp)), env);
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
		lac_error("SPLICE expected on car only.", NIL);
      
	      tosplice = eval(car(cdr(sexp)), env);
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

	  _qquote(car(sexp), env, &qqa, &qqalast, nested);
	  _qquote(cdr(sexp), env, &qqd, NULL, nested);

	  if ( qqalast != NIL )
	    {
	      if ( cdr(qqalast) == NIL  )
		get_cons(qqalast)->d = qqd;
	      else if ( qqd != NIL )
		lac_error("Dotted pairs in spliced list can be"
			    " present only when splicing is at end of a list.", qqd);

	      *first = qqa;
	    }
	  else
	      *first = cons(qqa, qqd);
	}
      break;
    default:
      *first = sexp;
    }
}

/* Special Form */
LAC_API static lreg_t proc_quasiquote(lreg_t args, lenv_t *env)
{
  lreg_t ret;
  _EXPECT_ARGS(args, 1);
  _qquote(car(args), env, &ret, NULL, 0);
  return ret;
}

LAC_API static lreg_t proc_car(lreg_t args, lenv_t *env)
{
  _EXPECT_ARGS(args, 1);
  lreg_t arg1 = eval(car(args), env);

  /* Lisp-specific! */
  if (arg1 == NIL)
    return NIL;

  if ( !is_cons(arg1) )
    _ERROR_AND_RET("argument is not cons");
 
  return car(arg1); 
}

LAC_API static lreg_t proc_cdr(lreg_t args, lenv_t *env)
{
  _EXPECT_ARGS(args, 1);
  lreg_t arg1 = eval(car(args), env);

  /* Lisp-specific!
     If I really want to keep this spec I should change cdr() and
     car() to return NIL on NIL and remove these checks. */
  if (arg1 == NIL)
	return NIL;

  if (!is_cons(arg1))
    _ERROR_AND_RET("argument is not cons");

  return cdr(arg1);  
}

LAC_API static lreg_t proc_cons(lreg_t args, lenv_t *env)
{
  _EXPECT_ARGS(args, 2);
  lreg_t arg1 = eval(car(args), env);
  lreg_t arg2 = eval(car(cdr(args)), env);

  return cons(arg1, arg2);
}

LAC_API static lreg_t proc_rplaca(lreg_t args, lenv_t *env)
{
  _EXPECT_ARGS(args, 2);
  lreg_t arg1 = eval(car(args), env);
  lreg_t arg2 = eval(car(cdr(args)), env);

  if ( !is_cons(arg1) )
    _ERROR_AND_RET("argument is not cons");

  get_cons(arg1)->a = arg2;
  return arg1;
}

LAC_API static lreg_t proc_rplacd(lreg_t args, lenv_t *env)
{
  _EXPECT_ARGS(args, 2);
  lreg_t arg1 = eval(car(args), env);
  lreg_t arg2 = eval(car(cdr(args)), env);

  if ( !is_cons(arg1) )
    _ERROR_AND_RET("argument is not cons");

  get_cons(arg1)->d = arg2;
  return arg1;
}

LAC_API static lreg_t proc_eq(lreg_t args, lenv_t *env)
{
  _EXPECT_ARGS(args, 2);
  lreg_t ans = sym_false;
  lreg_t arg1 = eval(car(args), env);
  lreg_t arg2 = eval(car(cdr(args)), env);

  if ( arg1 == arg2 )
    ans = sym_true;
  else
    {
      /* Special type. We don't use memory tagging but pointer
	 tagging, so this is a necessary evil. */
      if ( LREG_TYPE(arg1) == LREG_TYPE(arg2) )
        lacint_extty_eq(arg1, arg2, &ans);
    }
  return ans;
}

LAC_API static lreg_t proc_apply(lreg_t args, lenv_t *env)
{
  _EXPECT_ARGS(args, 2);
  lreg_t arg1 = eval(car(args), env);
  lreg_t arg2 = eval(car(cdr(args)), env);

  return _apply(arg1, arg2, env, 0);
}

/* Special Form */
LAC_API static lreg_t proc_cond(lreg_t args, lenv_t *env)
{
  /* Undefinite number of arguments */
  lreg_t cond = NIL;

  while ( args != NIL )
    {
      lreg_t test = car(args);
      if ( !is_cons(test) )
	_ERROR_AND_RET("Syntax error in cond");

      cond = eval(car(test), env);
      /* Lisp-specific! Scheme (as for R5RS) checks for #t,
       * though guile doesn't.  */
      if ( cond != NIL )
	{
	  if ( cdr(test) == NIL )
	    return cond;
	  else
	    return evlist(cdr(test), env);
        }
      args = cdr(args);
    }
  return NIL;
}

/* Special Form */
LAC_API static lreg_t proc_labels(lreg_t args, lenv_t *env)
{
  /* At least 3 arguments required. */
  _EXPECT_MIN_ARGS(args, 3);
  lreg_t ret;
  lreg_t lbl = car(args);
  lreg_t binds = car(cdr(args));
  lenv_t *penv = GC_malloc(sizeof(lenv_t));

  if ( !is_cons(binds) && binds != NIL )
    _ERROR_AND_RET("Syntax error in labels");

  env_pushnew(env, penv);
  ret = lreg_raw(lreg_raw_ptr(cons(cdr(args), lreg_raw(penv, LREG_NIL))), LREG_LAMBDA);
  env_define(penv, lbl, ret);
  return ret;
}

/* Special Form */
LAC_API static lreg_t proc_lambda(lreg_t args, lenv_t *env)
{
  /* At least 2 arguments required. */
  _EXPECT_MIN_ARGS(args, 2);
  lreg_t binds = car(args);
  lenv_t *penv = GC_malloc(sizeof(lenv_t));

  if ( !is_cons(binds) && binds != NIL )
    _ERROR_AND_RET("Syntax error in lambda");

  env_pushnew(env, penv);
  return lreg_raw(lreg_raw_ptr(cons(args, lreg_raw(penv, LREG_NIL))), LREG_LAMBDA);
}

/* Special Form */
LAC_API static lreg_t proc_macro(lreg_t args, lenv_t *env)
{
  /* At least 2 arguments required. */
  _EXPECT_MIN_ARGS(args, 2);
  lreg_t binds = car(args);
  lenv_t *penv = GC_malloc(sizeof(lenv_t));

  if ( !is_cons(binds) && binds != NIL )
    _ERROR_AND_RET("Syntax error in macro");

  env_pushnew(env, penv);
  return lreg_raw(lreg_raw_ptr(cons(args, lreg_raw(penv, LREG_NIL))), LREG_MACRO);
}

/* Special Form */
LAC_API static lreg_t proc_define(lreg_t args, lenv_t *env)
{
  lreg_t defd;
  _EXPECT_ARGS(args, 2);

  if ( !is_symbol(car(args)) )
    _ERROR_AND_RET("Syntax error in define");

  defd = eval(car(cdr(args)), env);
  env_define(env, car(args), defd);
  return defd;
}

LAC_API static lreg_t proc_set(lreg_t args, lenv_t *env)
{
  int r;
  _EXPECT_ARGS(args, 2);
  lreg_t arg1 = eval(car(args), env);
  lreg_t arg2 = eval(car(cdr(args)), env);

  if ( !is_symbol(arg1) )
    _ERROR_AND_RET("Syntax error in set");

  r = env_set(env, arg1, arg2);
  if ( r < 0 )
    lac_error("Error while setting env.", NIL);

  if ( r == 0 )
    return arg2;

  /* Not defined */
  return NIL;
}

LAC_DEFINE_TYPE_PFUNC(cons, LREG_CONS);
LAC_DEFINE_TYPE_PFUNC(symbol, LREG_SYMBOL);

LAC_API static lreg_t proc_gensym(lreg_t args, lenv_t *env)
{
  #define GENSYM "#GSYM"
  static int id = 0;
  int len;
  lreg_t ret;
  char *s, *s1;
  _EXPECT_ARGS(args, 0);
  asprintf(&s1, "%s-%08x", GENSYM, id);
  len = strlen(s1);
  s = GC_malloc(len);
  memcpy(s, s1, len);
  free(s1);
  ret = intern_symbol(s);
  id++;
  return ret;
}

int lac_read_eval(FILE *, lreg_t *, struct timeval *, struct timeval *);
LAC_API static lreg_t proc_load(lreg_t args, lenv_t *env)
{
  int r;
  char *file;
  lreg_t res;
  _EXPECT_ARGS(args, 1);
  lreg_t arg1 = eval(car(args), env);

  if ( LREG_TYPE(arg1) != LREG_STRING )
    _ERROR_AND_RET("Syntax error in load");

  lac_extty_unbox(arg1, &file, sizeof(file));

  FILE *fd = fopen((char *)file, "r");
  if ( fd == NULL )
    _ERROR_AND_RET("Could not open file");

  while((r = lac_read_eval(fd, &res, NULL, NULL)) > 0);
  fclose(fd);
  if ( r == 0 ) 
    return sym_true;
  else 
    return NIL;
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

  /* Lisp-style booleans.
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

int lac_read_eval(FILE *infd, lreg_t *res, struct timeval *t1, struct timeval *t2)
{
  int r;
  lreg_t tmp = NIL; 

  r = lac_read(infd, &tmp);
  if ( r <= 0 )
    return r;

  if ( t1 != NULL )
    gettimeofday(t1, NULL);

  *res = eval(tmp, &null_env);
    
  if ( t2 != NULL )
    gettimeofday(t2, NULL);

  return 1;
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
  int r = 0; 
  lreg_t res;
  FILE *fd = fopen("sys.lac", "r");
  if ( fd == NULL )
    fd = fopen(LAC_SYSDIR"/sys.lac", "r");
  if ( fd == NULL )
    lac_error("SYSTEM LIBRARY NOT FOUND", NIL);

  while((r = lac_read_eval(fd, &res, NULL, NULL)) > 0);
  fclose(fd);
}


int lac_init(FILE *errfd)
{
  sigset_t emptyset;
  GC_init();
 
  sigemptyset(&emptyset); 
  sigprocmask(SIG_BLOCK, &emptyset, &mainsigset);
  stackoverflow_install_handler(stackovf_handler, extra_stack, 16384);
  machine_init();
  modules_init();
  if ( _setjmp(lac_error_jmp) != 0 )
    {
      fprintf(errfd, "(LAC-SYSTEM \"ERROR IN SYSTEM LIBRARY: %s\")\n", lac_errmsg);
      return -1;
    }
  else
    library_init();

  return 0;
}

int lac_repl(FILE *infd, FILE *outfd, FILE *errfd)
{
  int r;
  struct timeval t1, t2;
  lreg_t res = NIL;
  if ( _setjmp(lac_error_jmp) != 0 )
    lac_error_print(errfd);
  while((r = lac_read_eval(infd, &res, &t1, &t2)) > 0) {
    if ( isatty(fileno(outfd)) )
      {
        fprintf(outfd, "=> "); 
        lac_print(outfd, res); 
        fprintf(outfd, "\n");
        fprintf(outfd, "Evaluation took %ld seconds and %ld microseconds.\n",
               t2.tv_usec >= t1.tv_usec ? t2.tv_sec - t1.tv_sec : t2.tv_sec - t1.tv_sec - 1, 
               t2.tv_usec >= t1.tv_usec ? t2.tv_usec - t1.tv_usec : t2.tv_usec + 1000000L - t1.tv_usec);
      }
  }
  return r;
}

int main()
{
  lac_init(stderr);
  lac_repl(stdin, stdout, stderr);
  fprintf(stdout, "\ngoodbye!\n");
  return 0;
}
