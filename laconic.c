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
#include <stdio.h>
#include <stdlib.h>
#include <search.h>
#include <string.h>
#include <gc/gc.h>
#include <signal.h>
#include <sigsegv.h>
#include "private.h"
#include "laconic.h"

/*
 * System environment
 */
static lenv_t null_env;
lenv_t *lac_null_env = &null_env;

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
 * Interface
 */

lreg_t register_symbol(const char *s)
{
  unsigned len = strlen(s) + 1;
  char *gcs = GC_malloc(len);
  strncpy(gcs, s, len);
  return intern_symbol(gcs);
}

/* Bind a value to a *global* variable. */
static void bind_symbol(lreg_t sym, lreg_t val)
{
  (void)env_define(&null_env, sym, val);
}

int lac_extproc_register(const char *sym, lac_function_t f)
{
  bind_symbol(register_symbol(sym), llproc_to_lreg(f));
  return 0;
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

lreg_t cons(lreg_t a, lreg_t d)
{
  struct cons *c = GC_malloc(sizeof(struct cons));
  c->a = a;
  c->d = d;
  return lreg_raw(c, LREG_CONS);
}


/*
 * Eval/Apply
 */

static lreg_t
evlist(lreg_t list, lenv_t *env)
{
  lreg_t ret = NIL;

  for (; list != NIL; list = cdr(list))
      ret = eval(car(list), env);
  return ret;
}

lreg_t evargs(lreg_t list, lenv_t *env)
{
  lreg_t tmp, head=NIL, tail=NIL;

  while (is_cons(list)) {
    tmp = cons(eval(car(list), env), NIL);
    if (head != NIL) {
      rplacd(tail, tmp);
      tail = cdr(tail);
    } else {
      head = tmp;
      tail = head;
    }
    list = cdr(list);
  }

  if (list != NIL)
    {
      lac_error("evargs: invalid arguments", list);
      head = NIL;
    }
  return head;
}

static lreg_t apply2(lreg_t proc, lreg_t args, lenv_t *argenv, lenv_t *env)
{
  lenv_t lenv;
  lreg_t ret, lproc, binds, body, arg;
  unsigned type = lreg_raw_type(proc);

  if (type == LREG_LLPROC)
    return lreg_to_llproc(proc)(args, env);
  if (type != LREG_MACRO && type != LREG_LAMBDA) {
    lac_error("not a procedure", proc);
    return NIL;
  }

  if (type == LREG_MACRO)
    argenv = NULL;

  env_pushnew(get_closure_env(proc), &lenv);

  lproc = get_closure_proc(proc);
  binds = get_proc_binds(lproc);
  body = get_proc_body(lproc);

  while (is_cons(binds) && is_cons(args)) {
	  if (car(binds) == sym_rest)
		  break;
	  arg = car(args);
	  if (argenv)
		  arg = eval(arg, env);
	  env_define(&lenv, car(binds), arg);
	  binds = cdr(binds);
	  args = cdr(args);
  }

  if (car(binds) == sym_rest) {
	  binds = cdr(binds);
	  arg = args;
	  if (argenv)
		  arg = evargs(arg, argenv);
	  env_define(&lenv, car(binds), arg);
	  binds = cdr(binds);
	  args = NIL;
  }

  if (is_cons(binds))
	  lac_error("Undefined bindings", binds);

  if (is_cons(args))
	  lac_error("Too many arguments", args);

  ret = evlist(body, &lenv);

  if (type == LREG_MACRO) {
    /* Macro expand hook? */
    ret = eval(ret, env);
  }

  return ret;
}

lreg_t apply(lreg_t proc, lreg_t args, lenv_t *env)
{
	return apply2(proc, args, env, env);
}

lreg_t eval(lreg_t sexp, lenv_t *env)
{
  lreg_t ans;
  switch (lreg_raw_type(sexp))
    {
    case LREG_NIL:
      ans = NIL;
      break;
    case LREG_SYMBOL:
      ans = env_lookup(env, sexp);
      break;
    case LREG_CONS:
      ans = apply(eval(car(sexp), env), cdr(sexp), env);
      break;
    default:
      ans = sexp;
      break;
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
  switch ( lreg_raw_type(sexp) )
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
	      switch( lreg_raw_type (tosplice) )
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
                rplacd(qqalast, qqd);
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

  rplaca(arg1, arg2);
  return arg1;
}

LAC_API static lreg_t proc_rplacd(lreg_t args, lenv_t *env)
{
  _EXPECT_ARGS(args, 2);
  lreg_t arg1 = eval(car(args), env);
  lreg_t arg2 = eval(car(cdr(args)), env);

  if ( !is_cons(arg1) )
    _ERROR_AND_RET("argument is not cons");

  rplacd(arg1, arg2);
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
      if ( lreg_type(arg1) == lreg_type(arg2) )
        lacint_extty_eq(arg1, arg2, &ans);
    }
  return ans;
}

LAC_API static lreg_t proc_apply(lreg_t args, lenv_t *env)
{
  _EXPECT_ARGS(args, 2);
  lreg_t arg1 = eval(car(args), env);
  lreg_t arg2 = eval(car(cdr(args)), env);

  return apply2(arg1, arg2, NULL, env);
}

/* Special Form */
LAC_API static lreg_t proc_cond(lreg_t args, lenv_t *env)
{
  /* Undefinite number of arguments */
  lreg_t cond = NIL;

  while ( args != NIL ) {
      lreg_t test = car(args);
      if ( !is_cons(test) )
	_ERROR_AND_RET("Syntax error in cond");

      cond = eval(car(test), env);
      /* Lisp-specific! Scheme (as for R5RS) checks for #t,
       * though guile doesn't.  */
      if ( cond != NIL ) {
	  return cdr(test) == NIL ? cond : evlist(cdr(test), env);
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

LAC_API static lreg_t proc_load(lreg_t args, lenv_t *env)
{
  int r;
  FILE *f;
  char *file;
  void *scan;
  lreg_t res, arg1;
  _EXPECT_ARGS(args, 1);

  arg1 = eval(car(args), env);
  if ( lreg_type(arg1) != LREG_STRING )
    _ERROR_AND_RET("Syntax error in load");

  lac_extty_unbox(arg1, (void **)&file);
  f = fopen((char *)file, "r");
  if ( f == NULL )
    _ERROR_AND_RET("Could not open file");

  
  sexpr_read_start(f, &scan);
  do {
    r = sexpr_read(&res, scan);
    eval(res, &null_env);
    
  } while(r);
  sexpr_read_stop(scan);
  return sym_true;
}


/*
 * Initialization Functions
 */

static void machine_init(void)
{
  /* Init symtab. */
  hcreate(500);

  /* Init Null Env */
  memset(&null_env, 0, sizeof(struct env));

  /* Lisp-style booleans.
     Can be changed into Scheme-scheme. */
  sym_false = NIL;
  sym_true = register_symbol("T");
  bind_symbol(sym_true, sym_true); /* Tautology. */

  sym_quote = register_symbol("QUOTE");
  bind_symbol(sym_quote, llproc_to_lreg(proc_quote));
  bind_symbol(register_symbol("COND"), llproc_to_lreg(proc_cond));
  bind_symbol(register_symbol("LAMBDA"), llproc_to_lreg(proc_lambda));
  bind_symbol(register_symbol("DEFINE"), llproc_to_lreg(proc_define));
  bind_symbol(register_symbol("MACRO"), llproc_to_lreg(proc_macro));
  bind_symbol(register_symbol("LABELS"), llproc_to_lreg(proc_labels));

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
  bind_symbol(sym_quasiquote, llproc_to_lreg(proc_quasiquote));
  sym_unquote = register_symbol("UNQUOTE");
  sym_splice = register_symbol("SPLICE");
  sym_rest = register_symbol("&REST");
}

void map_init(void);
void int_init(void);
void string_init(void);
static void
modules_init()
{
  int_init();
  string_init();
  map_init();
}

static void
library_init(void)
{
  int r;
  FILE *f;  
  lreg_t res;
  void *scan;

  f = fopen("sys.lac", "r");
  if ( f == NULL )
    f = fopen(LAC_SYSDIR"/sys.lac", "r");
  if ( f == NULL )
    lac_error("SYSTEM LIBRARY NOT FOUND", NIL);

  sexpr_read_start(f, &scan);
  do {
    r = sexpr_read(&res, scan);
    eval(res, &null_env);
  } while(r);
  sexpr_read_stop(scan);

  fclose(f);
}


void
lac_init(void)
{
  sigset_t emptyset;
  GC_init();
 
  sigemptyset(&emptyset); 
  sigprocmask(SIG_BLOCK, &emptyset, &mainsigset);
  stackoverflow_install_handler(stackovf_handler, extra_stack, 16384);
  machine_init();
  modules_init();
  library_init();
}



