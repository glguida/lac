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
 * System symbols
 */
lreg_t sym_true;
lreg_t sym_false;
lreg_t sym_cond;
lreg_t sym_apply;
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

void
lac_extproc_register(lenv_t *env, const char *sym, lac_function_t f)
{

  env_define(env, register_symbol(sym), llproc_to_lreg(f));
}


/*
 * Exception handling.
 */

__thread
struct _lac_xcpt *_lac_xcpt;
__thread
char *_lac_xcpt_msg;
__thread
lreg_t _lac_xcpt_reg;

inline void raise_exception(char *arg, lreg_t errlr)
{

    _lac_xcpt_msg = arg;
    _lac_xcpt_reg = errlr;
    _throw();
}

/*
 * Stack Overflow handling.
 */

static sigset_t mainsigset;
static char extra_stack[16384];

static void stackovf_continuation(void *arg1, void *arg2, void *arg3)
{
	raise_exception(arg1, NIL);
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
      raise_exception("evargs: invalid arguments", list);
      head = NIL;
    }
  return head;
}

static void
evbind(lreg_t binds, lreg_t args, lenv_t *argenv, lenv_t *env)
{
  lreg_t arg;

  while (is_cons(binds) && is_cons(args)) {
	  if (car(binds) == sym_rest)
		  break;
	  arg = car(args);
	  if (argenv)
		  arg = eval(arg, argenv);
	  env_define(env, car(binds), arg);
	  binds = cdr(binds);
	  args = cdr(args);
  }

  if (car(binds) == sym_rest) {
	  binds = cdr(binds);
	  arg = args;
	  if (argenv)
		  arg = evargs(arg, argenv);
	  env_define(env, car(binds), arg);
	  binds = cdr(binds);
	  args = NIL;
  }

  if (is_cons(binds))
	  raise_exception("Undefined bindings", binds);

  if (is_cons(args))
	  raise_exception("Too many arguments", args);
}

lreg_t
apply(lreg_t proc, lreg_t args, lenv_t *env)
{
	return eval(cons(sym_apply, cons(proc, cons(args, NIL))), env);
}

static __thread int in_tco = 0;

lreg_t eval(lreg_t sexp, lenv_t *env)
{
  lreg_t ans;
  unsigned type;
  lenv_t *cloenv;
  lenv_t *tenvs[2];

  tenvs[0] = NULL;
 tco:
  switch (lreg_raw_type(sexp))
    {
    case LREG_SYMBOL:
      ans = env_lookup(env, sexp);
      break;
    case LREG_CONS: {
      lreg_t proc = car(sexp), args = cdr(sexp);
      lenv_t *penv, *argenv;

      ans = NIL;
      /* COND: embedded procedure */
      if (proc == sym_cond) {
	      lreg_t cond = NIL;
	      lreg_t next, test, body;	

	      body = NIL; /* Default return  */
	      while ( args != NIL ) {
		      test = car(args);
		      if ( !is_cons(test) )
			      _ERROR_AND_RET("Syntax error in cond");
		      cond = eval(car(test), env);
		      /* Lisp-specific! Scheme (as for R5RS) checks for #t,
		       * though guile doesn't.  */
		      if ( cond == NIL ) {
			      args = cdr(args);
			      continue;
		      }
		      body = cdr(test);
		      break;
	      }
	      if (body == NIL)
		      return cond;
	      next = cdr(body);
	      while(next != NIL) {
		eval(car(body), env);
		body = next;
		next = cdr(next);
	      }
	      if (in_tco) {
		      sexp = car(body);
		      /* env unchanged */
		      goto tco;
	      }
	      in_tco = 1;
	      ans = eval(car(body), env);
	      in_tco = 0;
	      break;
      } else if (proc == sym_apply) {
	      proc = car(args);
	      args = eval(car(cdr(args)), env);;
	      argenv = NULL;
	      goto _apply;
      } else {
	      lreg_t lproc, binds, body, next;

	      argenv = env;
      _apply:
	      proc = eval(proc, env);
	      type = lreg_raw_type(proc);
	      if (type == LREG_LLPROC)
		      return lreg_to_llproc(proc)(args, argenv, env);
	      if (type != LREG_MACRO && type != LREG_LAMBDA) {
		      raise_exception("not a procedure", proc);
		      return NIL;
	      }
	      lproc = get_closure_proc(proc);
	      binds = get_proc_binds(lproc);
	      body = get_proc_body(lproc);

	      if (tenvs[0] == NULL) {
		      tenvs[0] = alloca(sizeof(lenv_t));
		      tenvs[1] = NULL;
		      cloenv = tenvs[0];
	      }
	      if (type == LREG_MACRO) {
		      penv = NULL;
	      } else
		      penv = argenv;
	      env_pushnew(get_closure_env(proc), cloenv);
	      evbind(binds, args, penv, cloenv);
	      next = cdr(body);
	      while (body != NIL) {
		      if (next == NIL && type == LREG_LAMBDA && in_tco) {
			      lenv_t *t;

			      if (tenvs[1] == NULL) {
				      tenvs[1] = alloca(sizeof(lenv_t));
				      env = tenvs[1];
			      }
			      /* Swap ENV */
			      t = env;
			      env = cloenv;
			      cloenv = t;
			      sexp = car(body);
			      goto tco;
		      }
		      in_tco = 1;
		      ans = eval(car(body), cloenv);
		      in_tco = 0;

		      body = next;
		      next = cdr(next);
	      }
	      if (type == LREG_LAMBDA)
		      break;

	      if (in_tco) {
		      /* Macro expand hook? */
		      sexp = ans;
		      /* env unchanged */
		      goto tco;
	      }
	      in_tco = 1;
	      ans = eval(ans, env);
	      in_tco = 0;
	      break;
      }
      break;
    }
    default:
      ans = sexp;
      break;
    }
  return ans;
}


/*
 * Embedded  Procedures
 */

#define ARGEVAL(_lr, _e) ((_e) == NULL ? _lr : eval((_lr), (_e)))

/* Special Form */
LAC_API static lreg_t proc_quote(lreg_t args, lenv_t *argenv, lenv_t *env)
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
		raise_exception("SPLICE expected on car only.", NIL);
      
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
		raise_exception("Dotted pairs in spliced list can be"
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
LAC_API static lreg_t proc_quasiquote(lreg_t args, lenv_t *argenv, lenv_t *env)
{
  lreg_t ret;
  _EXPECT_ARGS(args, 1);
  _qquote(car(args), env, &ret, NULL, 0);
  return ret;
}

LAC_API static lreg_t proc_car(lreg_t args, lenv_t *argenv, lenv_t *env)
{
  _EXPECT_ARGS(args, 1);
  lreg_t arg1 = ARGEVAL(car(args), argenv);

  /* Lisp-specific! */
  if (arg1 == NIL)
    return NIL;

  if ( !is_cons(arg1) )
    _ERROR_AND_RET("argument is not cons");
 
  return car(arg1); 
}

LAC_API static lreg_t proc_cdr(lreg_t args, lenv_t *argenv, lenv_t *env)
{
  _EXPECT_ARGS(args, 1);
  lreg_t arg1 = ARGEVAL(car(args), argenv);

  /* Lisp-specific!
     If I really want to keep this spec I should change cdr() and
     car() to return NIL on NIL and remove these checks. */
  if (arg1 == NIL)
	return NIL;

  if (!is_cons(arg1))
    _ERROR_AND_RET("argument is not cons");

  return cdr(arg1);  
}

LAC_API static lreg_t proc_cons(lreg_t args, lenv_t *argenv, lenv_t *env)
{
  _EXPECT_ARGS(args, 2);
  lreg_t arg1 = ARGEVAL(car(args), argenv);
  lreg_t arg2 = ARGEVAL(car(cdr(args)), argenv);

  return cons(arg1, arg2);
}

LAC_API static lreg_t proc_rplaca(lreg_t args, lenv_t *argenv, lenv_t *env)
{
  _EXPECT_ARGS(args, 2);
  lreg_t arg1 = ARGEVAL(car(args), argenv);
  lreg_t arg2 = ARGEVAL(car(cdr(args)), argenv);

  if ( !is_cons(arg1) )
    _ERROR_AND_RET("argument is not cons");

  rplaca(arg1, arg2);
  return arg1;
}

LAC_API static lreg_t proc_rplacd(lreg_t args, lenv_t *argenv, lenv_t *env)
{
  _EXPECT_ARGS(args, 2);
  lreg_t arg1 = ARGEVAL(car(args), argenv);
  lreg_t arg2 = ARGEVAL(car(cdr(args)), argenv);

  if ( !is_cons(arg1) )
    _ERROR_AND_RET("argument is not cons");

  rplacd(arg1, arg2);
  return arg1;
}

LAC_API static lreg_t proc_eq(lreg_t args, lenv_t *argenv, lenv_t *env)
{
  _EXPECT_ARGS(args, 2);
  lreg_t ans = sym_false;
  lreg_t arg1 = ARGEVAL(car(args), argenv);
  lreg_t arg2 = ARGEVAL(car(cdr(args)), argenv);

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

#if 0
LAC_API static lreg_t proc_apply(lreg_t args, lenv_t *env)
{
  _EXPECT_ARGS(args, 2);
  lreg_t arg1 = eval(car(args), env);
  lreg_t arg2 = eval(car(cdr(args)), env);

  return apply(arg1, arg2, NULL, env);
}
#endif

#if 0
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
	      return cdr(test) == NIL ? cond : evlist(cdr(test), env, 1);
      }
      args = cdr(args);
  }
  return NIL;
}
#endif

/* Special Form */
LAC_API static lreg_t proc_labels(lreg_t args, lenv_t *argenv, lenv_t *env)
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
LAC_API static lreg_t proc_lambda(lreg_t args, lenv_t *argenv, lenv_t *env)
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
LAC_API static lreg_t proc_macro(lreg_t args, lenv_t *argenv, lenv_t *env)
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
LAC_API static lreg_t proc_define(lreg_t args, lenv_t *argenv, lenv_t *env)
{
  lreg_t defd;
  _EXPECT_ARGS(args, 2);

  if ( !is_symbol(car(args)) )
    _ERROR_AND_RET("Syntax error in define");

  defd = eval(car(cdr(args)), env);
  env_define(env, car(args), defd);
  return defd;
}

LAC_API static lreg_t proc_set(lreg_t args, lenv_t *argenv, lenv_t *env)
{
  int r;
  _EXPECT_ARGS(args, 2);
  lreg_t arg1 = ARGEVAL(car(args), argenv);
  lreg_t arg2 = ARGEVAL(car(cdr(args)), argenv);

  if ( !is_symbol(arg1) )
    _ERROR_AND_RET("Syntax error in set");

  r = env_set(env, arg1, arg2);
  if ( r < 0 )
    raise_exception("Error while setting env.", NIL);

  if ( r == 0 )
    return arg2;

  /* Not defined */
  return NIL;
}

LAC_DEFINE_TYPE_PFUNC(cons, LREG_CONS);
LAC_DEFINE_TYPE_PFUNC(symbol, LREG_SYMBOL);

LAC_API static lreg_t proc_gensym(lreg_t args, lenv_t *argenv, lenv_t *env)
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

LAC_API static lreg_t proc_load(lreg_t args, lenv_t *argenv, lenv_t *env)
{
  int r;
  FILE *f;
  char *file;
  void *scan;
  lreg_t res, arg1;
  _EXPECT_ARGS(args, 1);

  arg1 = ARGEVAL(car(args), argenv);
  if ( lreg_type(arg1) != LREG_STRING )
    _ERROR_AND_RET("Syntax error in load");

  lac_extty_unbox(arg1, (void **)&file);
  f = fopen((char *)file, "r");
  if ( f == NULL )
    _ERROR_AND_RET("Could not open file");

  sexpr_read_start(f, &scan);
  lac_on_error({
      sexpr_read_stop(scan);
      _throw(); /* rethrow */
    });
  do {
    r = sexpr_read(&res, scan);
    eval(res, env);
  } while(r);

  lac_off_error();
  sexpr_read_stop(scan);
  return sym_true;
}


/*
 * Initialization Functions
 */

static void machine_init(lenv_t *env)
{
  /* Init symtab. */
  hcreate(500);

  /* Init Null Env */
  memset(env, 0, sizeof(struct env));

  /* Lisp-style booleans.
     Can be changed into Scheme-scheme. */
  sym_false = NIL;
  sym_true = register_symbol("T");
  env_define(env, sym_true, sym_true); /* Tautology. */
  sym_quote = register_symbol("QUOTE");
  env_define(env, sym_quote, llproc_to_lreg(proc_quote));
  sym_cond = register_symbol("COND");
  sym_apply = register_symbol("APPLY");

  lac_extproc_register(env, "LAMBDA", proc_lambda);
  lac_extproc_register(env, "DEFINE", proc_define);
  lac_extproc_register(env, "MACRO", proc_macro);
  lac_extproc_register(env, "LABELS", proc_labels);

  lac_extproc_register(env,"CONS", proc_cons);
  lac_extproc_register(env,"CAR", proc_car);
  lac_extproc_register(env,"CDR", proc_cdr);
  lac_extproc_register(env,"RPLACA", proc_rplaca);
  lac_extproc_register(env,"RPLACD", proc_rplacd);
  lac_extproc_register(env,"EQ", proc_eq);
  lac_extproc_register(env,"LOAD", proc_load);
  lac_extproc_register(env,"SET", proc_set);
  lac_extproc_register(env,"GENSYM", proc_gensym);
  lac_extproc_register(env,"CONSP", LAC_TYPE_PFUNC(cons));
  lac_extproc_register(env,"SYMBOLP", LAC_TYPE_PFUNC(symbol));

  sym_quasiquote = register_symbol("QUASIQUOTE");
  env_define(env, sym_quasiquote, llproc_to_lreg(proc_quasiquote));
  sym_unquote = register_symbol("UNQUOTE");
  sym_splice = register_symbol("SPLICE");
  sym_rest = register_symbol("&REST");
}

void map_init(lenv_t *env);
void int_init(lenv_t *env);
void string_init(lenv_t *env);
static void
modules_init(lenv_t *env)
{
  int_init(env);
  string_init(env);
  map_init(env);
}

static void
library_init(lenv_t *env)
{
  int r;
  FILE *f;  
  lreg_t res;
  void *scan;

  f = fopen("sys.lac", "r");
  if ( f == NULL )
    f = fopen(LAC_SYSDIR"/sys.lac", "r");
  if ( f == NULL )
    raise_exception("SYSTEM LIBRARY NOT FOUND", NIL);

  sexpr_read_start(f, &scan);
  do {
    r = sexpr_read(&res, scan);
    eval(res, env);
  } while(r);
  sexpr_read_stop(scan);

  fclose(f);
}

lenv_t *
lac_init(void)
{
  sigset_t emptyset;
  lenv_t *env;
  GC_init();
 
  sigemptyset(&emptyset); 
  sigprocmask(SIG_BLOCK, &emptyset, &mainsigset);

  stackoverflow_install_handler(stackovf_handler, extra_stack, 16384);
  env = lac_envalloc();
  machine_init(env);
  modules_init(env);
  library_init(env);

  return env;
}

lenv_t *
lac_envalloc(void)
{
  return GC_malloc(sizeof(lenv_t));
}


