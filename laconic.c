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
#include <setjmp.h>
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
jmp_buf main_jmp;

void lac_error(const char *arg)
{
  /* TODO VA ARGS */
  fprintf(stderr, "%s\n", arg);
  _longjmp(main_jmp, 1);
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
      /* Not reached. */
    }

  for (; alist != NIL; alist = cdr(alist))
    {
      lreg_t a = car(alist);
      
      if (!is_cons(a))
	{
	  lac_error("Invalid alist");
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
      lac_error("evargs: invalid arguments");
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
	  lac_error("Incorrect binding list");
          /* Not reached. */
	}
      if ( car(binds) == sym_rest )
	{
          evd = args;
	  if ( !is_cons(cdr(binds)) )
	    {
	      lac_error("Expected binding after &rest");
              /* Not reached. */
	    }
          if ( evenv != NULL )
            evd = evargs(args, evenv);
	  env_define(defenv, car(cdr(binds)), evd);
	  args = NIL;
	  if ( cdr(cdr(binds)) != NIL )
	    {
	      lac_error("Syntax error in bindings\n");
              /* Not reached. */
	    }
	  break;
	}
      if ( args == NIL )
	{
	  lac_error("Not enough arguments to function.");
          /* Not reached. */
	}
      evd = car(args);
      if ( evenv != NULL )
        evd = eval(car(args), evenv);
      env_define(defenv, car(binds), evd);
    }

  if ( args != NIL )
    {
      lac_error("Too many arguments to function.");
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
      fprintf(stderr, "Not a procedure: "); lac_print(stderr, proc); fprintf(stderr, "\n");
      lac_error("");
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
      fprintf(stderr, "Not a procedure: "); lac_print(stderr, proc); fprintf(stderr, "\n");
      lac_error("");
    }
  return _apply(proc, args, env, doeval);
}

static lreg_t eval_sym(lreg_t sym, lenv_t *env)
{
  int r;
  lreg_t ret;
  if (!is_symbol(sym))
    {
      lac_error("Not a symbol.");
      /* Not reached. */
    }

  r = env_lookup(env, sym, &ret);
  if (r != 0)
    {
      if (r == 1)
	  fprintf(stderr, "Symbol not defined: "); lac_print(stderr, sym); fprintf(stderr,"\n");
      lac_error("Env lookup failure.");
    }
  return ret;
}

static lreg_t eval_cons(lreg_t cons, lenv_t *env)
{
  return apply(eval(car(cons), env), cdr(cons), env);
}

lreg_t eval(lreg_t sexp, lenv_t *env)
{
  switch (LREG_TYPE(sexp))
    {
    case LREG_NIL:
      return NIL;
    case LREG_SYMBOL:
      return eval_sym(sexp, env);
    case LREG_CONS:
      return eval_cons(sexp, env);
    default:
      if ( ext_types[LREG_TYPE(sexp)] != NULL )
	return ext_types[LREG_TYPE(sexp)]->eval(sexp);
      else
	lac_error("Undefined Extension Type! This is a serious LAC BUG().");
        /* Not reached. */
    }
  return NIL;
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
		lac_error("SPLICE expected on car only.");
      
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
			    " present only when splicing is at end of a list.");

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

  /* LISP-specific! */
  if (arg1 == NIL)
    return NIL;

  if ( !is_cons(arg1) )
    _ERROR_AND_RET("%s: argument is not cons\n", __func__);
 
  return car(arg1); 
}

LAC_API static lreg_t proc_cdr(lreg_t args, lenv_t *env)
{
  _EXPECT_ARGS(args, 1);
  lreg_t arg1 = eval(car(args), env);

  /* LISP-specific!
     If I really want to keep this spec I should change cdr() and
     car() to return NIL on NIL and remove these checks. */
  if (arg1 == NIL)
	return NIL;

  if (!is_cons(arg1))
    _ERROR_AND_RET("%s: argument is not cons\n", __func__);

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
    _ERROR_AND_RET("%s: argument is not cons\n", __func__);

  get_cons(arg1)->a = arg2;
  return arg1;
}

LAC_API static lreg_t proc_rplacd(lreg_t args, lenv_t *env)
{
  _EXPECT_ARGS(args, 2);
  lreg_t arg1 = eval(car(args), env);
  lreg_t arg2 = eval(car(cdr(args)), env);

  if ( !is_cons(arg1) )
    _ERROR_AND_RET("%s: argument is not cons\n", __func__);

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
      if ( LREG_TYPE(arg1) == LREG_TYPE(arg2)
	   && ext_types[LREG_TYPE(arg1)] != NULL )
	ans = ext_types[LREG_TYPE(arg1)]->eq(arg1, arg2);
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
	_ERROR_AND_RET("Syntax error in cond\n");

      cond = eval(car(test), env);
      /* LISP-specific! Scheme (as for R5RS) checks for #t,
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
  ret = LREG(LREG_PTR(cons(cdr(args), LREG(penv, LREG_NIL))), LREG_LAMBDA);
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
    _ERROR_AND_RET("Syntax error in lambda\n");

  env_pushnew(env, penv);
  return LREG(LREG_PTR(cons(args, LREG(penv, LREG_NIL))), LREG_LAMBDA);
}

/* Special Form */
LAC_API static lreg_t proc_macro(lreg_t args, lenv_t *env)
{
  /* At least 2 arguments required. */
  _EXPECT_MIN_ARGS(args, 2);
  lreg_t binds = car(args);
  lenv_t *penv = GC_malloc(sizeof(lenv_t));

  if ( !is_cons(binds) && binds != NIL )
    _ERROR_AND_RET("Syntax error in macro\n");

  env_pushnew(env, penv);
  return LREG(LREG_PTR(cons(args, LREG(penv, LREG_NIL))), LREG_MACRO);
}

/* Special Form */
LAC_API static lreg_t proc_define(lreg_t args, lenv_t *env)
{
  lreg_t defd;
  _EXPECT_ARGS(args, 2);

  if ( !is_symbol(car(args)) )
    _ERROR_AND_RET("Syntax error in define\n");

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
    _ERROR_AND_RET("Syntax error in set\n");

  r = env_set(env, arg1, arg2);
  if ( r < 0 )
    lac_error("Error while setting env.");

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

static void repl(FILE *fd);
LAC_API static lreg_t proc_load(lreg_t args, lenv_t *env)
{
  _EXPECT_ARGS(args, 1);
  lreg_t arg1 = eval(car(args), env);

  if ( LREG_TYPE(arg1) != LREG_STRING )
    _ERROR_AND_RET("Syntax error in load");

  FILE *fd = fopen((char *)LREG_PTR(arg1), "r");
  if ( fd == NULL )
    _ERROR_AND_RET("Could not open file");

  repl(fd); // XXX: ret value?
  fclose(fd);
  return sym_true;
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
  lreg_t res = NIL;
  int quit = 0;
  struct timeval t1, t2;

  for(;;)
    {
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
	  res = eval(res, &null_env);
	  gettimeofday(&t2, NULL);
	  if ( isatty(fileno(fd)) )
	   {
	      printf("=> "); 
	      lac_print(stdout, res); 
	      printf("\n");
	      printf("Evaluation took %ld seconds and %ld microseconds.\n",
		     t2.tv_usec >= t1.tv_usec ? t2.tv_sec - t1.tv_sec : t2.tv_sec - t1.tv_sec - 1, 
		     t2.tv_usec >= t1.tv_usec ? t2.tv_usec - t1.tv_usec : t2.tv_usec + 1000000L - t1.tv_usec);
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
  if ( _setjmp(main_jmp) != 0 )
    fprintf(stderr, "Error in System Library.\n");
  else 
    library_init();

  if ( _setjmp(main_jmp) != 0 )
    fprintf(stderr, "Error.\n");
  repl(stdin);
  printf("\ngoodbye!\n");
  return 0;
}
