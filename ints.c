/* Simple Type Extension example. */
#include "laconic.h"
#include <gc/gc.h>
#include <stdio.h>

static void int_print(FILE *fd, lreg_t lr)
{
  fprintf(fd, "%ld", *(long*)LREG_PTR(lr));
}

static int int_eval(lreg_t lr, lreg_t *res)
{
  *res = lr;
  return 0;
}

static void int_eq(lreg_t arg1, lreg_t arg2, lreg_t *res)
{
  long n1 = *(unsigned long *)LREG_PTR(arg1);
  long n2 = *(unsigned long *)LREG_PTR(arg2);
  *res = (n1 == n2) ? sym_true : sym_false;
}

#define _BINOP_CHECKS(a, b, new)			\
  _EXPECT_ARGS(args, 2);				\
  lreg_t arg1 = car(args);				\
  lreg_t arg2 = car(cdr(args));				\
							\
  if ( !(LREG_TYPE(arg1) == LREG_TYPE(arg2))		\
       || LREG_TYPE(arg1) != LREG_INTEGER )		\
      _ERROR_AND_RET("+ requires two integers");	\
							\
  a = *(unsigned long *)LREG_PTR(arg1);			\
  b = *(unsigned long *)LREG_PTR(arg2);			\
							\
  new = (long *)GC_malloc(sizeof(unsigned long));

LAC_API static int proc_plus(lreg_t args, lreg_t *env, lreg_t *res)
{
  long n1, n2, *n;
  _BINOP_CHECKS(n1, n2, n);

  *n = n1 + n2;

  *res = LREG(n, LREG_INTEGER);
  return 0;
}

LAC_API static int proc_minus(lreg_t args, lreg_t *env, lreg_t *res)
{
  long n1, n2, *n;
  _BINOP_CHECKS(n1, n2, n);

  *n = n1 - n2;

  *res = LREG(n, LREG_INTEGER);
  return 0;
}

static ext_type_t int_ty = { .print = int_print, .eval = int_eval, .eq = int_eq };

LAC_INITF(int_init)
{
  ext_type_register(LREG_INTEGER, &int_ty);
  bind_symbol(register_symbol("+"), llproc_to_lreg(proc_plus));
  bind_symbol(register_symbol("-"), llproc_to_lreg(proc_minus));
}
