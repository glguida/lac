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

/* Simple Type Extension example. */
#include "laconic.h"
#include <stdio.h>
#include <limits.h>

static void int_print(FILE *fd, lreg_t lr)
{
  fprintf(fd, "%ld ", (long)LREG_PTR(lr));
}

static lreg_t int_eval(lreg_t lr)
{
  return lr;
}

static lreg_t int_eq(lreg_t arg1, lreg_t arg2)
{
  long n1 = (long)LREG_PTR(arg1);
  long n2 = (long)LREG_PTR(arg2);
  return (n1 == n2) ? sym_true : sym_false;
}

#define _BINOP_CHECKS(a, b)				\
  _EXPECT_ARGS(args, 2);				\
  lreg_t arg1 = eval(car(args), env);			\
  lreg_t arg2 = eval(car(cdr(args)), env);		\
							\
  if ( !(LREG_TYPE(arg1) == LREG_TYPE(arg2))		\
       || LREG_TYPE(arg1) != LREG_INTEGER )		\
      _ERROR_AND_RET("+ requires two integers");	\
							\
  a = (long)LREG_PTR(arg1);				\
  b = (long)LREG_PTR(arg2);


LAC_API static lreg_t proc_plus(lreg_t args, lenv_t *env)
{
  long n1, n2, n;
  _BINOP_CHECKS(n1, n2);

  if ( ((n1>0) && (n2>0) && (n1 > (LONG_MAX-n2))) 
       || ((n1<0) && (n2<0) && (n1 < (LONG_MIN-n2))) )
    _ERROR_AND_RET("+: Integer overflow\n");

  n = n1 + n2;
  return LREG((void *)n, LREG_INTEGER);
}

LAC_API static lreg_t proc_minus(lreg_t args, lenv_t *env)
{
  long n1, n2, n;
  _BINOP_CHECKS(n1, n2);

  if ( ((n1>0) && (n2 < 0) && (n1 > (LONG_MAX+n2)))
       || ((n1<0) && (n2>0) && (n1 < (LONG_MIN + n2))) )
    _ERROR_AND_RET("-: Integer signed overflow\n");
  
  n = n1 - n2;
  return LREG((void *)n, LREG_INTEGER);
}

LAC_API static lreg_t proc_star(lreg_t args, lenv_t *env)
{
  long n1, n2, n;
  _BINOP_CHECKS(n1, n2);

  if ( n1 == 0 || n2 == 0 )
    goto mul_res;

  if ( n1 > 0 )
    if ( n2 > 0 ) {
      if ( n1>(LONG_MAX/n2) )
	goto mul_of;
    } else {
      if ( n2<(LONG_MIN/n1) )
	goto mul_of;
    }
  else
    if ( n2 > 0 ) {
      if ( n1<(LONG_MIN/n2) )
	goto mul_of;
    } else {
      if ( n2 < (LONG_MAX/n1) )
	goto mul_of;
    }

 mul_res:
  n = n1 * n2;
  return LREG((void *)n, LREG_INTEGER);

 mul_of:
  _ERROR_AND_RET("*: Integer sign overflow\n");
  return NIL;
}

LAC_API static lreg_t proc_mod(lreg_t args, lenv_t *env)
{
  long n1, n2, n;
  _BINOP_CHECKS(n1, n2);

  if ( (n2 == 0 ) || ( (n1 == LONG_MIN) && (n2 == -1) ) )
      _ERROR_AND_RET("\%% would overflow or divide by zero\n");

  n = n1 % n2;
  return LREG((void *)n, LREG_INTEGER);
}

LAC_API static lreg_t proc_div(lreg_t args, lenv_t *env)
{
  long n1, n2, n;
  _BINOP_CHECKS(n1, n2);

  if ( (n2 == 0 ) || ( (n1 == LONG_MIN) && (n2 == -1) ) )
      _ERROR_AND_RET("\%% would overflow or divide by zero\n");

  n = n1 / n2;
  return LREG((void *)n, LREG_INTEGER);
}

LAC_DEFINE_TYPE_PFUNC(integer, LREG_INTEGER);

static ext_type_t int_ty = { .print = int_print, .eval = int_eval, .eq = int_eq };

void int_init(void)
{
  ext_type_register(LREG_INTEGER, &int_ty);
  bind_symbol(register_symbol("INTEGERP"), llproc_to_lreg(LAC_TYPE_PFUNC(integer)));
  bind_symbol(register_symbol("+"), llproc_to_lreg(proc_plus));
  bind_symbol(register_symbol("-"), llproc_to_lreg(proc_minus));
  bind_symbol(register_symbol("*"), llproc_to_lreg(proc_star));
  bind_symbol(register_symbol("%"), llproc_to_lreg(proc_mod));
  bind_symbol(register_symbol("/"), llproc_to_lreg(proc_div));
}
