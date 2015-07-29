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

#include "laconic.h"
#include <stdio.h>
#include <limits.h>

#define INT_UNBOX(lr, n) lac_extty_unbox(lr, (void **)&n)

/*
  LAC Type Interface.
 */

static void int_print(FILE *fd, lreg_t lr)
{
  long *n;

  INT_UNBOX(lr, n);
  fprintf(fd, "%ld ", *n);
}

static lreg_t int_eq(lreg_t arg1, lreg_t arg2)
{
  long *n1, *n2;
  INT_UNBOX(arg1, n1);
  INT_UNBOX(arg2, n2);
  return (*n1 == *n2) ? sym_true : sym_false;
}

static lac_exttype_t int_ty = { 
	.name = "integer",
	.print = int_print,
	.eq = int_eq,
};


/*
  Additional procedures.
 */

#define _BINOP_CHECKS(a, b)				\
  _EXPECT_ARGS(args, 2);				\
  lreg_t arg1 = eval(car(args), env);			\
  lreg_t arg2 = eval(car(cdr(args)), env);		\
							\
  if ( !(lreg_type(arg1) == lreg_type(arg2))		\
       || lreg_type(arg1) != LREG_INTEGER )		\
      _ERROR_AND_RET("+ requires two integers");	\
							\
  lac_extty_unbox(arg1, (void **)&a);			\
  lac_extty_unbox(arg2, (void **)&b);


LAC_API static lreg_t proc_plus(lreg_t args, lenv_t *env)
{
  long *n1, *n2, *n;
  _BINOP_CHECKS(n1, n2);

  if ( ((*n1>0) && (*n2>0) && (*n1 > (LONG_MAX-*n2))) 
       || ((*n1<0) && (*n2<0) && (*n1 < (LONG_MIN-*n2))) )
    _ERROR_AND_RET("+: Integer overflow\n");

  n = lac_alloc(sizeof(*n));
  *n = *n1 + *n2;
  return lac_extty_box(LREG_INTEGER, n, sizeof(*n));
}

LAC_API static lreg_t proc_minus(lreg_t args, lenv_t *env)
{
  long *n1, *n2, *n;
  _BINOP_CHECKS(n1, n2);

  if ( ((*n1>0) && (*n2 < 0) && (*n1 > (LONG_MAX+*n2)))
       || ((*n1<0) && (*n2>0) && (*n1 < (LONG_MIN + *n2))) )
    _ERROR_AND_RET("-: Integer signed overflow\n");

  n = lac_alloc(sizeof(*n));
  *n = *n1 - *n2;
  return lac_extty_box(LREG_INTEGER, n, sizeof(*n));
}

LAC_API static lreg_t proc_star(lreg_t args, lenv_t *env)
{
  long *n1, *n2, *n;
  _BINOP_CHECKS(n1, n2);

  if ( *n1 == 0 || *n2 == 0 )
    goto mul_res;

  if ( *n1 > 0 )
    if ( *n2 > 0 ) {
      if ( *n1>(LONG_MAX/(*n2)) )
	goto mul_of;
    } else {
      if ( *n2<(LONG_MIN/(*n1)) )
	goto mul_of;
    }
  else
    if ( *n2 > 0 ) {
      if ( *n1<(LONG_MIN/(*n2)) )
	goto mul_of;
    } else {
      if ( *n2 < (LONG_MAX/(*n1)) )
	goto mul_of;
    }

 mul_res:
  n = lac_alloc(sizeof(*n));
  *n = *n1 * *n2;
  return lac_extty_box(LREG_INTEGER, n, sizeof(*n));

 mul_of:
  _ERROR_AND_RET("*: Integer sign overflow\n");
  return NIL;
}

LAC_API static lreg_t proc_mod(lreg_t args, lenv_t *env)
{
  long *n1, *n2, *n;
  _BINOP_CHECKS(n1, n2);

  if ( (*n2 == 0 ) || ( (*n1 == LONG_MIN) && (*n2 == -1) ) )
      _ERROR_AND_RET("\%% would overflow or divide by zero\n");

  n = lac_alloc(sizeof(*n));
  *n = *n1 % *n2;
  return lac_extty_box(LREG_INTEGER, n, sizeof(*n));
}

LAC_API static lreg_t proc_div(lreg_t args, lenv_t *env)
{
  long *n1, *n2, *n;
  _BINOP_CHECKS(n1, n2);

  if ( (*n2 == 0 ) || ( (*n1 == LONG_MIN) && (*n2 == -1) ) )
      _ERROR_AND_RET("\%% would overflow or divide by zero\n");

  n = lac_alloc(sizeof(*n));
  *n = *n1 / *n2;
  return lac_extty_box(LREG_INTEGER, n, sizeof(*n));
}

LAC_API static lreg_t proc_greater(lreg_t args, lenv_t *env)
{
  long *n1, *n2;
  _BINOP_CHECKS(n1, n2);
  return *n1 > *n2 ? sym_true : sym_false;
}

LAC_API static lreg_t proc_greatereq(lreg_t args, lenv_t *env)
{
  long *n1, *n2;
  _BINOP_CHECKS(n1, n2);
  return *n1 >= *n2 ? sym_true : sym_false;
}

LAC_API static lreg_t proc_less(lreg_t args, lenv_t *env)
{
  long *n1, *n2;
  _BINOP_CHECKS(n1, n2);
  return *n1 < *n2 ? sym_true : sym_false;
}

LAC_API static lreg_t proc_lesseq(lreg_t args, lenv_t *env)
{
  long *n1, *n2;
  _BINOP_CHECKS(n1, n2);
  return *n1 <= *n2 ? sym_true : sym_false;
}

LAC_DEFINE_TYPE_PFUNC(integer, LREG_INTEGER);

void int_init(lenv_t *env)
{
  lac_extty_register(LREG_INTEGER, &int_ty);
  lac_extproc_register(env, "INTEGERP",LAC_TYPE_PFUNC(integer));
  lac_extproc_register(env, "+", proc_plus);
  lac_extproc_register(env, "-", proc_minus);
  lac_extproc_register(env, "*", proc_star);
  lac_extproc_register(env, "%", proc_mod);
  lac_extproc_register(env, "/", proc_div);
  lac_extproc_register(env, ">", proc_greater);
  lac_extproc_register(env, ">=", proc_greatereq);
  lac_extproc_register(env, "<", proc_less);
  lac_extproc_register(env, "<=", proc_lesseq);
}
