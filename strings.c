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

/* String type */
#include "laconic.h"
#include <gc/gc.h>
#include <stdio.h>
#include <strings.h>

static void string_print(FILE *fd, lreg_t lr)
{
  fprintf(fd, "\"%s\" ", (char *)LREG_PTR(lr));
}

static int string_eval(lreg_t lr, lreg_t *res)
{
  *res = lr;
  return 0;
}

static void string_eq(lreg_t arg1, lreg_t arg2, lreg_t *res)
{
  lreg_t ans = sym_false;
  if ( arg1 == arg2 )
    ans = sym_true;
  *res = ans;
}

static int string_compare(lreg_t arg1, lreg_t arg2)
{
  char *s1 = (char *)LREG_PTR(arg1);
  char *s2 = (char *)LREG_PTR(arg2);
  return strcmp(s1, s2);
}

#define BINARY_STR_OP_CHECKS(args)			\
  _EXPECT_ARGS(args, 2);				\
  lreg_t s1 = car(args);				\
  lreg_t s2 = car(cdr(args));				\
							\
  if ( LREG_TYPE(s1) != LREG_TYPE(s2)			\
       || !(LREG_TYPE(s1) == LREG_STRING) )		\
    _ERROR_AND_RET("Function requires two strings!\n");


LAC_API int proc_string_lessp(lreg_t args, lenv_t *env, lreg_t *res)
{
  BINARY_STR_OP_CHECKS(args);
  *res = string_compare(s1, s2) >= 0 ? sym_false : sym_true;
  return 0;
}

LAC_API static int proc_string_greaterp(lreg_t args, lenv_t *env, lreg_t *res)
{
  BINARY_STR_OP_CHECKS(args);
  *res = string_compare(s1, s2) <= 0 ? sym_false : sym_true;
  return 0;
}

LAC_API static int proc_string_equal(lreg_t args, lenv_t *env, lreg_t *res)
{
  BINARY_STR_OP_CHECKS(args);
  *res = string_compare(s1, s2) != 0 ? sym_false : sym_true;
  return 0;
}

LAC_DEFINE_TYPE_PFUNC(string, LREG_STRING)

static ext_type_t string_ty = { .print = string_print, .eval = string_eval, .eq = string_eq };

LAC_INITF(string_init, MODULE_FUNCTION)
{
  ext_type_register(LREG_STRING, &string_ty);
  bind_symbol(register_symbol("STRINGP"), llproc_to_lreg(LAC_TYPE_PFUNC(string)));
  bind_symbol(register_symbol("STRING-LESSP"), llproc_to_lreg(proc_string_lessp));
  bind_symbol(register_symbol("STRING-GREATERP"), llproc_to_lreg(proc_string_greaterp));
  bind_symbol(register_symbol("STRING-EQUAL"), llproc_to_lreg(proc_string_equal));
}
