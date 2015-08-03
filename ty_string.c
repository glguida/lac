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
#include <string.h>

#define ARGEVAL(_lr, _e) ((_e) == NULL ? _lr : eval((_lr), (_e)))

static void string_print(FILE *fd, lreg_t lr)
{

  char *s;
  lac_extty_unbox(lr, (void **)&s);
  fprintf(fd, "\"%s\" ", s);
}

static lreg_t string_eq(lreg_t arg1, lreg_t arg2)
{
  char *s1, *s2;
  lac_extty_unbox(arg1, (void **)&s1);
  lac_extty_unbox(arg2, (void **)&s2);
  if ( s1 == s2 )
    return sym_true;
  return sym_false;
}

static int string_compare(lreg_t arg1, lreg_t arg2)
{
  int d;
  char *s1, *s2;
  lac_extty_unbox(arg1, (void **)&s1);
  lac_extty_unbox(arg2, (void **)&s2);
  d = strcmp(s1, s2);
  printf("strcmp = %s %s %d\n", s1, s2, d);
  return d;
}

#define BINARY_STR_OP_CHECKS(args)			\
  _EXPECT_ARGS(args, 2);				\
  lreg_t s1 = ARGEVAL(car(args), argenv);		\
  lreg_t s2 = ARGEVAL(car(cdr(args)), argenv);		\
							\
  if ( lreg_type(s1) != lreg_type(s2)			\
       || !(lreg_type(s1) == LREG_STRING) )		\
    _ERROR_AND_RET("Function requires two strings!\n");


LAC_API lreg_t proc_string_lessp(lreg_t args, lenv_t *argenv, lenv_t *env)
{
  BINARY_STR_OP_CHECKS(args);
  return (string_compare(s1, s2) >= 0 ? sym_false : sym_true);
}

LAC_API static lreg_t proc_string_greaterp(lreg_t args, lenv_t *argenv, lenv_t *env)
{
  BINARY_STR_OP_CHECKS(args);
  return (string_compare(s1, s2) <= 0 ? sym_false : sym_true);
}

LAC_API static lreg_t proc_string_equal(lreg_t args, lenv_t *argenv, lenv_t *env)
{
  BINARY_STR_OP_CHECKS(args);
  return (string_compare(s1, s2) != 0 ? sym_false : sym_true);
}

LAC_DEFINE_TYPE_PFUNC(string, LREG_STRING)

static lac_exttype_t string_ty = {
	.name = "string",
	.print = string_print,
	.eq = string_eq
};

void string_init(lenv_t *env)
{
  lac_extty_register(LREG_STRING, &string_ty);
  lac_extproc_register(env, "STRINGP", LAC_TYPE_PFUNC(string));
  lac_extproc_register(env, "STRING-LESS", proc_string_lessp);
  lac_extproc_register(env, "STRING-GREATER", proc_string_greaterp);
  lac_extproc_register(env, "STRING-EQUAL", proc_string_equal);
}
