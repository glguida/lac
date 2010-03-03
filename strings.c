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
  fprintf(fd, "\"%s\"", (char *)LREG_PTR(lr));
}

static int string_eval(lreg_t lr, lreg_t *res)
{
  *res = lr;
  return 0;
}

static void string_eq(lreg_t arg1, lreg_t arg2, lreg_t *res)
{
  char *s1 = (char *)LREG_PTR(arg1);
  char *s2 = (char *)LREG_PTR(arg2);
  *res = strcmp(s1, s2) != 0 ? sym_false : sym_true;
}

static ext_type_t string_ty = { .print = string_print, .eval = string_eval, .eq = string_eq };

LAC_INITF(string_init)
{
  ext_type_register(LREG_STRING, &string_ty);
}
