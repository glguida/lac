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

/* Mapping functions */
#include "laconic.h"
#include <gc/gc.h>

static int map_args(lreg_t lists, lreg_t *res)
{
  int r = 1;
  lreg_t outargs = NIL, tail;
  lreg_t args = lists;

  for ( ; args != NIL ; args = cdr(args) )
    {
      if ( !is_cons(args) ||
	   !is_cons(car(args)) )
	{
	  if ( is_cons(args) && car(args) == NIL )
	    {
	      r = 0;
	      break;
	    }

	  lac_error("Syntax Error in mapcar");
	  return -1;
	}

      if ( outargs == NIL )
	outargs = tail = cons(car(car(args)), NIL);
      else
	get_cons(tail)->d = tail = cons(car(car(args)), NIL);

      get_cons(args)->a = cdr(car(args));
    }

  *res = outargs;
  return r;
}

LAC_API static int proc_mapcar(lreg_t args, lreg_t *env, lreg_t *res)
{
  _EXPECT_MIN_ARGS(args, 2);
  int r;
  lreg_t mapargs;
  lreg_t fn = car(args);
  lreg_t lists = cdr(args);
  lreg_t outlist = NIL, tail;

  switch ( LREG_TYPE(fn) )
    {
    case LREG_LAMBDA:
    case LREG_MACRO:
    case LREG_LLPROC:
    case LREG_SFORM:
      break;
    default:
      _ERROR_AND_RET("Syntax error in mapcar");
    }


  for (;;)
    {
      lreg_t outelm;
      r = map_args(lists, &mapargs);
      
      if ( r < 0 )
	return r;
      
      if ( r == 0 )
	break;

      r = apply(fn, mapargs, env, &outelm);
      if ( r != 0 )
	return r;
      
      if ( outlist == NIL )
	outlist = tail = cons(outelm, NIL);
      else
	get_cons(tail)->d = tail = cons(outelm, NIL);
    }
  
  *res = outlist;
  return 0;

}

LAC_API static int proc_reduce(lreg_t args, lreg_t *env, lreg_t *res)
{
  _EXPECT_ARGS(args, 2);
  int r;
  lreg_t acc;
  lreg_t fn = car(args);
  lreg_t list;

  if ( !is_cons(car(cdr(args))) )
      _ERROR_AND_RET("Syntax error in reduce\n");

  list = car(cdr(args));
  acc = car(list);
  list = cdr(list);

  for ( ; list != NIL; list = cdr(list) )
    {
      r = apply(fn, cons(acc, cons(car(list), NIL)), env, &acc);
      if ( r != 0 )
	return r;
    }

  *res = acc;
  return 0;
}

LAC_INITF(map_init)
{
  bind_symbol(register_symbol("MAPCAR"), llproc_to_lreg(proc_mapcar));
  bind_symbol(register_symbol("REDUCE"), llproc_to_lreg(proc_reduce));
}