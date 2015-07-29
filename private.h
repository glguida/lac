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

#ifndef __PRIVATE_H
#define __PRIVATE_H

#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include <setjmp.h>
#include <gc/gc.h>
#include "laconic.h"

#ifdef NO_ASSERT
#define assert(...)
#else
#include <assert.h>
#endif

#define is_llproc(lr) (lreg_raw_type(lr) == LREG_LLPROC)
#define is_symbol(lr) (lreg_raw_type(lr) == LREG_SYMBOL)
#define is_cons(lr) (lreg_raw_type(lr) == LREG_CONS)
#define is_lambda(lr) (lreg_raw_type(lr) == LREG_LAMBDA)
#define is_macro(lr) (lreg_raw_type(lr) == LREG_MACRO)

int lacint_extty_eq(lreg_t arg1, lreg_t arg2, lreg_t * ans);

#define HT_SIZE 32

struct ht_entry {
	lreg_t key;
	lreg_t value;
	struct ht_entry *next;
};

struct ht_cache {
	lreg_t key;
	lreg_t value;
};

typedef struct ht {
	struct ht_entry *table[HT_SIZE];
} ht_t;

struct env {
	ht_t htable;
};

/*
 * Embedded procedures
 */

#define lreg_to_llproc(lr) lreg_to_cfunc(lr)
#define llproc_to_lreg(llproc) cfunc_to_lreg(llproc, LREG_LLPROC)

typedef lreg_t(*lac_function_t) (lreg_t args, lenv_t * env);
static inline lac_function_t lreg_to_cfunc(lreg_t lr)
{
	assert(is_llproc(lr));
	return (lac_function_t) lreg_raw_ptr(lr);
}

static inline lreg_t cfunc_to_lreg(lac_function_t llproc, unsigned type)
{
	assert(((uintptr_t) llproc & LREG_TYPE_MASK) == 0);
	return lreg_raw(llproc, type);
}

/*
 * Macro/Lambda procedures
 */

static inline lreg_t get_closure_proc(lreg_t lr)
{
	lreg_t c = lreg_raw(lreg_raw_ptr(lr), LREG_CONS);
	assert((lreg_raw_type(lr) == LREG_LAMBDA)
	       || (lreg_raw_type(lr) == LREG_MACRO));

	return car(c);
}

static inline lenv_t *get_closure_env(lreg_t lr)
{
	lreg_t c = lreg_raw(lreg_raw_ptr(lr), LREG_CONS);
	assert((lreg_raw_type(lr) == LREG_LAMBDA)
	       || (lreg_raw_type(lr) == LREG_MACRO));

	return (lenv_t *) lreg_raw_ptr(cdr(c));
}

static inline lreg_t get_proc_binds(lreg_t lr)
{
	return car(lr);
}

static inline lreg_t get_proc_body(lreg_t lr)
{
	return cdr(lr);
}


/*
 * Private exception management. 
 */

#define _throw() do {				\
	struct _lac_xcpt *p = _lac_xcpt;	\
	_lac_xcpt = p->next;			\
	longjmp(p->buf, 1);			\
    } while(0)

/*
 * Environment management.
 */

void env_init(lenv_t * env);
lreg_t env_lookup(lenv_t * env, lreg_t key);
int env_define(lenv_t * env, lreg_t key, lreg_t value);
int env_set(lenv_t * env, lreg_t key, lreg_t value);
void env_pushnew(lenv_t * env, lenv_t * new);


#endif				/* PRIVATE_H */
