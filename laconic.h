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

#ifndef __LACONIC_H
#define __LACONIC_H

#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include <gc/gc.h>

#ifdef NO_ASSERT
#define assert(...)
#else
#include <assert.h>
#endif

#ifdef __GNUC__
#define _noreturn __attribute__((noreturn))
#else
#define _noreturn
#endif


/*
 * Basic Types.
 */

typedef uintptr_t lreg_t;


/*
 * LREG/Type model.
 *
 * We require GC-allocated memory to be 8-byte aligned, so that we can
 * use the least significant three bits for storing type information
 * (up to seven types). Extended types (types whose id cannot be
 * decoded in three bits) are encoded via the EXTT-type LREG, whose
 * PTR points to a TREG, capable of storing a full object and a
 * bigger tag.
 */

enum lreg_type
  {
    LREG_CONS = 0,  /* Cons cells. */
    LREG_SYMBOL,    /* Symbols. */
    LREG_ENV,       /* Symbol Table */
    LREG_LLPROC,    /* C procedures. */
    LREG_LAMBDA,    /* Lambda procedures. */
    LREG_MACRO,     /* Macro procedures. */
    LREG_NIL,       /* NIL. */
    LREG_EXTT,      /* External Type. */
    /* EXTTYs */
    LREG_STRING,    /* String, Fixed External type. */
    LREG_INTEGER,   /* Integers, Fixed External type. */
    LREG_AVAIL,
    LREG_TYPES=256
  };
#define LREG_TYPE_MASK 0x7


/*
 * EXTTY handling.
 */

struct treg_hdr {
	unsigned type;
   size_t   size;
};

typedef struct {
  char *name;
  void (*print)(FILE *fd, lreg_t lr);
  lreg_t (*eval)(lreg_t lr);
  lreg_t (*eq)(lreg_t arg1, lreg_t arg2);
} lac_exttype_t;

int lac_extty_register(unsigned typeno, lac_exttype_t *extty);
lreg_t lac_extty_box(unsigned typeno, void *ptr, size_t size);
size_t lac_extty_unbox(lreg_t lr, void *ptr, size_t maxsz);
unsigned lac_extty_get_type(lreg_t lr);
size_t lac_extty_get_size(lreg_t lr);

#ifdef _LAC_INTERNAL
int lacint_extty_print(FILE *fd, lreg_t lr);
int lacint_extty_eq(lreg_t arg1, lreg_t arg2, lreg_t *ans);
int lacint_extty_eval(lreg_t lr, lreg_t *ans);
#endif

static inline lreg_t lreg_raw(void *ptr, unsigned type)
{
  return (lreg_t)((uintptr_t)ptr & ~LREG_TYPE_MASK) | type;
}

static inline void *lreg_raw_ptr(lreg_t lr)
{
  return (void *)((uintptr_t)lr & ~LREG_TYPE_MASK);
}

static inline uintptr_t lreg_raw_type(lreg_t lr)
{
  return (uintptr_t)lr & LREG_TYPE_MASK;
}

static inline unsigned lreg_type(lreg_t lr)
{
  unsigned raw_type = lr & LREG_TYPE_MASK;

  switch(raw_type) {
  case LREG_EXTT:
    return ((struct treg_hdr *)lreg_raw_ptr(lr))->type;
  default:
    return raw_type;
  }
}


#define LREG_TYPE(lr) lreg_type(lr)
#define NIL lreg_raw(0,LREG_NIL)

#define HT_SIZE 32

struct ht_entry
{
  lreg_t key;
  lreg_t value;
  struct ht_entry *next;
};

struct ht_cache
{
  lreg_t key;
  lreg_t value;
};

typedef struct ht
{
  struct ht_entry *table[HT_SIZE];
} ht_t;

typedef struct env
{
  ht_t htable;
} lenv_t;

struct cons
{
  lreg_t a;
  lreg_t d;
};
typedef struct cons cons_t;

#define is_cons(lr) (LREG_TYPE(lr) == LREG_CONS)
static inline cons_t *get_cons(lreg_t lr)
{
  if (lreg_raw_type(lr) == LREG_CONS)
    return (cons_t *)lreg_raw_ptr(lr);
  lac_error("not a cons", lr);

}

#define is_symbol(lr) (LREG_TYPE(lr) == LREG_SYMBOL)

lreg_t cons(lreg_t a, lreg_t b);
lreg_t intern_symbol(char *s);

#define car(_lr) (get_cons(_lr)->a)
#define cdr(_lr) (get_cons(_lr)->d)
#define rplaca(_lr, _a) do { get_cons(_lr)->a = (_a); } while(0)
#define rplacd(_lr, _d) do { get_cons(_lr)->d = (_d); } while(0)


/*
 * Embedded procedures
 */
#define is_llproc(lr) (LREG_TYPE(lr) == LREG_LLPROC)
#define lreg_to_llproc(lr) lreg_to_cfunc(lr)
#define llproc_to_lreg(llproc) cfunc_to_lreg(llproc, LREG_LLPROC)

#define LAC_API __attribute__((aligned(16)))

typedef lreg_t (*lac_function_t)(lreg_t args, lenv_t *env);
static inline lac_function_t lreg_to_cfunc(lreg_t lr)
{
  assert(is_llproc(lr) || is_sform(lr));
  return (lac_function_t)lreg_raw_ptr(lr);
}
static inline lreg_t cfunc_to_lreg(lac_function_t llproc, unsigned type)
{
  assert(((uintptr_t)llproc & LREG_TYPE_MASK) == 0);
  return lreg_raw(llproc, type);
}

/*
 * Macro/Lambda procedures
 */
#define is_lambda(lr) (LREG_TYPE(lr) == LREG_LAMBDA)
#define is_macro(lr) (LREG_TYPE(lr) == LREG_MACRO)
static inline lreg_t get_closure_proc(lreg_t lr)
{
  lreg_t c = lreg_raw(lreg_raw_ptr(lr), LREG_CONS);
  assert((LREG_TYPE(lr) == LREG_LAMBDA)
	 || (LREG_TYPE(lr) == LREG_MACRO));
  
  return car(c);
}
static inline lenv_t *get_closure_env(lreg_t lr)
{
  lreg_t c = lreg_raw(lreg_raw_ptr(lr), LREG_CONS);
  assert((LREG_TYPE(lr) == LREG_LAMBDA)
	 || (LREG_TYPE(lr) == LREG_MACRO));

  return (lenv_t *)lreg_raw_ptr(cdr(c));
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
 * System symbols
 */
extern lreg_t sym_true;
extern lreg_t sym_false;
extern lreg_t sym_quote;
extern lreg_t sym_quasiquote;
extern lreg_t sym_unquote;
extern lreg_t sym_splice;
extern lreg_t sym_rest;

void bind_symbol(lreg_t sym, lreg_t val);
lreg_t register_symbol(const char *s);
lreg_t evargs(lreg_t list, lenv_t *env);
lreg_t eval(lreg_t list, lenv_t *env);
lreg_t apply(lreg_t proc, lreg_t args, lenv_t *env);

#define _ERROR_AND_RET(err)	\
  do {				\
    lac_error(err, NIL);	\
  } while ( 0 )

#define __EXPECT_MIN_ARGS__(args, num)					\
  do {									\
    int i;								\
    for ( i = 0; i < num; tmp = cdr(tmp), i++ )				\
      if ( tmp == NIL )							\
	_ERROR_AND_RET("Not enough arguments");	\
  } while ( 0 )

#define _EXPECT_MIN_ARGS(args, num)					\
  do {									\
    lreg_t tmp = args;							\
    __EXPECT_MIN_ARGS__(args, num);					\
  } while ( 0 )

#define _EXPECT_ARGS(args, num)						\
  do {									\
    lreg_t tmp = args;							\
    __EXPECT_MIN_ARGS__(args, num);					\
    if ( tmp != NIL )							\
      _ERROR_AND_RET("Too Many arguments");				\
  } while ( 0 )




#define LAC_DEFINE_TYPE_PFUNC(typename, typeno)				\
LAC_API static lreg_t proc_##typename##p (lreg_t args, lenv_t *env)	\
{									\
  _EXPECT_ARGS(args, 1);						\
  lreg_t arg1 = eval(car(args), env);					\
  if ( LREG_TYPE(arg1) == typeno )					\
    return sym_true;							\
  else									\
    return sym_false;							\
}

#define LAC_TYPE_PFUNC(typename) proc_##typename##p

void lac_error(char *, lreg_t) _noreturn;
/*
 * Environment management.
 */
void env_init(lenv_t *env);
lreg_t env_lookup(lenv_t *env, lreg_t key);
int env_define(lenv_t *env, lreg_t key, lreg_t value);
int env_set(lenv_t *env, lreg_t key, lreg_t value);
void env_pushnew(lenv_t *env, lenv_t *new);

#endif /* LACONIC_H */
