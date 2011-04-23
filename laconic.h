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
#include <assert.h>
#include <gc/gc.h>

/*
 * LREG/TREG model.
 *
 * We require GC-allocated memory to be 8-byte aligned, so that we can
 * use the least significant three bits for storing type information
 * (up to seven types). Extended types (types whose id cannot be
 * decoded in three bits) are encoded via the EXTT-type LREG, whose
 * PTR points to a TREG, capable of storing a full pointer and a
 * bigger tag.
 * This is potentially slow, since we will allocate memory with the GC
 * everytime we create a LREG.
 * As an optimization, in x86-64 systems we exploit the fact that
 * every pointer has unused bits from the 48th to the 63th. These
 * 16bits are used to store the full pointer (in the upper 48 bits)
 * and a full 12-bit tag (bits 4 to 16) and the 3 bit tag (0 to
 * 2). Bit 3 is unused to avoid more complicated masks that affect
 * performances.
 */

typedef uintptr_t lreg_t;
typedef struct {
  unsigned tag;
  void *ptr;
} treg_t;

#define LREG_TYPE_MASK 0x7
enum 
  {
    LREG_CONS = 0,  /* Cons cells */
    LREG_SYMBOL,    /* Symbols */
    LREG_SFORM,     /* Special forms. */
    LREG_LLPROC,    /* C ll procedures */
    LREG_LAMBDA,    /* Lambda procedures. */
    LREG_MACRO,     /* Macro procedures. */
    LREG_NIL,       /* NIL */
    LREG_EXTT,
    LREG_STRING,
    LREG_INTEGER,
    /* Not implemented yet */
    LREG_FLOAT,
    LREG_TYPES = 16
  };

static inline unsigned lreg_type(lreg_t lr)
{
  treg_t *tr;
  if ( (lr & LREG_TYPE_MASK) != LREG_EXTT )
    return lr & LREG_TYPE_MASK;

#ifdef __x86_64__
  return (lr >> 4) & 0xfff;
#else
  tr = (treg_t *)((uintptr_t)lr & ~LREG_TYPE_MASK);
  return tr->tag;
#endif
}

static inline void *lreg_ptr(lreg_t lr)
{
  treg_t *tr;
  if ( (lr & LREG_TYPE_MASK) != LREG_EXTT )
    return (void *)((uintptr_t)lr & ~LREG_TYPE_MASK);

#ifdef __x86_64__
  return (void *)(lr >> 16);
#else
  tr = (treg_t *)((uintptr_t)lr & ~LREG_TYPE_MASK);
  return tr->ptr;
#endif
}

static inline lreg_t lreg(void *ptr, unsigned type)
{
  treg_t *tr;
  if ( type < LREG_EXTT )
    return (lreg_t)((uintptr_t)ptr & ~LREG_TYPE_MASK) | type;

#ifdef __x86_64__
  assert( type == (type & 0xfff) );
  return (lreg_t)(((uintptr_t)ptr << 16) | (type << 4) | LREG_EXTT);
#else
  tr = GC_malloc(sizeof(treg_t));
  tr->tag = type;
  tr->ptr = ptr;
  return (lreg_t)(((uintptr_t)tr & ~LREG_TYPE_MASK) | LREG_EXTT);
#endif
}

#define LREG_PTR(lr) lreg_ptr(lr)
#define LREG_TYPE(lr) lreg_type(lr)
#define LREG(ptr, ty) lreg(ptr, ty)
#define NIL LREG(0,LREG_NIL)

#define HT_SIZE 31

struct ht_entry
{
  lreg_t key;
  lreg_t value;
  struct ht_entry *next;
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
  assert(is_cons(lr));
  return (cons_t *)LREG_PTR(lr);
}

#define is_symbol(lr) (LREG_TYPE(lr) == LREG_SYMBOL)

lreg_t cons(lreg_t a, lreg_t b);
lreg_t car(lreg_t lr);
lreg_t cdr(lreg_t lr);
lreg_t intern_symbol(char *s);


/*
 * Embedded procedures
 */
#define is_llproc(lr) (LREG_TYPE(lr) == LREG_LLPROC)
#define lreg_to_llproc(lr) lreg_to_cfunc(lr)
#define llproc_to_lreg(llproc) cfunc_to_lreg(llproc, LREG_LLPROC)

#define is_sform(lr) (LREG_TYPE(lr) == LREG_SFORM)
#define lreg_to_sform(lr) lreg_to_cfunc(lr)
#define sform_to_lreg(sform) cfunc_to_lreg(sform, LREG_SFORM)

#define LAC_API __attribute__((aligned(16)))

typedef int (*lac_function_t)(lreg_t args, lenv_t *env, lreg_t *res);
static inline lac_function_t lreg_to_cfunc(lreg_t lr)
{
  assert(is_llproc(lr) || is_sform(lr));
  return (lac_function_t)LREG_PTR(lr);
}
static inline lreg_t cfunc_to_lreg(lac_function_t llproc, unsigned type)
{
  assert(((uintptr_t)llproc & LREG_TYPE_MASK) == 0);
  return LREG(llproc, type);
}

/*
 * Macro/Lambda procedures
 */
#define is_lambda(lr) (LREG_TYPE(lr) == LREG_LAMBDA)
#define is_macro(lr) (LREG_TYPE(lr) == LREG_MACRO)
static inline lreg_t get_closure_proc(lreg_t lr)
{
  lreg_t c = LREG(LREG_PTR(lr), LREG_CONS);
  assert((LREG_TYPE(lr) == LREG_LAMBDA)
	 || (LREG_TYPE(lr) == LREG_MACRO));
  
  return car(c);
}
static inline lenv_t *get_closure_env(lreg_t lr)
{
  lreg_t c = LREG(LREG_PTR(lr), LREG_CONS);
  assert((LREG_TYPE(lr) == LREG_LAMBDA)
	 || (LREG_TYPE(lr) == LREG_MACRO));

  return (lenv_t *)LREG_PTR(cdr(c));
}
static inline lreg_t get_proc_binds(lreg_t lr)
{
   return car(lr);
}
static inline lreg_t get_proc_evlist(lreg_t lr)
{
  return cdr(lr);
}    

extern lreg_t sym_true;
extern lreg_t sym_false;
extern lreg_t sym_quote;
extern lreg_t sym_quasiquote;
extern lreg_t sym_unquote;
extern lreg_t sym_splice;

typedef struct {
  void (*print)(FILE *fd, lreg_t lr);
  int (*eval)(lreg_t lr, lreg_t *res);
  void (*eq)(lreg_t arg1, lreg_t arg2, lreg_t *res);
} ext_type_t;

int ext_type_register(int typeno, ext_type_t *extty);

void bind_symbol(lreg_t sym, lreg_t val);
lreg_t register_symbol(const char *s);
int eval(lreg_t list, lenv_t *env, lreg_t *res);
int apply(lreg_t proc, lreg_t args, lenv_t *env, lreg_t *res);


#define _ERROR_AND_RET(err, ...)		\
  do {						\
    fprintf(stderr, err, ##__VA_ARGS__);	\
    return -1;					\
  } while ( 0 )

#define __EXPECT_MIN_ARGS__(args, num)					\
  do {									\
    int i;								\
    for ( i = 0; i < num; tmp = cdr(tmp), i++ )				\
      if ( tmp == NIL )							\
	_ERROR_AND_RET("Not enough arguments to %s\n", __func__);	\
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
      _ERROR_AND_RET("Too Many arguments to %s\n", __func__);		\
  } while ( 0 )


#define LAC_DEFINE_TYPE_PFUNC(typename, typeno)				\
LAC_API static int proc_##typename##p (lreg_t args, lenv_t *env, lreg_t *res) \
{									\
  _EXPECT_ARGS(args, 1);						\
  if ( LREG_TYPE(car(args)) == typeno )					\
    *res = sym_true;							\
  else									\
    *res = sym_false;							\
  return 0;								\
}

#define LAC_TYPE_PFUNC(typename) proc_##typename##p


/*
 * Environment management.
 */
void env_init(lenv_t *env);
int env_lookup(lenv_t *env, lreg_t key, lreg_t *res);
int env_define(lenv_t *env, lreg_t key, lreg_t value);
int env_set(lenv_t *env, lreg_t key, lreg_t value);
void env_pushnew(lenv_t *env, lenv_t *new);


#endif /* LACONIC_H */
