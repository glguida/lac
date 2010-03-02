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

typedef uintptr_t lreg_t;
#define LREG_TYPE_MASK 0x7
#define LREG_PTR(lr) ((uintptr_t)lr & ~LREG_TYPE_MASK)
#define LREG_TYPE(lr) (lr & LREG_TYPE_MASK)
#define LREG(ptr, ty) (lreg_t)(LREG_PTR(ptr) | LREG_TYPE(ty))

#define NIL LREG(0,LREG_NIL)

enum 
  {
    LREG_CONS = 0,  /* Cons cells */
    LREG_SYMBOL,    /* Symbols */
    LREG_SFORM,     /* Special forms. */
    LREG_LLPROC,    /* C ll procedures */
    LREG_LAMBDA,    /* Lambda procedures. */
    LREG_MACRO,     /* Macro procedures. */
    LREG_NIL,       /* NIL */
    /* Not implemented yet */
    LREG_INTEGER,
    LREG_FLOAT,
    LREG_TYPES
  };

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

typedef int (*lac_function_t)(lreg_t args, lreg_t *env, lreg_t *res);
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
static inline lreg_t get_closure_env(lreg_t lr)
{
  lreg_t c = LREG(LREG_PTR(lr), LREG_CONS);
  assert((LREG_TYPE(lr) == LREG_LAMBDA)
	 || (LREG_TYPE(lr) == LREG_MACRO));

  return cdr(c);
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

typedef struct {
  void (*print)(FILE *fd, lreg_t lr);
  int (*eval)(lreg_t lr, lreg_t *res);
  void (*eq)(lreg_t arg1, lreg_t arg2, lreg_t *res);
} ext_type_t;

int ext_type_register(int typeno, ext_type_t *extty);

#define LAC_INITF(f)					\
static void f (void) __attribute__((constructor));	\
static void f (void)

void bind_symbol(lreg_t sym, lreg_t val);
lreg_t register_symbol(const char *s);

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


#endif /* LACONIC_H */
