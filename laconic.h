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
#include <inttypes.h>

#ifdef __GNUC__
#define _noreturn __attribute__((noreturn))
#else
#define _noreturn
#endif


/*
 * Basic Types.
 */

struct env;
typedef struct env lenv_t;
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
	void *ptr;
};

typedef struct {
  char *name;
  void (*print)(FILE *fd, lreg_t lr);
  lreg_t (*eq)(lreg_t arg1, lreg_t arg2);
} lac_exttype_t;

static inline lreg_t lreg_raw(void *ptr, unsigned type)
{
  return (lreg_t)((uintptr_t)ptr) | type;
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

void raise_exception(char *, lreg_t) _noreturn;

lenv_t *lac_envalloc(void);
lenv_t * lac_init(void);
extern void *GC_malloc(size_t);
#define lac_alloc GC_malloc

struct cons
{
  lreg_t a;
  lreg_t d;
};

static inline struct cons *
get_cons(lreg_t lr)
{
  if (lreg_raw_type(lr) == LREG_CONS)
    return (struct cons *)lreg_raw_ptr(lr);
  raise_exception("not a cons", lr);
}


/*
 * Embedded procedures
 */

#define LAC_API __attribute__((aligned(16)))
typedef lreg_t (*lac_function_t)(lreg_t args, lenv_t *env);


/*
 * Extension
 */

void lac_extproc_register(lenv_t *env, const char *sym, lac_function_t f);

int lac_extty_register(unsigned typeno, lac_exttype_t *extty);
lreg_t lac_extty_box(unsigned typeno, void *ptr, size_t size);
size_t lac_extty_unbox(lreg_t lr, void **ptr);
unsigned lac_extty_get_type(lreg_t lr);
size_t lac_extty_get_size(lreg_t lr);
int lac_extty_print(FILE * fd, lreg_t lr);


/*
 * Lisp Machine
 */

#define NIL lreg_raw(0,LREG_NIL)
extern lreg_t sym_true;
extern lreg_t sym_false;
extern lreg_t sym_quote;
extern lreg_t sym_quasiquote;
extern lreg_t sym_unquote;
extern lreg_t sym_splice;
extern lreg_t sym_rest;

#define car(_lr) ((_lr) == NIL ? NIL : get_cons(_lr)->a)
#define cdr(_lr) ((_lr) == NIL ? NIL : get_cons(_lr)->d)
#define rplaca(_lr, _a) do { get_cons(_lr)->a = (_a); } while(0)
#define rplacd(_lr, _d) do { get_cons(_lr)->d = (_d); } while(0)
lreg_t evargs(lreg_t list, lenv_t *env);
lreg_t eval(lreg_t list, lenv_t *env);
lreg_t apply(lreg_t proc, lreg_t args, lenv_t *argenv, lenv_t *env);
lreg_t cons(lreg_t a, lreg_t b);
lreg_t intern_symbol(char *s);

#define _ERROR_AND_RET(err)	\
  do {				\
    raise_exception(err, NIL);	\
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
  if ( lreg_type(arg1) == typeno )					\
    return sym_true;							\
  else									\
    return sym_false;							\
}
#define LAC_TYPE_PFUNC(typename) proc_##typename##p

/*
 * Exception handling.
 * Simple SJLJ for now(?).
 */

#include <setjmp.h>
#include <stdlib.h>

struct _lac_xcpt {
  sigjmp_buf buf;
  struct _lac_xcpt *next;
};

/* Per thread. Correct but ugh. */
extern __thread struct _lac_xcpt *_lac_xcpt;
extern __thread char *_lac_xcpt_msg;
extern __thread lreg_t _lac_xcpt_reg;

#define lac_errlreg() _lac_xcpt_reg
#define lac_errmsg() _lac_xcpt_msg

#define lac_on_error(_b) do {					\
    struct _lac_xcpt *p = malloc(sizeof(struct _lac_xcpt));	\
    p->next = _lac_xcpt;					\
    _lac_xcpt = p;						\
    if ( sigsetjmp(p->buf, 1) != 0 ) {				\
      { _b };							\
    }								\
  } while(0)

#define lac_off_error() do {			\
    struct _lac_xcpt *p = _lac_xcpt;		\
    _lac_xcpt = p->next;			\
    free(p);					\
  } while(0)


/*
 * Representations
 */

void sexpr_read_start(FILE *f, void **yyscan);
int sexpr_read(lreg_t *res, void *yyscan);
void sexpr_read_stop(void *yyscan);
void sexpr_print(FILE *f, lreg_t lr);  

#endif /* LACONIC_H */
