#include "laconic.h"
#include <gc/gc.h>

/*
 * Utterly simple hash table implementation
 */

#define HT_SIZE 17

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

static unsigned ht_hashf(lreg_t key)
{
  uintptr_t n;
  n = (key >> 6) % HT_SIZE;
  return n;
}

static int _ht_findptr(struct ht_entry *hte, lreg_t key, struct ht_entry **e)
{
  if ( hte == NULL )
    return 1;

  if ( hte->key == key )
    {
      *e = hte;
      return 0;
    }

  return _ht_findptr(hte->next, key, e);
}

/* Ret values: < 0 => error, 0 => found, 1 not found */
static int ht_findptr(ht_t *ht, lreg_t key, struct ht_entry **e)
{
  unsigned n = ht_hashf(key);
  assert(n < HT_SIZE);
  return _ht_findptr(ht->table[n], key, e);
}

/* Ret values: < 0 => error, 0 => found, 1 not found */
static int ht_find(ht_t *ht, lreg_t key, lreg_t *res)
{
  int n;
  struct ht_entry *hte;
  n = ht_findptr(ht, key, &hte);
  if ( n == 0 )
    *res = hte->value;
  return n;
}

static int _ht_insert(struct ht_entry **htep, lreg_t key, lreg_t value)
{
  struct ht_entry *hte = *htep;
  if ( hte == NULL )
    {
      struct ht_entry *e = GC_malloc(sizeof(struct ht_entry));
      e->key = key;
      e->value = value;
      e->next = NULL;
      *htep = e;
      return 0;
    }
 
  if ( hte->key == key )
    {
      hte->value = value;
      return 0;
    }

  return _ht_insert(&hte->next, key, value);
}

static int ht_insert(ht_t *ht, lreg_t key, lreg_t value)
{
  unsigned n = ht_hashf(key);
  assert(n < HT_SIZE);
  return _ht_insert(ht->table + n, key, value);
}


/*
 * Environment stacks. Mostly Activation Records.
 */

struct env
{
  ht_t *htable;
  struct env *prev;
};

/* Ret values: < 0 => error, 0 => found, 1 not found */
int env_lookup(lenv_t *env, lreg_t key, lreg_t *res)
{
  int r = ht_find(env->htable, key, res);
  if ( r < 1 )
    return r;

  /* Not found, try searching other envs */
  if ( env->prev == NULL )
    return 1;

  return env_lookup(env->prev, key, res);
}

int env_define(lenv_t *env, lreg_t key, lreg_t value)
{
  /* Add to the toplevel table. */
  return ht_insert(env->htable, key, value);
}

/* Ret values: < 0 => error, 0 => found, 1 not found */
int env_set(lenv_t *env, lreg_t key, lreg_t value)
{
  int n;
  struct ht_entry *hte;
  n = ht_findptr(env->htable, key, &hte);
  if ( n == 0 )
      hte->value = value;
  if ( (n == 1) && env->prev != NULL )
    return env_set(env->prev, key, value);
  return n;
}

lenv_t *env_pushnew(lenv_t *env)
{
  lenv_t *new = GC_malloc(sizeof(lenv_t));
  new->htable = GC_malloc(sizeof(ht_t));
  new->prev = env;
  return new;
}

#if 0

static lenv_t le;
int main ()
{
  int n1, n2;
  lreg_t res = 0;
  lenv_t *le2;
  GC_INIT();

  le.htable = GC_malloc(sizeof(ht_t));
  
  n1 = env_set(&le, LREG(0x505000, 5), LREG(0xa0a000, 0xa));
  printf("n1 = %d\n", n1);
  n1 = env_lookup(&le, LREG(0x505000, 5), &res);
  printf("%llx: %d\n", res, n1);
  n1 = env_define(&le, LREG(0x505000, 5), LREG(0xa0a000, 0xa));
  printf("n1 = %d\n", n1);
  n1 = env_lookup(&le, LREG(0x505000, 5), &res);
  printf("%llx: %d\n", res, n1);

  printf("Creating new env\n");

  le2 = env_pushnew(&le);
  n1 = env_lookup(le2, LREG(0x505000, 5), &res);
  printf("%llx: %d\n", res, n1);
  n1 = env_define(le2, LREG(0x505000, 5), LREG(0xb0b000, 0xa));
  printf("n1 = %d\n", n1);
  n1 = env_lookup(le2, LREG(0x505000, 5), &res);
  printf("%llx: %d\n", res, n1);

  printf("Back to old env\n");

  n1 = env_lookup(&le, LREG(0x505000, 5), &res);
  printf("%llx: %d\n", res, n1);


}

#endif 
