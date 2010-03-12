#include "laconic.h"
#include <gc/gc.h>
#include <strings.h>

/*
 * Utterly simple hash table implementation
 */

static unsigned ht_hashf(lreg_t key)
{
  return (key >> 5) % HT_SIZE;
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
  struct ht_entry *e = GC_malloc(sizeof(struct ht_entry));
  e->key = key;
  e->value = value;
  e->next = hte;
  *htep = e;
  return 0;
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

/* Ret values: < 0 => error, 0 => found, 1 not found */
int env_lookup(lenv_t *env, lreg_t key, lreg_t *res)
{
  return ht_find(&env->htable, key, res);
}

int env_define(lenv_t *env, lreg_t key, lreg_t value)
{
  return ht_insert(&env->htable, key, value);
}

/* Ret values: < 0 => error, 0 => found, 1 not found */
int env_set(lenv_t *env, lreg_t key, lreg_t value)
{
  int r;
  struct ht_entry *hte;
  r = ht_findptr(&env->htable, key, &hte);
  if ( r == 0 )
      hte->value = value;
  return r;
}

void env_pushnew(lenv_t *env, lenv_t *new)
{
  if ( env == NULL )
    memset(new, 0, sizeof(lenv_t));
  else
    new->htable = env->htable;
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