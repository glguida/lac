#include "laconic.h"
#include <string.h>

/*
 * External type handling.
 */
static lac_exttype_t *ext_types[LREG_TYPES];

#define EXTTY_IS_VALID(typeno) \
	( typeno > LREG_EXTT && typeno <= LREG_TYPES )

int lac_extty_register(unsigned typeno, lac_exttype_t *extty)
{
  if ( !EXTTY_IS_VALID(typeno) )
    return -1;
  ext_types[typeno] = extty;
  return 0;
}

unsigned lac_extty_get_type(lreg_t lr)
{
	struct treg_hdr *treg = lreg_raw_ptr(lr);
	return treg->type;
}

size_t lac_extty_get_size(lreg_t lr)
{
	struct treg_hdr *treg = lreg_raw_ptr(lr);
	return treg->size;
}

lreg_t lac_extty_box(unsigned typeno, void *ptr, size_t sz)
{
	struct treg_hdr *treg = GC_malloc(sizeof(struct treg_hdr) + sz);
	treg->type = typeno;
	treg->size = sz;
	treg->ptr = ptr;

	return lreg_raw(treg, LREG_EXTT);
}


size_t lac_extty_unbox(lreg_t lr, void **ptr)
{
	struct treg_hdr *treg = lreg_raw_ptr(lr);

	if (ptr)
		*ptr = treg->ptr;
	return treg->size;
}

int lacint_extty_print(FILE *fd, lreg_t lr)
{
	unsigned typeno = lac_extty_get_type(lr);
	if ( EXTTY_IS_VALID(typeno)
	     && ext_types[typeno] != NULL )
		ext_types[typeno]->print(fd, lr);
        else
		return 0;

	return 1;
}

int lacint_extty_eq(lreg_t arg1, lreg_t arg2, lreg_t *ans)
{
	unsigned typeno1 = lac_extty_get_type(arg1);
	unsigned typeno2 = lac_extty_get_type(arg2);

	if ( EXTTY_IS_VALID(typeno1)
	     && EXTTY_IS_VALID(typeno2)
	     && typeno1 == typeno2 
	     && ext_types[typeno1] != NULL )
		*ans = ext_types[typeno1]->eq(arg1, arg2);
        else
		return 0;
	return 1;
}
