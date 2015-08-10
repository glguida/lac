#include "private.h"
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
	struct treg_hdr *treg = lac_alloc(sizeof(struct treg_hdr) + sz);
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

int lac_extty_print(FILE *fd, lreg_t lr)
{
	unsigned typeno = lac_extty_get_type(lr);
	if ( EXTTY_IS_VALID(typeno)
	     && ext_types[typeno] != NULL )
		ext_types[typeno]->print(fd, lr);
        else
		return 0;

	return 1;
}

int lacint_extty_equal(lreg_t arg1, lreg_t arg2)
{
	int rc = 0;
	unsigned typeno1 = lac_extty_get_type(arg1);
	unsigned typeno2 = lac_extty_get_type(arg2);

	if ( !EXTTY_IS_VALID(typeno1)
	     || !EXTTY_IS_VALID(typeno2)
	     || typeno1 != typeno2 
	     || ext_types[typeno1] == NULL )
		raise_exception("Internal error", NIL);

	if (ext_types[typeno1]->equal == NULL) {
		void *ptr1 = ((struct treg_hdr *)lreg_raw_ptr(arg1))->ptr;
		void *ptr2 = ((struct treg_hdr *)lreg_raw_ptr(arg2))->ptr;
		rc = ptr1 == ptr2;
	} else
		rc = ext_types[typeno1]->equal(arg1, arg2);

	return rc;
}
