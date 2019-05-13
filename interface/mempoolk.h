/*
 * Copyright 2019, Mokshasoft AB (mokshasoft.com)
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 3. Note that NO WARRANTY is provided.
 * See "LICENSE.txt" for details.
 */

#ifndef _MEMPOOLK_H_
#define _MEMPOOLK_H_

#include <stddef.h>

typedef struct mempoolk_t mempoolk;

mempoolk* mempoolk_create(size_t item_size, size_t nbr_items);
void mempoolk_destroy(mempoolk* pool);

void* mempoolk_get(mempoolk* pool);
void mempoolk_return(mempoolk* pool, void* ptr);

#endif
