/*
 * Copyright 2019, Mokshasoft AB (mokshasoft.com)
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 3. Note that NO WARRANTY is provided.
 * See "LICENSE.txt" for details.
 */

#ifndef _MEMPOOL_H_
#define _MEMPOOL_H_

#include <stddef.h>

typedef struct mempool_t mempool;

mempool* mempool_create(size_t item_size, size_t nbr_items);
void mempool_destroy(mempool* pool);

void* mempool_get(mempool* pool);
void mempool_return(mempool* pool, void* ptr);

#endif
