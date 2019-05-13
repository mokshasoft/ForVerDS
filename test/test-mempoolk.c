/*
 * Copyright 2019, Mokshasoft AB (mokshasoft.com)
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 3. Note that NO WARRANTY is provided.
 * See "LICENSE.txt" for details.
 */

#include "mempoolk.h"

#include <assert.h>
#include <string.h>

void test1()
{
    mempoolk* pool;
    pool = mempoolk_create(7, 1);
    mempoolk_destroy(pool);
}

void test2()
{
    mempoolk* pool;
    pool = mempoolk_create(1, 10);
    mempoolk_destroy(pool);
}

void test3()
{
    int items = 10;
    int item_size = 100;
    mempoolk* pool;
    pool = mempoolk_create(item_size, items);
    for (int i = 0; i < 2 * items; i++) {
        void* ptr = mempoolk_get(pool);
        memset(ptr, (char)i, item_size);
        mempoolk_return(pool, ptr);
    }
    mempoolk_destroy(pool);
}

int test4()
{
    mempoolk* pool;
    pool = mempoolk_create(10, 1);
    void* ptr1 = mempoolk_get(pool);
    void* ptr2 = mempoolk_get(pool);
    mempoolk_return(pool, ptr1);
    if (ptr2 != NULL) {
        mempoolk_return(pool, ptr2);
    }
    mempoolk_destroy(pool);
    return ptr1 != NULL && ptr2 == NULL;
}

int main()
{
    test1();
    test2();
    test3();
    assert(test4());
    return 0;
}
