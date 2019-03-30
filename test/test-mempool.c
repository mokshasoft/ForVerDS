/*
 * 2019, Mokshasoft AB (mokshasoft.com)
 *
 * This software may be distributed and modified according to the terms of
 * the Unlicense license. Note that NO WARRANTY is provided.
 * See "LICENSE.txt" for details.
 */

#include "mempool.h"

#include <assert.h>
#include <string.h>

void test1()
{
    mempool* pool;
    pool = mempool_create(7, 1);
    mempool_destroy(pool);
}

void test2()
{
    mempool* pool;
    pool = mempool_create(1, 10);
    mempool_destroy(pool);
}

void test3()
{
    int items = 10;
    int item_size = 100;
    mempool* pool;
    pool = mempool_create(item_size, items);
    for (int i = 0; i < 2 * items; i++) {
        void* ptr = mempool_get(pool);
        memset(ptr, 'a', item_size);
        mempool_return(pool, ptr);
    }
    mempool_destroy(pool);
}

int test4()
{
    mempool* pool;
    pool = mempool_create(10, 1);
    void* ptr1 = mempool_get(pool);
    void* ptr2 = mempool_get(pool);
    mempool_destroy(pool);
    return ptr1 != NULL && ptr2 == NULL;
}

int test5()
{
#define ITEMS 10
    int item_size = 100;
    void* ptrs[ITEMS];
    mempool* pool;
    pool = mempool_create(item_size, ITEMS);
    for (int i = 0; i < ITEMS; i++) {
        ptrs[i] = mempool_get(pool);
        memset(ptrs[i], (char)i, item_size);
        if (!ptrs[i]) {
            return 0;
        }
    }
    if (mempool_get(pool)) {
        return 0;
    }
    for (int i = 0; i < ITEMS; i++) {
        mempool_return(pool, ptrs[i]);
    }
    mempool_destroy(pool);
    return 1;
}

int main()
{
    test1();
    test2();
    test3();
    assert(test4());
    assert(test5());
    return 0;
}
