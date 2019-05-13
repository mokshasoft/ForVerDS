/*
 * Copyright 2019, Mokshasoft AB (mokshasoft.com)
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 3. Note that NO WARRANTY is provided.
 * See "LICENSE.txt" for details.
 */

#include "mempoolk.h"

#include <malloc.h>

struct node_t {
    struct node_t* prev;
    struct node_t* next;
};
typedef struct node_t node;

struct dl_list_t {
    node* head;
    node* tail;
};
typedef struct dl_list_t dl_list;

struct mempoolk_t {
    void* slab;
    dl_list* free_list;
    dl_list* used_list;
};

#define SIZEOF_MEMPOOL (sizeof(mempoolk))
#define SIZEOF_NODE_HEADER (2*sizeof(node*))
#define SIZEOF_MEMPOOL_DATA(item_size, nbr_items) ((SIZEOF_NODE_HEADER + (item_size))*(nbr_items))
#define get_node_ptr(ptr, offset) ((node*)((char*)(ptr) + (offset)))

// list functions

static dl_list* dl_list_create()
{
    dl_list* l = malloc(sizeof(dl_list));
    l->head = NULL;
    l->tail = NULL;
    return l;
}

static void dl_list_destroy(dl_list* list)
{
    free(list);
}

static void dl_list_remove(dl_list* list, node* node)
{
    if (!node->prev) {
        list->head = node->next;
    } else {
        node->prev->next = node->next;
    }

    if (!node->next) {
        list->tail = node->prev;
    } else {
        node->next->prev = node->prev;
    }
}

static void dl_list_append(dl_list* list, node* n)
{
    if (!list->head) {
        list->head = n;
        list->tail = n;
        n->prev = NULL;
        n->next = NULL;
    } else {
        node* tail = list->tail;
        list->tail = n;
        tail->next = n;
        n->prev = tail;
        n->next = NULL;
    }
}

// mempool functions

mempoolk* mempoolk_create(size_t item_size, size_t nbr_items)
{
    // allocate the mempool
    mempoolk* p = malloc(SIZEOF_MEMPOOL);
    void* data = malloc(SIZEOF_MEMPOOL_DATA(item_size, nbr_items));
    // init mempool
    p->free_list = dl_list_create();
    p->used_list = dl_list_create();
    p->slab = data;
    // get the address of the first and last node in the slab
    node* ptr = data;
    node* last = get_node_ptr(data, SIZEOF_MEMPOOL_DATA(item_size, nbr_items) - SIZEOF_NODE_HEADER - item_size);
    // add all nodes in the slab to the free list
    p->free_list->head = ptr;
    p->free_list->tail = last;
    // handle first and last node
    ptr->prev = NULL;
    last->next = NULL;
    // let all nodes in the slab point to the previous and next nodes
    for (; ptr < last;) {
        node* next = get_node_ptr(ptr, SIZEOF_NODE_HEADER + item_size);
        ptr->next = next;
        if (ptr != last) {
            next->prev = ptr;
            ptr = next;
        }
    }
    return p;
}

void mempoolk_destroy(mempoolk* pool)
{
    free(pool->slab);
    dl_list_destroy(pool->free_list);
    dl_list_destroy(pool->used_list);
    free(pool);
}

void* mempoolk_get(mempoolk* pool)
{
    node* free_node = pool->free_list->head;
    if (!free_node) {
        // no free elements in pool
        return NULL;
    } else {
        // get address of memory to get
        void* ptr = get_node_ptr(free_node, SIZEOF_NODE_HEADER);
        // move the free node to the used list
        dl_list_remove(pool->free_list, free_node);
        dl_list_append(pool->used_list, free_node);
        return ptr;
    }
}

void mempoolk_return(mempoolk* pool, void* ptr)
{
    // get address of memory to return
    node* n = get_node_ptr(ptr, -SIZEOF_NODE_HEADER);
    // move the free node to the used list
    dl_list_remove(pool->used_list, n);
    dl_list_append(pool->free_list, n);
}
