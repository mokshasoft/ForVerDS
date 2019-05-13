/*
 * Copyright 2019, Mokshasoft AB (mokshasoft.com)
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 3. Note that NO WARRANTY is provided.
 * See "LICENSE.txt" for details.
 */

#include "mempool.h"

#include <malloc.h>

struct node_t {
    struct node_t* prev;
    struct node_t* next;
    char data[];
};
typedef struct node_t node;

struct dl_list_t {
    node* head;
    node* tail;
};
typedef struct dl_list_t dl_list;

struct mempool_t {
    dl_list* slab_list;
    dl_list* free_list;
    dl_list* used_list;
    size_t item_size;
    size_t nbr_items;
};

#define SIZEOF_MEMPOOL (sizeof(mempool))
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

// slab functions

static node* slab_list_get_first_mem_node(node* slab_node)
{
    return (node*)slab_node->data;
}

static node* slab_list_get_last_mem_node(node* slab_node, size_t item_size, size_t nbr_items)
{
    return get_node_ptr(slab_node->data, SIZEOF_MEMPOOL_DATA(item_size, nbr_items) - SIZEOF_NODE_HEADER - item_size);
}

static node* slab_list_create_slab(size_t item_size, size_t nbr_items)
{
    node* slab_node = malloc(SIZEOF_NODE_HEADER + SIZEOF_MEMPOOL_DATA(item_size, nbr_items));
    slab_node->prev = NULL;
    slab_node->next = NULL;
    // create a list in the slab data
    // get the address of the first and last node in the slab
    node* ptr = slab_list_get_first_mem_node(slab_node);
    node* last = slab_list_get_last_mem_node(slab_node, item_size, nbr_items);
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
    return slab_node;
}

static dl_list* slab_list_create(size_t item_size, size_t nbr_items)
{
    // init and alloc list header
    dl_list* slab_list = dl_list_create();
    node* slab_node = slab_list_create_slab(item_size, nbr_items);
    slab_list->head = slab_node;
    slab_list->tail = slab_node;
    return slab_list;
}

static void slab_list_add_slab(mempool* pool)
{
    dl_list* slab_list = pool->slab_list;
    size_t item_size = pool->item_size;
    size_t nbr_items = pool->nbr_items;
    node* slab_node = slab_list_create_slab(item_size, nbr_items);
    dl_list_append(slab_list, slab_node);
    // add all nodes in the new slab to the free list
    pool->free_list->head = slab_list_get_first_mem_node(slab_node);
    pool->free_list->tail = slab_list_get_last_mem_node(slab_node, item_size, nbr_items);
}

static void slab_list_destroy(dl_list* slab_list)
{
    node* itr = slab_list->head;
    node* next = NULL;
    do {
        next = itr->next;
        free(itr);
        itr = next;
    } while (itr);
    dl_list_destroy(slab_list);
}

// mempool functions

mempool* mempool_create(size_t item_size, size_t nbr_items)
{
    // allocate the mempool
    mempool* p = malloc(SIZEOF_MEMPOOL);
    p->item_size = item_size;
    p->nbr_items = nbr_items;
    // init mempool
    p->free_list = dl_list_create();
    p->used_list = dl_list_create();
    p->slab_list = slab_list_create(item_size, nbr_items);
    // add all nodes in the slab to the free list
    p->free_list->head = slab_list_get_first_mem_node(p->slab_list->head);
    p->free_list->tail = slab_list_get_last_mem_node(p->slab_list->head, item_size, nbr_items);
    return p;
}

void mempool_destroy(mempool* pool)
{
    slab_list_destroy(pool->slab_list);
    dl_list_destroy(pool->free_list);
    dl_list_destroy(pool->used_list);
    free(pool);
}

void* mempool_get(mempool* pool)
{
    node* free_node = pool->free_list->head;
    if (!free_node) {
        // no free elements in pool
        slab_list_add_slab(pool);
        return mempool_get(pool);
    } else {
        // move the free node to the used list
        dl_list_remove(pool->free_list, free_node);
        dl_list_append(pool->used_list, free_node);
        return free_node->data;
    }
}

void mempool_return(mempool* pool, void* ptr)
{
    // get address of memory to return
    node* n = get_node_ptr(ptr, -SIZEOF_NODE_HEADER);
    // move the free node to the used list
    dl_list_remove(pool->used_list, n);
    dl_list_append(pool->free_list, n);
}
