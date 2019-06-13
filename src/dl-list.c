#include "dl-list.h"

#include <malloc.h>
#include <stddef.h>

struct dl_list_t {
    item *head;
    item *tail;
};

struct item_t {
    struct item_t *prev;
    struct item_t *next;
    void *ptr;
};

dl_list* dl_list_create()
{
    dl_list *list = malloc(sizeof(dl_list));
    list->head = NULL;
    list->tail = NULL;
    return list;
}

void dl_list_destroy(dl_list* list)
{
    item *iter = list->head;
    item *stop = list->tail;

    // free the item headers in the list
    while (iter != NULL) {
        item *tmp_ptr = iter->next;
        free(iter);
        iter = tmp_ptr;
    }

    // free the list head
    free(list);
}

item* dl_list_append(dl_list* list, void* ptr)
{
    item *it = malloc(sizeof(item));
    it->ptr = ptr;

    if (!list->head) {
        list->head = it;
        list->tail = it;
        it->prev = NULL;
        it->next = NULL;
    } else {
        it->next = list->head;
        it->prev = NULL;
        it->next->prev = it;
    }
}

item* dl_list_head(dl_list* list)
{
    return list->head;
}

void dl_list_delete_item(dl_list* list, item* it)
{
    if (!it->prev) {
        list->head = it->next;
    } else {
        it->prev->next = it->next;
    }

    if (!it->next) {
        list->tail = it->prev;
    } else {
        it->next->prev = it->prev;
    }

    free(it);
}
