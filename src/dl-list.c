#include "dl-list.h"

#include <malloc.h>
#include <stddef.h>

struct item_t {
    struct item_t *prev;
    struct item_t *next;
    void *ptr;
};
typedef struct item_t item;

struct dl_list_t {
    item *head;
    item *tail;
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

    // free the item headers in the list
    while (iter != NULL) {
        item *tmp_ptr = iter->next;
        free(iter);
        iter = tmp_ptr;
    }

    // free the list head
    free(list);
}

void dl_list_append(dl_list* list, void* ptr)
{
    item *it = malloc(sizeof(item));
    it->ptr = ptr;

    if (!list->head) {
        list->tail = it;
        it->prev = NULL;
        it->next = NULL;
    } else {
        it->next = list->head;
        it->prev = NULL;
        it->next->prev = it;
    }
    list->head = it;
}

void* dl_list_head(dl_list* list)
{
    void* ptr = NULL;
    if (list->head != NULL) {
        ptr = list->head->ptr;
    }
    return ptr;
}

void dl_list_drop(dl_list* list)
{
    item *head = list->head;
    if (!head->prev) {
        list->head = head->next;
    }

    if (!head->next) {
        list->tail = head->prev;
    } else {
        head->next->prev = head->prev;
    }
    free(head);
}
