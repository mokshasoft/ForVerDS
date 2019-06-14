#include "dl-list.h"
#include <assert.h>
#include <stddef.h>

void test1()
{
    dl_list* list;
    list = dl_list_create();
    dl_list_destroy(list);
}

void test2()
{
    int item = 1;
    dl_list* list;
    list = dl_list_create();
    dl_list_append(list, &item);
    dl_list_destroy(list);
}

void test3()
{
    int item = 2;
    dl_list* list;
    list = dl_list_create();
    dl_list_append(list, &item);
    dl_list_append(list, &item);
    dl_list_append(list, &item);
    dl_list_append(list, &item);
    dl_list_append(list, &item);
    dl_list_destroy(list);
}

void test4()
{
    int item = 3;
    int *head = NULL;
    dl_list* list;
    list = dl_list_create();
    dl_list_append(list, &item);
    head = dl_list_head(list);
    assert(*head == item);
    dl_list_drop(list);
    dl_list_destroy(list);
}

void test5()
{
    int item = 4;
    int *head = NULL;
    dl_list* list;
    list = dl_list_create();
    dl_list_append(list, &item);
    dl_list_append(list, &item);
    dl_list_append(list, &item);
    dl_list_append(list, &item);
    head = dl_list_head(list);
    assert(*head == item);
    dl_list_drop(list);
    head = dl_list_head(list);
    assert(*head == item);
    dl_list_drop(list);
    head = dl_list_head(list);
    assert(*head == item);
    dl_list_drop(list);
    head = dl_list_head(list);
    assert(*head == item);
    dl_list_drop(list);
    dl_list_destroy(list);
}

int main()
{
    test1();
    test2();
    test3();
    test4();
    test5();
    return 0;
}
