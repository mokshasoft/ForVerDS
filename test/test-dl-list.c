#include "dl-list.h"
#include <assert.h>

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
    item* it;
    int item = 3;
    dl_list* list;
    list = dl_list_create();
    dl_list_append(list, &item);
    it = dl_list_head(list);
    //assert(it->ptr == 3);
    dl_list_delete_item(list, it);
    dl_list_destroy(list);
}

void test5()
{
    item* it;
    int item = 4;
    dl_list* list;
    list = dl_list_create();
    dl_list_append(list, &item);
    dl_list_append(list, &item);
    dl_list_append(list, &item);
    dl_list_append(list, &item);
    it = dl_list_head(list);
    dl_list_delete_item(list, it);
    it = dl_list_head(list);
    dl_list_delete_item(list, it);
    it = dl_list_head(list);
    dl_list_delete_item(list, it);
    it = dl_list_head(list);
    dl_list_delete_item(list, it);
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
