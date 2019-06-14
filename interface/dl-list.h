#ifndef _DL_LINKED_LIST_H_
#define _DL_LINKED_LIST_H_

// Define the types
typedef struct dl_list_t dl_list;

// Define constructor and destructor
dl_list* dl_list_create();
void dl_list_destroy(dl_list* list);

// Define list functions
void dl_list_append(dl_list* list, void* ptr);
void* dl_list_head(dl_list* list);
void dl_list_drop(dl_list* list);

#endif
