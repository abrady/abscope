/***************************************************************************
 *     Copyright (c) 2009-2009, Aaron Brady
 *     All Rights Reserved
 *
 * Module Description:
 *
 *
 ***************************************************************************/
#ifndef ABHASHTABLE_H
#define ABHASHTABLE_H

#include "abutil.h"

typedef struct HashNode
{
    char *key;
    void *p;
    U32 hash;
} HashNode;

typedef struct HashTable
{
    HashNode *elts;
    int n_elts;
    int n_used;
    void *ctxt;
    U32 (*hashfp)(char *key, void *ctxt);
    int (*cmpfp)(const void*, const void*);
} HashTable;

HashNode *hash_findnode(HashTable *ht, char *key);
void     *hash_find(HashTable *ht, char *key);
BOOL      hash_exists(HashTable *ht, char *key);
BOOL      hash_insert(HashTable *ht, char *key, void *p);
void      hash_cleanup(HashTable *ht);
int       hash_test();

#endif //ABHASHTABLE_H