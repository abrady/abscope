/***************************************************************************
 *     Copyright (c) 2009-2009, Aaron Brady
 *     All Rights Reserved
 *
 * Module Description:
 *
 *
 ***************************************************************************/
#ifndef ABTREE_H
#define ABTREE_H

typedef struct AvlNode
{
    char *p;
    struct AvlNode *left;
    struct AvlNode *right;
    struct AvlNode *up;
    int  height;
} AvlNode;

typedef struct AvlTree
{
    AvlNode *root;
    int (*cmp)(const void*, const void*);
} AvlTree;

// called after an insert of node n
void avltree_insert(AvlTree *t, void *p);
void avltree_clear(AvlTree *t);


int avltree_test();

#endif //ABTREE_H
