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
typedef void (AvlTreeCleanupCb)(void *);
void avltree_cleanup(AvlTree *t, AvlTreeCleanupCb *cb);
char *avltree_find(AvlTree *t, char *p);
AvlNode *avltree_findnode(AvlTree *t, char *p);

typedef int (*AvlTreeTraverser)(AvlNode *node,void *ctxt);

int avltree_traverse(AvlTree *tree, AvlTreeTraverser fp, void *ctxt);


int avltree_test();

#endif //ABTREE_H
