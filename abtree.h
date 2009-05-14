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
    void *p;
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
void avltree_cleanup(AvlTree *t)
{
    AvlNode *n;
    AvlNode *tmp;
    if(!t)
        return;
    n = t->root;
    while(n)
    {
        if(n->left)
            n = n->left;
        else if(n->right)
            n = n->right;
        else
        {
            tmp = n;
            n = n->up;
            free(tmp);
            if(!n)
                continue;
            else if(n->left == tmp)
                n->left = NULL;
            else
                n->right = NULL;
        }
    }
}


int avltree_test();

#endif //ABTREE_H
