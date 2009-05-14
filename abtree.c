/***************************************************************************
 *     Copyright (c) 2009-2009, Aaron Brady
 *     All Rights Reserved
 *
 * Module Description:
 *
 *
 ***************************************************************************/
#include "abutil.h"
#include "abtree.h"

static void avltree_rebalance(AvlTree *t, AvlNode *n_in)
{
    int n_l;
    int n_r;
    AvlNode *n;
    AvlNode **up;
    AvlNode *tmp;
    AvlNode *r;

#define AVL_NODE_HEIGHT(n) (MAX(DEREF(n->left,height),DEREF(n->right,height)) + 1)

    for(n = n_in; n; n = n->up)
    {
        n_l = n->left  ? n->left->height  : 0;
        n_r = n->right ? n->right->height : 0;
        n->height = (n_l > n_r ? n_l : n_r) + 1;

        if((n_l - n_r) > 1)
        {
            // left heavy. move left branch up.
            r = n->left;
            tmp = r->right;
            r->right = n;
            n->left = tmp;
        }
        else if((n_r - n_l) > 1)
        {
            // right heavy: right becomes parent.
            r = n->right;
            tmp = r->left;
            r->left = n;
            n->right = tmp;
        }
        else
            continue;

        // rotation happened
        n->height = AVL_NODE_HEIGHT(n);
        r->height = AVL_NODE_HEIGHT(r);
        
        tmp = n->up;
        n->up = r;
        r->up = tmp;
        if(n == t->root && n->up)
            t->root = n->up;
    }
}

void avltree_insert(AvlTree *t, void *p)
{
    AvlNode **n = &t->root;
    AvlNode *up = NULL;
    
    if(!t || !node)
        return;
    while(*n)
    {
        if(t->cmp((*n)->p,p) < 0)
            n = &((*n)->left);
        else
            n = &((*n)->right);
        up = *n;
    }
    (*n) = calloc(sizeof(AvlNode),1);
    (*n)->p = p;
    (*n)->up = up;
    avltree_rebalance(t,(*n));
}

#define TEST(COND) if(!(COND)) {printf(#COND ": failed\n"); return -1;}

int avltree_test()
{
    AvlTree t = {0};
    AvlNode *n;

    t.cmp = strcmp;
    avltree_insert(&t,"A");
    avltree_insert(&t,"B");
    avltree_insert(&t,"C");

    TEST(t->root);
    n = t->root;
    TEST(0==strcmp(n->p,"B"));
    TEST(0==strcmp(n->left->p,"A"));
    TEST(0==strcmp(n->right->p,"C"));

    //          B             B
    //        A   C         x   C
    //      w         =>   w A
    //        x                

    avltree_insert(&t,"w");
    avltree_insert(&t,"x");
    TEST(0==strcmp(n->p,"B"));
    TEST(0==strcmp(n->right->p,"C"));

    n = n->left;
    TEST(0==strcmp(n->p,"w"));
    TEST(!n->left);
    TEST(0==strcmp(n->right->p,"A"));
    TEST(n->right->left);
    TEST(0==strcmp(n->right->left->p,"x"));
    
    

    avltree_cleanup(&t);
}
