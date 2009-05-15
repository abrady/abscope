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

// ***********************************************************************
// For a left heavy tree (symmetric):
//     X
//   W   _
// ( k+1 k-1)
//  L  R
// Two cases: 
// Case 1: L = k, R = k-1
// - rotate W up, R to X->left
//      W     
//   L     X   
// ( k     k)
//       R    _ 
// (     k-1  k-1)
//
// Case 2: L = k-1, R = k
// - in this case setting X->l to W->r keeps same depth: k+1,
//   and W->l = k-1, so no change. instead rotate rotate w->l to the left
//   so it will now be left heavy as well (within bounds), making it match
//   case 1, apply case 1.
// ***********************************************************************
static void avltree_rebalance(AvlTree *t, AvlNode *n_in)
{
    int n_l;
    int n_r;
    AvlNode *n;
    AvlNode *tmp;
    AvlNode *r;

#define AVL_NODE_HEIGHT(n) (MAX(DEREF(n->left,height),DEREF(n->right,height)) + 1)

#define ROT_FIXUP(n,r)        n->height = AVL_NODE_HEIGHT(n);   \
    r->height = AVL_NODE_HEIGHT(r);                             \
    tmp = n->up;                                                \
    n->up = r;                                                  \
    r->up = tmp;                                                \
    if(n == t->root && n->up)                                   \
        t->root = n->up;                                        \
    n = r


#define ROT_RIGHT(n) {                          \
        r = n->left;                            \
        tmp = r->right;                         \
        r->right = n;                           \
        n->left = tmp;                          \
        ROT_FIXUP(n,r);                         \
    }

#define ROT_LEFT(n) {                           \
        r = n->right;                           \
        tmp = r->left;                          \
        r->left = n;                            \
        n->right = tmp;                         \
        ROT_FIXUP(n,r);                         \
    } 
    

    for(n = n_in; n; n = n->up)
    {
        n_l = n->left  ? n->left->height  : 0;
        n_r = n->right ? n->right->height : 0;
        n->height = (n_l > n_r ? n_l : n_r) + 1;

        if((n_l - n_r) > 1)
        {
            // left heavy. move left branch up.
            if(DEREF2(n->left,right,height) > DEREF2(n->left,left,height))
                ROT_LEFT(n->left);
            ROT_RIGHT(n);
        }
        else if((n_r - n_l) > 1)
        {
            // right heavy: right becomes parent.
            if(DEREF2(n->right,left,height) > DEREF2(n->right,right,height))
                ROT_RIGHT(n->right);
            ROT_LEFT(n);
        }
    }
}

void avltree_insert(AvlTree *t, void *p)
{
    AvlNode **n = &t->root;
    AvlNode *up = NULL;
    
    if(!t || !p)
        return;
    while(*n)
    {
        up = *n;
        if(t->cmp(p,(*n)->p) < 0)
            n = &((*n)->left);
        else
            n = &((*n)->right);
    }
    (*n) = calloc(sizeof(AvlNode),1);
    (*n)->p = p;
    (*n)->up = up;
    avltree_rebalance(t,(*n));
}

void avltree_clear(AvlTree *t)
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
    t->root = NULL;
}

#define TEST(COND) if(!(COND)) {printf(#COND ": failed\n"); return -1;}
int avltree_test()
{
    AvlTree t = {0};
    AvlNode *n;

    printf("testing avltree...");
    t.cmp = strcmp;
    avltree_insert(&t,"5");
    avltree_insert(&t,"3");
    avltree_insert(&t,"2");

    TEST(t.root);
    n = t.root;
    TEST(0==strcmp(n->p,"3"));
    TEST(0==strcmp(n->left->p,"2"));
    TEST(0==strcmp(n->right->p,"5"));
    TEST(!n->left->left   && !n->left->right);
    TEST(!n->right->left  && !n->right->right);
    TEST(n->left->up == n && n->right->up == n);

    avltree_clear(&t);
    TEST(!t.root);
    avltree_insert(&t,"3");
    avltree_insert(&t,"5");
    avltree_insert(&t,"7");
    n = t.root;
    TEST(0==strcmp(n->p,"5"));
    TEST(0==strcmp(n->left->p,"3"));
    TEST(0==strcmp(n->right->p,"7"));
    TEST(!n->left->left   && !n->left->right);
    TEST(!n->right->left  && !n->right->right);
    TEST(n->left->up == n && n->right->up == n);

    avltree_clear(&t);
    avltree_insert(&t,"5");
    avltree_insert(&t,"3");
    avltree_insert(&t,"4");
    n = t.root;
    TEST(0==strcmp(n->p,"4"));
    TEST(0==strcmp(n->left->p,"3"));
    TEST(0==strcmp(n->right->p,"5"));
    TEST(!n->left->left   && !n->left->right);
    TEST(!n->right->left  && !n->right->right);
    TEST(n->left->up == n && n->right->up == n);

    avltree_clear(&t);
    avltree_insert(&t,"3");
    avltree_insert(&t,"5");
    avltree_insert(&t,"4");
    n = t.root;
    TEST(0==strcmp(n->p,"4"));
    TEST(0==strcmp(n->left->p,"3"));
    TEST(0==strcmp(n->right->p,"5"));
    TEST(!n->left->left   && !n->left->right);
    TEST(!n->right->left  && !n->right->right);
    TEST(n->left->up == n && n->right->up == n);

    avltree_clear(&t);
    printf("done\n");
    return 0;
}
