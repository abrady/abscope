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

#define AVL_NODE_HEIGHT(n) (MAX(DEREF(n->left,height),DEREF(n->right,height)) + 1)

static void avltree_rotfixup(AvlTree *t, AvlNode *n, AvlNode *r)
{
    AvlNode *tmp;
    if(n == t->root)
        t->root = r;
    else if(n->up->left == n)
        n->up->left = r;
    else if(n->up->right == n)
        n->up->right = r;

    tmp = DEREF(n->up,up);
    n->up = r;
    r->up = tmp;

    n->height = AVL_NODE_HEIGHT(n);
    r->height = AVL_NODE_HEIGHT(r);
}


static void avltree_rotleft(AvlTree *t, AvlNode *n)
{
    AvlNode *r;
    AvlNode *tmp;
    r = n->right;
    if(!r) 
        return;
    tmp = r->left;
    r->left = n;
    n->right = tmp;
    avltree_rotfixup(t,n,r);
}

static void avltree_rotright(AvlTree *t, AvlNode *n)
{
    AvlNode *r;
    AvlNode *tmp;
    r = n->left;
    if(!r)
        return;
    tmp = r->right;
    r->right = n;
    n->left = tmp;
    avltree_rotfixup(t,n,r);
}

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

    for(n = n_in; n; n = n->up)
    {
        n_l = n->left  ? n->left->height  : 0;
        n_r = n->right ? n->right->height : 0;
        n->height = (n_l > n_r ? n_l : n_r) + 1;

        if((n_l - n_r) > 1)
        {
            // left heavy. move left branch up.
            if(DEREF2(n->left,right,height) > DEREF2(n->left,left,height))
                avltree_rotleft(t,n->left);
            avltree_rotright(t,n);
        }
        else if((n_r - n_l) > 1)
        {
            // right heavy: right becomes parent.
            if(DEREF2(n->right,left,height) > DEREF2(n->right,right,height))
                avltree_rotright(t,n->right);
            avltree_rotleft(t,n);
        }
    }
}

static void avltree_init(AvlTree *t)
{
    if(!t->cmp)
        t->cmp = strcmp;
}


void avltree_insert(AvlTree *t, void *p)
{
    AvlNode **n = &t->root;
    AvlNode *up = NULL;
    
    if(!t || !p)
        return;
    avltree_init(t);
    
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
    t->root = NULL;
}

char *avltree_find(AvlTree *t, char *p)
{
    AvlNode *n = avltree_findnode(t,p);
    if(n)
        return n->p;
    return NULL;
}

AvlNode *avltree_findnode(AvlTree *t, char *p)
{
    AvlNode *n;
    int c;
    if(!t || !p)
        return NULL;
    avltree_init(t);
    for(n = t->root;n;)
    {
        c = t->cmp(p,n->p);
        if(c == 0)
            return n;
        else if(c < 0)
            n = n->left;
        else
            n = n->right;
    }
    return n;
}


static int avlnode_traverse(AvlNode *n,AvlTreeTraverser fp, void *ctxt)
{
    int res = 0;
    if(n->left)
        res = avlnode_traverse(n->left,fp,ctxt);
    if(0 == res && n->right)
        res = avlnode_traverse(n->right,fp,ctxt);
    if(0 != res)
        return res;
    return fp(n,ctxt);
}
int avltree_traverse(AvlTree *tree, AvlTreeTraverser fp, void *ctxt)
{
    if(!tree || !tree->root || !fp)
        return 0;
    return avlnode_traverse(tree->root,fp,ctxt);    
}

#define TEST(COND) if(!(COND)) {printf(#COND ": failed\n"); break_if_debugging(); return -1;}
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

    avltree_cleanup(&t);
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

    avltree_cleanup(&t);
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

    avltree_cleanup(&t);
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

    avltree_cleanup(&t);
    avltree_insert(&t,"abc");
    avltree_insert(&t,"0");
    avltree_insert(&t,"1");
    avltree_insert(&t,"2");
    avltree_insert(&t,"3");
    TEST(avltree_find(&t,"abc"));
    TEST(avltree_find(&t,"0"));
    TEST(avltree_find(&t,"1"));
    TEST(avltree_find(&t,"2"));
    TEST(avltree_find(&t,"3"));

    avltree_cleanup(&t);
    printf("done\n");
    return 0;
}

