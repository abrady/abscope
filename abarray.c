/***************************************************************************
 *     Copyright (c) 2009-2009, Aaron Brady
 *     All Rights Reserved
 *
 * Module Description:
 *
 *
 ***************************************************************************/
#include "abutil.h"

// *************************************************************************
// standard containers
// *************************************************************************

#define TYPE_T void*
#define TYPE_FUNC_PREFIX ap
#include "abarrayx.c"
#undef TYPE_T
#undef TYPE_FUNC_PREFIX

#define TYPE_T char*
#define TYPE_FUNC_PREFIX astr
#include "abarrayx.c"
#undef TYPE_T
#undef TYPE_FUNC_PREFIX

#define TYPE_T int
#define TYPE_FUNC_PREFIX aint
#include "abarrayx.c"
#undef TYPE_T
#undef TYPE_FUNC_PREFIX


// *************************************************************************
//  test suite
// *************************************************************************

typedef struct Foo
{
    char b[5];
    int a;
} Foo;

static int foo_acc;
static int foo_acc;
void Foo_Destroy(Foo *helt)
{
    foo_acc += helt->a;
}

int foo_cmp(Foo *a, Foo *b, void *ctxt)
{
    intptr_t i = (intptr_t)ctxt;
    if(!i)
        return strcmp(a->b,b->b);
    return a->a - b->a;
}

void foo_foreach_ctxt_func(Foo *a, void *ctxt)
{
    int *pi = (int*)ctxt;
    *pi += a->a;
}

void foo_foreach_func(Foo *a)
{
    foo_acc += a->a;
}


#define TYPE_T Foo
#define TYPE_FUNC_PREFIX foo
#include "abarrayx.h"
#include "abarrayx.c"
#undef TYPE_T
#undef TYPE_FUNC_PREFIX

#define TEST(COND) if(!(COND)) {printf("%s(%d):"#COND ": failed\n",__FILE__,__LINE__); break_if_debugging(); return -1;}

int abarray_test()
{
    int i;
    int n_tests = 100;
    Foo *ap = NULL;
    Foo *acp = NULL;

    printf("ncarray_unittest...");

    foo_setsize(&ap,0);
    for( i = 0; i < n_tests; ++i )
    {
        Foo *fp = foo_push(&ap);
        sprintf_s(SSTR(fp->b),"%i",i);
        fp->a = i;
        TEST(fp - ap == i);
    }
    TEST(foo_size(&ap) == i);
    foo_setsize(&ap,0);  // force re-use of allocated memory
    {
        Foo *fp = foo_pushn(&ap,n_tests);
        Foo tst = {0};            
        for( i = 0; i < n_tests; ++i )
        {
            TEST(0 == memcmp(fp,&tst,sizeof(tst)));
            sprintf_s(SSTR(fp->b),"%i",i);
            fp->a = i;
            fp++;
        }
    }        
    TEST(foo_size(&ap) == n_tests);
    foo_setsize(&ap,n_tests);
    
    // copy
    foo_cp(&acp,&ap,0);
    TEST(foo_size(&acp) == i);
    for( i = 0; i < n_tests; ++i )
        TEST(0==memcmp(&acp[i],&ap[i],sizeof(Foo)));
    
    TEST(foo_find(&acp,&acp[1],NULL,NULL) == 1);
    TEST(foo_find(&acp,&acp[1],&foo_cmp,(void*)0) == 1);
    TEST(foo_find(&acp,&acp[1],&foo_cmp,(void*)1) == 1);
    
    {
        int acc = 0;
        foo_foreach_ctxt(&acp,&foo_foreach_ctxt_func,&acc);
        TEST(acc == 99*50);
        foo_acc = 0;
        foo_foreach(&acp,&foo_foreach_func);
        TEST(foo_acc == 99*50);
    }
    
    // pop
    for( i--; i >= 0; --i)
    {
        Foo *ft = foo_top(&ap);
        Foo *f =  foo_pop(&ap);
        TEST(ft == f);
        TEST(f->a == i);
        TEST(atoi(f->b) == i);
    }
    TEST(foo_size(&ap) == 0);
    foo_destroy(&ap,Foo_Destroy);
    foo_acc = 0;
    foo_destroy(&acp,Foo_Destroy);
    TEST(foo_acc == 99*50);
    
    printf("done\n");
    return 0;
}

