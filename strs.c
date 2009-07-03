/***************************************************************************
 *     Copyright (c) 2009-2009, Aaron Brady
 *     All Rights Reserved
 *
 * Module Description:
 *
 *
 ***************************************************************************/
#include "abscope.h"
#include "strs.h"
#include "abserialize.h"

char *strs_find_str(char **strs, int n_strs, char *s)
{
    int i;
    for(i = 0; i < n_strs; ++i)
        if(0==strcmp(s,strs[i]))
            return strs[i];
    return NULL;
}

char *strs_add_str(char ***pstrs, int *n_strs, char *s)
{
    int n = ++*n_strs;
    *pstrs = realloc(*pstrs,sizeof(*pstrs)*n);
    (*pstrs)[n-1] = s;
    return (*pstrs)[n-1];
}

char *strs_find_add_str(char ***pstrs, int *n_strs, char *s)
{
    char *r;
    if(NULL != (r=strs_find_str(*pstrs,*n_strs,s)))
        return r;
    return strs_add_str(pstrs,n_strs,s);
}

char *strpool_find_str(StrPool *pool, char *s)
{
    if(!pool)
        return NULL;
    return avltree_find(&pool->tree,s);
}

char *strpool_find_add_str(StrPool *pool, char *s)
{
    char *p;
    if(!pool || !s)
        return NULL;
    p = strpool_find_str(pool,s);
    if(p)
        return p;
    return strpool_add_str(pool,s);
}

char *strpool_add_str(StrPool *pool, char *s_in)
{
    char *s = s_in;
    if(!pool || !s)
        return NULL;
    s = _strdup(s);
    strs_add_str(&pool->strs,&pool->n_strs,s);
    avltree_insert(&pool->tree,s);    
    return s;
}

void strpool_cleanup(StrPool *p)
{
    avltree_cleanup(&p->tree);
    free(p->strs);
}

#define TEST(COND) if(!(COND)) {printf("%s(%d):"#COND ": failed\n",__FILE__,__LINE__); break_if_debugging(); return -1;}

int test_strpool(void)
{
    StrPool pool = {0};
    char *p;
    int i;

    printf("testing strpool...");
    TEST(!strpool_find_str(&pool,"abc"));
    p=strpool_find_add_str(&pool,"abc");
    TEST(p);
    TEST(0==strcmp("abc",strpool_find_add_str(&pool,"abc")));
    TEST(p == strpool_find_add_str(&pool,"abc"));
    
    for(i = 0; i < 100; ++i)
    {
        int j;
        char tmp[128];
        sprintf(tmp,"%i",i);
        strpool_find_add_str(&pool,tmp);
        for(j = 0; j <= i; ++j)
        {
            sprintf(tmp,"%i",j);
            TEST(strpool_find_str(&pool,tmp));
        }
    }
    strpool_cleanup(&pool);
    printf("done.\n");
    return 0;
}

void strpool_add_strblock(StrPool *pool, char *strblock, int n)
{
    char *end = strblock + n;
    char *s;
    if(!pool || !strblock)
        return;
    s = strblock;
    while(s < end)
    {
        strpool_add_str(pool,s);
        s += strlen(s)+1;
    }
}

// char *strs_pack(char **strs, int n_strs, int *n_bytes_res)
// {
//     int strs_len;
//     int i;
//     char *res;
//     char *s;
//     strs_len = 0;
//     for(i = 0;i < n_strs; ++i)
//         strs_len += strlen(p->pool.strs[i]);    
//     res = malloc(strs_len);
//     for(i = 0;i < n_strs; ++i)
//     {
//         strcpy(s,strs[i]);
//         s += strlen(strs[i])+1;
//     }
//     return res;
// }

void strs_cleanup(char **strs, int n_strs)
{
    int i;
    for(i = 0; i < n_strs; ++i)
    {
        free(strs[i]);
        strs[i] = 0;
    }
}

static int strpool_tree_traverser_cb(AvlNode *n, void *ctxt)
{
    FILE *fp = (FILE*)ctxt;
    return string_binwrite(fp,n->p);
}

int strpool_binwrite(FILE *fp, StrPool *pool)
{
    if(!pool)
        return 0;
    return avltree_traverse(&pool->tree,&strpool_tree_traverser_cb,fp);
}

char *strblock_from_strpool(int *res_block_len, StrPool *pool)
{
    int i;
    int n_res = 0;
    int n;
    char *res;
    char *s;
    for(i = 0; i < pool->n_strs; ++i)
        n_res += (strlen(pool->strs[i]) + 1);
    res = malloc(n);
    s = res;
    for(i = 0; i < pool->n_strs; ++i)
    {
        n = strlen(pool->strs[i])+1;
        memmove(s,pool->strs[i],n);
        s += n;
    }
    if(res_block_len)
        *res_block_len = n_res;
    return res;
}
