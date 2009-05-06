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
    char *p;
    if(!pool || !s)
        return NULL;
    p = pool->strs;
    while(p != pool->end)
    {
        if(0 == strcmp(p,s))
            return p;
        p += strlen(p)+1;
    }
    return NULL;
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

char *strpool_add_str(StrPool *pool, char *s)
{
    int n;
    int n_s;
    int n_pre;
    char *r;
    n_s = strlen(s) + 1;
    n_pre = pool->end - pool->strs;
    n = n_pre + n_s;
    pool->strs = realloc(pool->strs,n);
    pool->end = pool->strs + n_pre;
    strcpy(pool->end,s);
    r = pool->end;
    pool->end += n_s;
    return r;
}

void strpool_cleanup(StrPool *p)
{
    free(p->strs);
}
