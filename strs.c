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


int32_t strs_find_str(char **strs, int n_strs, char *s)
{
    int i;
    for(i = 0; i < n_strs; ++i)
        if(0==strcmp(s,strs[i]))
            return i;
    return -1;
}

int32_t strs_add_str(char ***pstrs, int *n_strs, char *s)
{
    int n = ++*n_strs;
    *pstrs = realloc(*pstrs,sizeof(*pstrs)*n);
    (*pstrs)[n-1] = s;
    return n-1;
}

int32_t strs_find_add_str(char ***pstrs, int *n_strs, char *s)
{
    int r;
    if((r=strs_find_str(*pstrs,*n_strs,s))>=0)
        return r;
    return strs_add_str(pstrs,n_strs,s);
}
