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
    if(!*pstrs)
        return NULL;
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
    s = strdup(s);
    strs_add_str(&pool->strs,&pool->n_strs,s);
    avltree_insert(&pool->tree,s);    
    return s;
}

void strpool_cleanup(StrPool *p)
{
    avltree_cleanup(&p->tree,0);
    free(p->strs);
}


// a contiguous block of strs
void strpool_add_strblock(StrPool *pool, char *strblock, char *end)
{
    char *s;
    if(!pool || !strblock)
        return;
    s = strblock;
    while(s < end)
    {
        int n = strlen(s) + 1;
        if(!strpool_find_str(pool,s))
        {
            strs_add_str(&pool->strs,&pool->n_strs,s);
            avltree_insert(&pool->tree,s);
        }
        s += n;
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
    File *fp = (File*)ctxt;
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
    res = malloc(n_res);
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

int str_vsprintf(char **dst,char *fmt,va_list args)
{
    int c = _vscprintf(fmt,args) + 1;
    *dst = realloc(*dst,c);
	if(!*dst)
		return 0;
    return vsprintf(*dst,fmt,args);
}

int str_sprintf(char **dst, char *fmt, ...)
{
    int r;
    va_list vl;
    va_start(vl,fmt);
    r = str_vsprintf(dst,fmt,vl);
    va_end(vl);
    return r;
}

char* str_cat(char **dst, char *src)
{
    int m,n = 0;
    if(!src || !dst)
        return dst ? *dst : NULL;
    
    m = strlen(src) + 1;
    if(*dst)
        n = strlen(*dst);
    *dst = realloc(*dst,m+n);
    if(!n)
        *(*dst) = 0; // clear the string if we alloc'd it
    if(0==strcat_s(*dst,m+n,src))
        return *dst;
    return NULL;
}

char* str_catf(char **dst, char *fmt, ...)
{
    char *r;
    va_list vl;
    va_start(vl,fmt);
    r = str_vcatf(dst,fmt,vl);
    va_end(vl);
    return r;
}


char *str_vcatf(char **dst,char *fmt,va_list args)
{
    int c = _vscprintf(fmt,args) + 1;
    int n = 0;
    if(!dst)
        return NULL;
    if(*dst)
        n = strlen(*dst);
    
    *dst = realloc(*dst,n+c);
	if(!*dst)
		return NULL;
    vsprintf(*dst+n,fmt,args);
    return *dst;
}

#define TEST(COND) if(!(COND)) {printf("%s(%d):"#COND ": failed\n",__FILE__,__LINE__); break_if_debugging(); return -1;}

int strpool_test(void)
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

#define TEST_S(A,B) TEST(0==strcmp(A,B))
    p = NULL;
    str_cat(&p,"a");
    TEST_S(p,"a");
    str_cat(&p,"b");
    TEST_S(p,"ab");
    str_catf(&p,"%i%s%i",1,"foo",2);
    TEST_S(p,"ab1foo2");
    free(p);
    printf("done.\n");
    return 0;
}

U32 hexify_chars(char a, char b, char c, char d)
{
#if __BIG_ENDIAN
    return ((a<<24)|(b<<16)|(c<<8)|d);
#else
    return ((d<<24)|(c<<16)|(b<<8)|a);
#endif
}

U32 hexify_str(char *s) 
{
    char a = s?s[0]:0; 
    char b = s&&s[0]?s[1]:0; 
    char c = (s&&s[0]&&s[1])?s[2]:0; 
    char d = (s&&s[0]&&s[1]&&s[2])?s[3]:0; 
    return hexify_chars(a,b,c,d);
}
