/***************************************************************************
 *     Copyright (c) 2009-2009, Aaron Brady
 *     All Rights Reserved
 *
 * Module Description:
 *
 *
 ***************************************************************************/
#include "abscope.h"
#include "abarray.h"
#include "strs.h"
#include "abserialize.h"

StrPool g_strpool;

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

void strs_cleanup(char **strs, int n_strs)
{
    int i;
    for(i = 0; i < n_strs; ++i)
    {
        free(strs[i]);
        strs[i] = 0;
    }
}

// *************************************************************************
//  strpool
// *************************************************************************


typedef struct StrBlock
{
	char *block;
	int n;
} StrBlock;

char *strpool_find(StrPool *pool, char *s)
{
    if(!pool)
        return NULL;
    return (char*)hash_find(pool->dict,s);
}

char *strpool_add(StrPool *pool, char *s)
{
    char *p;
    if(!pool || !s)
        return NULL;
	if(!pool->dict)
		pool->dict = STRUCT_ALLOC(*pool->dict);
    p = strpool_find(pool,s);
    if(p)
        return p;
	p = strdup(s);
    hash_insert(pool->dict,p,p);
    return p;
}

static int str_in_block(StrPool *p, char *s)
{
	int i;
	for(i = 0; i < ap_size(&p->strblocks); ++i) 
	{
		StrBlock *sb = p->strblocks[i];
		if(INRANGE(s,sb->block,sb->block+sb->n))
			return 1;
	}
	return 0;
}


void strpool_cleanup(StrPool *p)
{
	int i;

	if(!p)
		return;

	for(i = 0; i < DEREF(p->dict,n_elts); ++i)
	{
		HashNode *n = p->dict->elts + i;
		if(!n->key)
			continue;
		if(!str_in_block(p,n->key))
			continue;
		free(n->key);
	}

	for(i = 0; i < ap_size(&p->strblocks); ++i)
		free(p->strblocks[i]);
	ap_destroy(&p->strblocks,NULL);
	free_safe(p->dict);
	p->dict = 0;
}

int strpool_write(char *fn, StrPool *pool)
{
	int n;
	int i;
	File *fp = absfile_open_write(fn);

    if(!DEREF(pool,dict) || !fn)
        return -1;
	
	// write the hashtable
	if(!int_binwrite(fp,pool->dict->n_used)
	   || !array_binwrite(fp,pool->dict->elts,pool->dict->n_elts))
		return -1;

	n = 0;
	for(i = 0; i < pool->dict->n_elts; ++i)
	{
		HashNode *node = pool->dict->elts + i;
		if(!node->key)
			continue;
		n += strlen(node->key) + 1;
	}
	
	if(!int_binwrite(fp,n))
		return -1;
	
	for(i = 0; i < pool->dict->n_elts; ++i)
	{
		HashNode *node = pool->dict->elts + i;
		int len;

		if(!node->key)
			continue;
		len = strlen(node->key);
		abfwrite(node->key,len+1,1,fp);                 // str data
	}

	abfclose(fp);

	return !(i == pool->dict->n_elts);
}

int strpool_read(char *fn, StrPool *pool, SerializeCtxt *ctxt)
{
	StrBlock *sb;
	char *s;
	char *strblock = 0;
	int n;
	int i;
	File *fp = absfile_open_read(fn);

    if(!pool || !fp)
        return -1;

	assert(!pool->dict);
	pool->dict = STRUCT_ALLOC(*pool->dict);

	if(!int_binread(fp,&pool->dict->n_used)
	   || !array_binread(fp,&pool->dict->elts,&pool->dict->n_elts)) // read the hashtable
		return -1;

	if(!array_binread(fp,&strblock,&n)) // blob of string data
		return -1;
	
	sb = calloc(sizeof(*sb),1);
	sb->block = strblock;
	sb->n = n;
	ap_push(&pool->strblocks,sb);

	// fixup
	s = strblock;
	for(i = 0; i < pool->dict->n_elts; ++i)
	{
		HashNode *node = pool->dict->elts + i;
		if(!node->key)
			continue;
		serialize_fixup_add_ptr(ctxt,node->key,s);
		node->key = s;
		s += strlen(s)+1;
		assert(s <= strblock + n);
	}

	return 0;
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
    TEST(!strpool_find(&pool,"abc"));
    p=strpool_add(&pool,"abc");
    TEST(p);
    TEST(0==strcmp("abc",strpool_add(&pool,"abc")));
    TEST(p == strpool_add(&pool,"abc"));
    
    for(i = 0; i < 100; ++i)
    {
        int j;
        char tmp[128];
        sprintf(tmp,"%i",i);
        strpool_add(&pool,tmp);
        for(j = 0; j <= i; ++j)
        {
            sprintf(tmp,"%i",j);
            TEST(strpool_find(&pool,tmp));
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
