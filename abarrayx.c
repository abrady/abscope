/***************************************************************************
 *     Copyright (c) 2009-2009, Aaron Brady
 *     All Rights Reserved
 *
 * Module Description:
 * crazy macro array definition.
 *
 ***************************************************************************/
#include "abutil.h"
#include "abarrayx.h"


#ifndef TYPE_T
#error "TYPE_T not defined"
#endif

#ifndef TYPE_FUNC_PREFIX
#error "TYPE_FUNC_PREFIX not defined"
#endif

#ifndef ABARRAYHDR
#define ABARRAYHDR
typedef struct AbArrayHdr
{
    int size;
    int cap;
    U32 code;
	U32 flags;
} AbArrayHdr;
STATIC_ASSERT(!(sizeof(AbArrayHdr)%sizeof(void*))); // alignment
#endif

#define ABARRAY_MEMSIZE(CAP) ((CAP)*(sizeof(TYPE_T))+sizeof(AbArrayHdr))
#define ABARRAY_FROM_T(P) (P?((AbArrayHdr*)(((U8*)(P))- sizeof(AbArrayHdr))):0)
#define T_FROM_ABARRAY(P) (P?((TYPE_T*)(((U8*)(P)) + sizeof(AbArrayHdr))):0)
#define ARRAY_MIN_CAPACITY(TYPE_SIZE) ((64 - sizeof(AbArrayHdr) + (TYPE_SIZE) - 1)/(TYPE_SIZE))


#if DEBUG_ARRAY_HANDLES
static HashPtr *S_ADECL(abarray_tracker);
static void S_ADECL(check_handle)(void *P)
{
    if(P && !hash_find(&S_ADECL(abarray_tracker),P))
        assertm(0,"unknown handle %p passed to Array",P);
}

static void S_ADECL(track_handle)(void *P)
{
    static int ntracks = 0;
    ntracks++;
    if(!P || !HashPtr_Insert(&S_ADECL(abarray_tracker),P,P,FALSE))
        assertm(0,"already tracking handle %p",P);
    ARRAY_LOG("track(%i) %p\n",ntracks,P);
}

static void S_ADECL(untrack_handle)(void *P)
{
    static int untracks;
    untracks++;
    if(P && !HashPtr_Remove(&S_ADECL(abarray_tracker),P))
        assertm(0,"not tracking handle %p",P);
    ARRAY_LOG("untrack(%i) %p",untracks,P);
}

    
#       define ABARRAY_TRACK_HANDLE(P) S_ADECL(track_handle)(P);
#       define ABARRAY_UNTRACK_HANDLE(P) S_ADECL(untrack_handle)(P)
#       define ABARRAY_CHECK_HANDLE(P) S_ADECL(check_handle)(P)
#else
#       define ABARRAY_TRACK_HANDLE(P)
#       define ABARRAY_UNTRACK_HANDLE(P)
#       define ABARRAY_CHECK_HANDLE(P)
#endif

static AbArrayHdr* S_ADECL(fill)(void *mem, int memsize, U32 flags)
{
	AbArrayHdr *buf = (AbArrayHdr*)mem;
	if(!buf)
		return NULL;
    
	ABARRAY_TRACK_HANDLE(buf);
	buf->size = 0;
	buf->cap = (memsize - sizeof(AbArrayHdr))/sizeof(TYPE_T);
#define TOSTR2(X) #X
#define TOSTR(X) TOSTR2(X)
	buf->code = hexify_str(TOSTR(TYPE_FUNC_PREFIX));
#undef TOSTR
#undef TOSTR2
	buf->flags = flags;
	return buf;
}

// for resizing the AbArrayHdr. never shrinks the array
static AbArrayHdr* S_ADECL(re_capacity)(AbArrayHdr *a, int cap) 
{
    if(cap < ARRAY_MIN_CAPACITY(sizeof(TYPE_T)))
       cap = ARRAY_MIN_CAPACITY(sizeof(TYPE_T));

    if(!a)
        return S_ADECL(fill)(calloc(ABARRAY_MEMSIZE(cap),1), ABARRAY_MEMSIZE(cap), 0);

    ABARRAY_CHECK_HANDLE(a);
    if(a->cap < cap)
    {
		AbArrayHdr *res;
        res = (AbArrayHdr*)realloc(a,ABARRAY_MEMSIZE(cap));
		if(a != res)
		{
			ABARRAY_UNTRACK_HANDLE(a);
			ABARRAY_TRACK_HANDLE(res);
		}
        if(res)
            res->cap = cap;
        return res;
    }
    return a;
}
    
    
AbArrayHdr *S_ADECL(setsize)(TYPE_T** ha, int size)
{
    AbArrayHdr *res = NULL;
    assert(ha); 
    if(!ha)
        return NULL;

    if(*ha)
        res = ABARRAY_FROM_T(*ha);
    else
        res = S_ADECL(re_capacity)(NULL,size*2);

    if(res->cap < size)
        res = S_ADECL(re_capacity)(res,size*2);

    if(res->size < size)
    {
        TYPE_T *elts = T_FROM_ABARRAY(res);
        elts += res->size;
        ZeroStructs(elts,size-res->size);
    }
    res->size = size;
    *ha = T_FROM_ABARRAY(res);
    // todo: some fill values here
    return res;
}

// public one. doesn't return AbArrayHdr
void ADECL(setsize)(TYPE_T** ha, int size)
{
    S_ADECL(setsize)(ha,size);
}


TYPE_T* ADECL(create)(int capacity) 
{
    return T_FROM_ABARRAY(S_ADECL(re_capacity)(NULL,capacity));
}

TYPE_T* ADECL(init)(void *mem, size_t mem_size, U32 flags)
{
	if(mem)
	{
		int cap = (mem_size - sizeof(AbArrayHdr))/sizeof(TYPE_T);
		AbArrayHdr *buf = S_ADECL(fill)(mem, cap, flags);
		assert(mem_size >= sizeof(AbArrayHdr)); // we just overflowed mem
		if(buf)
			return T_FROM_ABARRAY(buf);
	}
	return NULL;
}

void ADECL(destroy)(TYPE_T** ha, ADECL(destroyelt_fp) *fp) 
{
    AbArrayHdr *a;
    assert(ha);
    if(!ha)
        return;

    a = ABARRAY_FROM_T(*ha);
    ABARRAY_CHECK_HANDLE(a);
    ABARRAY_UNTRACK_HANDLE(a);
	if(!a)
		return;
    if(fp)
    {
        int i;
        for( i = 0; i < a->size; ++i ) 
            fp((*ha)+i);
    }
    free(a);
    *ha = NULL;
}

int ADECL(size)(TYPE_T const * const * ha) 
{
    AbArrayHdr *a;
    assert(ha);
    if(!ha)
        return 0;
    
    a = ABARRAY_FROM_T(*ha);        
    ABARRAY_CHECK_HANDLE(a);
    return DEREF(a,size);
}

TYPE_T* ADECL(push)(TYPE_T **ha) 
{
	return ADECL(pushn)(ha, 1);
}

TYPE_T* ADECL(pushn)(TYPE_T **ha, int n) 
{
	AbArrayHdr *a;
    TYPE_T *res;
	assert(ha);
    if(!ha)
        return NULL;
    
	a = ABARRAY_FROM_T(*ha);
	ABARRAY_CHECK_HANDLE(a);
    if(n <= 0)
        return NULL;
	a = S_ADECL(setsize)(ha, DEREF(a,size)+n);
    res = &(*ha)[a->size - n];
    ZeroStructs(res,n);
	return res;
}

TYPE_T* ADECL(pushfront)(TYPE_T **ha)
{
	AbArrayHdr *a;
    assert(ha);
    if(!ha)
        return NULL;
    ADECL(push)(ha);
	a = ABARRAY_FROM_T(*ha);
    assert(a);
    if(!a)
        return NULL;    
    memmove(*ha + 1, *ha,(a->size-1)*sizeof(TYPE_T));
    return *ha;
}


int ADECL(push_by_cp)(TYPE_T **ha, TYPE_T b) 
{
    TYPE_T *a;
    a = ADECL(push)(ha);
    if(!a)
        return -1;
    *a = b;
    return (int)(a - *ha);
}
    
TYPE_T* ADECL(pop)(TYPE_T **ha) 
{
    AbArrayHdr *a;
    assert(ha);
    if(!ha)
        return NULL;

    a = ABARRAY_FROM_T(*ha);
    ABARRAY_CHECK_HANDLE(a);

    if(DEREF(a,size) <= 0)
        return NULL;
    return &(*ha)[--a->size];
}

TYPE_T* ADECL(top)(TYPE_T **ha) 
{
    AbArrayHdr *a;
    assert(ha);
    if(!ha)
        return NULL;
    a = ABARRAY_FROM_T(*ha);
    assert(a);
    if(!a)
        return NULL;
    ABARRAY_CHECK_HANDLE(a);
        
    if(DEREF(a,size) <= 0)
        return NULL;
    return &(*ha)[a->size-1];
}

void ADECL(cp_raw)(TYPE_T **hdest,TYPE_T const *s,int n) 
{
    AbArrayHdr *d;
    assert(hdest && s);
    if(!hdest || !s)
        return;
    d = ABARRAY_FROM_T(*hdest);
    ABARRAY_CHECK_HANDLE(d);
    if(n < 0)
        return;
    d = S_ADECL(setsize)(hdest ,n);
    memmove(*hdest,s,n*sizeof(TYPE_T));
}

void ADECL(cp)(TYPE_T **hdest,TYPE_T const * const *hsrc,int n) 
{
    AbArrayHdr *s;
    assert(hdest && hsrc);
    if(!hdest || !hsrc)
       return;
    s = ABARRAY_FROM_T(*hsrc);
    assert(s);
    if(!s)
        return;
    if(n == 0)
        n = s->size;
    ADECL(cp_raw)(hdest,*hsrc,n);
}

void ADECL(insert)(TYPE_T **hdest, TYPE_T *src, int i, int n)
{
    AbArrayHdr *d = ABARRAY_FROM_T(*hdest);
    TYPE_T *offset;
    int new_size;
    int remain;
    ABARRAY_CHECK_HANDLE(d);
    if(n <= 0 || i < 0 || i > DEREF(d,size))
        return;
    new_size = n + DEREF(d,size);
    remain = DEREF(d,size) - i;
    d = S_ADECL(setsize)(hdest,new_size);
    offset = (*hdest)+i;
    memmove(offset+n,offset,remain*sizeof(TYPE_T));
    memmove(offset,src,n*sizeof(TYPE_T));
}

void ADECL(append)(TYPE_T **hdest, TYPE_T *src, int n) 
{
    AbArrayHdr *d = ABARRAY_FROM_T(*hdest);
    ADECL(insert)(hdest,src,DEREF(d,size),n);
}
 
void ADECL(rm)(TYPE_T **ha, int offset, int n)
{
    AbArrayHdr *a;
    assert(ha);
    if(!ha)
        return;
    a = ABARRAY_FROM_T(*ha);        
    ABARRAY_CHECK_HANDLE(a);

    if(offset < 0 || DEREF(a,size) <= offset || n < 0)
        return;
    n = MIN(n,a->size-offset);
    if(n)
    {
        a->size -= n;
        memmove((*ha)+offset,(*ha)+offset+n,(a->size-offset)*sizeof(TYPE_T));
        ZeroStructs((*ha)+a->size,n);
    }        
}

// find using comparator, or memcmp
int ADECL(find)(TYPE_T **ha, TYPE_T *b, ADECL(cmp_fp) *cmp, void *ctxt)
{
    int i;
    for( i = 0; i < ADECL(size)(ha); ++i)
    {
        TYPE_T *a = (*ha)+i;
        if(cmp)
        {
            if(0 == cmp(a,b,ctxt))
            return i;
        }
        else if(0 == memcmp(a,b,sizeof(*a)))
            return i;
    }
    return -1;
}

void ADECL(foreach_ctxt)(TYPE_T **ha, ADECL(foreach_ctxt_fp) *fp, void *ctxt)
{
    int i;
    if(!fp)
        return;
    for( i = 0; i < ADECL(size)(ha); ++i)
        fp(&(*ha)[i],ctxt);
}

void ADECL(foreach)(TYPE_T **ha, ADECL(foreach_fp) *fp)
{
    int i;
    if(!fp)
        return;
    for( i = 0; i < ADECL(size)(ha); ++i)
        fp(&(*ha)[i]);
}

#ifdef ABARRAY_SERIALIZE
int ADECL(binwrite)(File *fp, TYPE_T **ha)
{
	AbArrayHdr *a;
    assert(ha);
    if(!ha)
        return 0;
    a = ABARRAY_FROM_T(*ha);        
	if(!a)
		return 0;
    ABARRAY_CHECK_HANDLE(a);
	return mem_binwrite(fp,a,sizeof(*a)+sizeof(TYPE_T)*a->size,1);
}
#endif

#ifdef ABARRAY_SERIALIZE
int ADECL(binread)(File *fp, TYPE_T **ha)
{
	AbArrayHdr *a = 0;
    assert(ha);
    if(!ha)
        return 0;
	if(!mem_binread(fp,&a,NULL,1))
		return 0;
    ABARRAY_CHECK_HANDLE(a);
	*ha = T_FROM_ABARRAY(a);
	return 1;
}
#endif


#undef ABARRAY_TRACK_HANDLE
#undef ABARRAY_UNTRACK_HANDLE
#undef ABARRAY_CHECK_HANDLE

#undef ABARRAY_MEMSIZE
#undef ABARRAY_FROM_T
#undef T_FROM_ABARRAY
#undef ARRAY_MIN_CAPACITY

#undef TYPE_T 
#undef TYPE_FUNC_PREFIX 

