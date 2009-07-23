/***************************************************************************
 *     Copyright (c) 2009-2009, Aaron Brady
 *     All Rights Reserved
 *
 * Module Description:
 * todo:
 * - add some measurement of performance problems (e.g. probe miss) 
 ***************************************************************************/
#include "abhash.h"

//-----------------------------------------------------------------------------
// MurmurHash2, by Austin Appleby

// Note - This code makes a few assumptions about how your machine behaves -

// 1. We can read a 4-byte value from any address without crashing
// 2. sizeof(int) == 4

// And it has a few limitations -

// 1. It will not work incrementally.
// 2. It will not produce the same results on little-endian and big-endian
//    machines.
static ABINLINE U32 MurmurHash2( char* key, U32 len, U32 seed )
{
	// 'm' and 'r' are mixing constants generated offline.
	// They're not really 'magic', they just happen to work well.

	const unsigned int m = 0x5bd1e995;
	const int r = 24;

	// Initialize the hash to a 'random' value

	unsigned int h = seed ^ len;

	// Mix 4 bytes at a time into the hash

	const unsigned char * data = (const unsigned char *)key;

	while(len >= 4)
	{
		unsigned int k = *(unsigned int *)data;

		k *= m; 
		k ^= k >> r; 
		k *= m; 
		
		h *= m; 
		h ^= k;

		data += 4;
		len -= 4;
	}
	
	// Handle the last few bytes of the input array

	switch(len)
	{
	case 3: h ^= data[2] << 16;
	case 2: h ^= data[1] << 8;
	case 1: h ^= data[0];
	        h *= m;
	};

	// Do a few final mixes of the hash to ensure the last few
	// bytes are well-incorporated.

	h ^= h >> 13;
	h *= m;
	h ^= h >> 15;

	return h;
}

U32 str_hashfunc(char *str, void *ctxt)
{
    ctxt;
    if(!str)
        return 0;
    return MurmurHash2(str,strlen(str),0);
}

#define VALID_HASHTABLE_CHECK(HT) if(!POW_OF_2(HT->n_elts)){    \
        fprintf(stderr,"invalid hashtable " __FUNCTION__ "\n"); \
        return 0;                                               \
    }

HashNode *hash_findnode(HashTable *ht, char *key)
{
    int i;
    U32 hash;
    int off;
    HashNode *n;
    
    if(!DEREF(ht,elts) || !key )
        return NULL;
    
    VALID_HASHTABLE_CHECK(ht);
    
    hash = ht->hashfp(key, ht->ctxt);
    if(!hash)
    {
        fprintf(stderr,"key didn't make a hash value\n");
        return NULL;
    }
    
    // linear probe
    for(i = 0; i<ht->n_elts; ++i)
    {
        off = ((hash + i) & (ht->n_elts - 1));
        n = ht->elts + off;
        if(n->hash == hash && 0==ht->cmpfp(key,n->key))
            return n;
        else if(!n->hash)
            return NULL;
    }
    return NULL;
}

void *hash_find(HashTable *ht, char *key)
{
    HashNode *n = hash_findnode(ht,key);
    if(n)
        return n->p;
    return NULL;
}

BOOL hash_exists(HashTable *ht, char *key)
{
    return hash_findnode(ht,key) != NULL;
}

BOOL hash_insert(HashTable *ht, char *key, void *p)
{
    U32 off;
    int i;
    U32 hash;
    HashNode *n;
    
    if(!ht || !key)
        return FALSE;
    
    VALID_HASHTABLE_CHECK(ht);
    
    if(!ht->elts)
    {
        ht->n_elts = 16;
        ht->elts = calloc(sizeof(*ht->elts),ht->n_elts);
        ht->n_used = 0;
    }
    
    if(!ht->cmpfp)
        ht->cmpfp = strcmp;
    if(!ht->hashfp)
        ht->hashfp = str_hashfunc;
    
    // check for need to rehash
#define PT75(N) (((N)>>2) + ((N)>>1)) // 75% = 1/2 + 1/4
    if(ht->n_used > PT75(ht->n_elts))
    {
        HashNode *new_elts;
        int n_new = ht->n_elts;
        n_new *= 2;
        
        // do this first to give realloc a chance to keep this in the
        // same spot
        new_elts = calloc(sizeof(*ht->elts),n_new);
        for(i = 0; i<ht->n_elts; ++i)
        {
            HashNode *n_old = ht->elts + i;
            int j;
            
            if(!n_old->hash)
                continue;
            for(j = 0; j < n_new; ++j)
            {
                off = ((n_old->hash + j) & (n_new - 1));
                n = new_elts + off;
                if(!n->hash)
                {
                    *n = *n_old;
                    break;
                }
            }
            if(j == n_new)
                abassert(0 && "failed to re-hash");
        }
        free(ht->elts);
        ht->elts = new_elts;
        ht->n_elts = n_new;
    }
#undef PT75
    
    hash = ht->hashfp(key, ht->ctxt);
    if(!hash)
    {
        fprintf(stderr,"key didn't make a hash value\n");
        return FALSE;
    }
    
    // linear probe
    for(i = 0; i<ht->n_elts; ++i)
    {
        off = ((hash + i) & (ht->n_elts - 1));
        n = ht->elts + off;
        
        if(!n->hash)
        {
            n->hash = hash;
            n->p = p;
            n->key = key;
            ht->n_used++;
            return TRUE;
        }
        if(n->hash == hash && 0==ht->cmpfp(key,n->key))
            return FALSE; // match existing
    }
    abassert(0&&"shouldn't get here");
    return FALSE;
}

void hash_cleanup(HashTable *ht, HashCleanupFp *cb)
{
    if(!ht)
        return;
    free(ht->elts);
    if(cb)
    {
        int i;
        for(i = 0; i<ht->n_elts; ++i)
        {
            HashNode *n = ht->elts + i;
            if(!n->hash)
                continue;
            cb(n,ht->ctxt);
        }
    }
}


#define TEST(COND) if(!(COND)) {fprintf(stderr,#COND ": failed\n"); break_if_debugging(); return -1;}

int hash_test()
{
    int i;
    HashTable hash_table = {0};
    HashTable *ht = &hash_table;
    TEST(!hash_exists(ht,"foo"));
    TEST(hash_insert(ht,"foo",ht));
    TEST(!hash_insert(ht,"foo",ht)); // no dupes
    TEST(hash_exists(ht,"foo"));
    TEST(ht == hash_find(ht,"foo"));
    
    for(i = 0; i< 100; ++i)
    {
        char tmp[16];
        char *t;
        sprintf(tmp,"%.3i",i);
        t = _strdup(tmp);
        TEST(hash_insert(ht,t,t));
        TEST(!hash_insert(ht,t,t));
        TEST(hash_exists(ht,t));
    }

    for(i = 0; i< 100; ++i)
    {
        char tmp[16];
        char *t;
        sprintf(tmp,"%.3i",i);
        t = hash_find(ht,tmp);
        TEST(t);
        TEST(0==strcmp(t,tmp));
        free(t);
    }
    return 0;
}
