/***************************************************************************
 *     Copyright (c) 2009-2009, Aaron Brady
 *     All Rights Reserved
 *
 * Module Description:
 * writes and reads only write in-struct and substruct data directly owned by it
 * it is assumed that external structs are serialized on their own and will exist
 * in memory after all loads are done, and will have registered their new address and
 * old address in the SerializeCtxt.
 * 
 * When an object is serialized it should:
 * - record all of the information it 'owns' (structs, substructs, etc.)
 * - record pointers to external addresses for fixup
 * - record addresses in memory that are used by unowned data to refer to the object.
 * 
 * When an object is deserialized it should have two passes:
 * a read pass:
 * - load the information it owns
 * - load the external address pointers
 * - register the old to new pointer mapping
 *
 * a fixup pass:
 * - for each unowned pointer it should try to get a valid reference to that pointer
 *
 ***************************************************************************/
#ifndef ABSERIALIZE_H
#define ABSERIALIZE_H

typedef struct HashTable;

typedef struct SerializeCtxt 
{
	HashTable *newptr_from_oldptr;
} SerializeCtxt;

ABINLINE void serializectxt_init(SerializeCtxt *ctxt)
{
	assert(!ctxt->newptr_from_oldptr);
	ctxt->newptr_from_oldptr = calloc(sizeof(*ctxt->newptr_from_oldptr),1);
	ptrhash_init(ctxt->newptr_from_oldptr,NULL);
}

ABINLINE void serializectxt_cleanup(SerializeCtxt *ctxt)
{
	if(!ctxt)
		return;
	if(ctxt->newptr_from_oldptr)
	{
		hash_cleanup(ctxt->newptr_from_oldptr,NULL);
		free(ctxt->newptr_from_oldptr);
	}
}

ABINLINE void *serialize_fixup_ptr(SerializeCtxt *ctxt, void *oldptr)
{
	return hash_find(DEREF(ctxt,newptr_from_oldptr),oldptr);
}

ABINLINE void serialize_fixup_add_ptr(SerializeCtxt *ctxt, void *oldptr, void *newptr)
{
	if(!ctxt) 
		return;
	ptrhash_insert(ctxt->newptr_from_oldptr,oldptr,newptr);	
}



ABINLINE int int_binwrite(File *fp, int n) {return abfwrite(&n,sizeof(n),1,fp);}
ABINLINE int ptr_binwrite(File *fp, void *p) {return abfwrite(&p,sizeof(p),1,fp);}

ABINLINE int string_binwrite(File *fp, char *s)
{
    int n;
    if(!s)
        return int_binwrite(fp,0);
    n = strlen(s)+1;
    return int_binwrite(fp,n)
        && abfwrite(s,sizeof(*s)*n,1,fp); // include NULL for fun
}

ABINLINE int int_binread(File *fp, int *n)    {return abfread(n,sizeof(*n),1,fp);}
ABINLINE int ptr_binread(File *fp, void **n)  {return abfread(n,sizeof(*n),1,fp);}

ABINLINE int string_binread(File *fp, char **s)
{
    int n;
    if(!int_binread(fp,&n) || n < 0)
        return 0;
    else if(n == 0)
    {
        if(*s)
            free(*s);
        *s = 0;
        return 0;
    }
    (*s) = realloc(*s,n);
    return abfread(*s,sizeof(**s)*(n),1,fp); // include NULL for fun
}

#define array_binwrite(FP,D,N) mem_binwrite(FP,D,N,sizeof(*(D)))
#define array_binread(FP,D,N)  mem_binread(FP,D,N,sizeof(**D))

#define struct_binwrite(FP,D) mem_binwrite(FP,D,1,sizeof(*(D)))
#define struct_binread(FP,D)  mem_binread(FP,D,NULL,sizeof(**D))

ABINLINE int mem_binwrite(File *fp, void *d, int n, int size)
{
	assert(size>0);
    if(!int_binwrite(fp,n))
        return 0;
    if(!d)
        return (n == 0); // if 0 bytes written, success.
    return abfwrite(d,n*size,1,fp);
}

ABINLINE int mem_binread(File *fp, void **d, int *n_d, int size)
{
    int n;
    if(!int_binread(fp,&n) || n < 0)
        return 0;
    if(n_d)
        *n_d = n;
	// edge case: nothing written
    if(n == 0) 
    {
        if(*d)
            free(*d);
        *d = 0;
        return 1;
    }
    *d = realloc(*d,n*size);
    return abfread(*d,n*size,1,fp);
}



#endif //ABSERIALIZE_H
