/***************************************************************************
 *     Copyright (c) 2009-2009, Aaron Brady
 *     All Rights Reserved
 *
 * Module Description:
 *
 *
 ***************************************************************************/
#ifndef STRS_H
#define STRS_H

#include "abtree.h"
#include "abhash.h"

typedef struct SerializeCtxt SerializeCtxt;

// dumb 'ol array of strings
char *strs_find_str(char **strs, int n_strs, char *s);
char *strs_add_str(char ***pstrs, int *n_strs, char *s);
char *strs_find_add_str(char ***pstrs, int *n_strs, char *s);
void strs_cleanup(char **strs, int n_strs);

//char *strs_pack(char **strs, int n_strs, int *n_bytes_res);
typedef struct StrBlock StrBlock;
typedef struct HashTable HashTable;

typedef struct StrPool
{
	StrBlock **strblocks;
	HashTable *dict; 
} StrPool;

char	*strpool_find(StrPool *pool, char *s);
char	*strpool_add(StrPool *pool, char *s);
void	 strpool_cleanup(StrPool *p);
int		 strpool_test(void);

int strpool_write(char *fn, StrPool *pool);
int strpool_read(char *fn, StrPool *pool, SerializeCtxt *ctxt);

// *************************************************************************
//  str formatting stuff
// *************************************************************************

int   str_vsprintf(char **dst,char *fmt,va_list args);
int   str_sprintf(char **dst, char *fmt, ...);

char* str_cat  (char **dst, char *src);
char* str_catf (char **dst, char *fmt, ...);
char* str_vcatf(char **dst,char *fmt,va_list args);

// turn the first four bytes of a string into a U32
U32 hexify_chars(char a, char b, char c, char d);
U32 hexify_str(char *s);


#endif //STRS_H
