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

char *strs_find_str(char **strs, int n_strs, char *s);
char *strs_add_str(char ***pstrs, int *n_strs, char *s);
char *strs_find_add_str(char ***pstrs, int *n_strs, char *s);
void strs_cleanup(char **strs, int n_strs);

//char *strs_pack(char **strs, int n_strs, int *n_bytes_res);

typedef struct StrPool
{
    AvlTree tree;
    char **strs;
    int n_strs;
} StrPool;

char *strpool_find_str(StrPool *pool, char *s);
char *strpool_find_add_str(StrPool *pool, char *s);
char *strpool_add_str(StrPool *pool, char *s);
void strpool_add_strblock(StrPool *pool, char *strblock, int n);

void strpool_cleanup(StrPool *p);

int strpool_binwrite(FILE *fp, StrPool *pool);


int test_strpool(void);

#endif //STRS_H
