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

char *strs_find_str(char **strs, int n_strs, char *s);
char *strs_add_str(char ***pstrs, int *n_strs, char *s);
char *strs_find_add_str(char ***pstrs, int *n_strs, char *s);

typedef struct StrPool
{
    char *strs;
    char *end;
} StrPool;

char *strpool_find_str(StrPool *pool, char *s);
char *strpool_find_add_str(StrPool *pool, char *s);
char *strpool_add_str(StrPool *pool, char *s);
void strpool_cleanup(StrPool *p);

#endif //STRS_H
