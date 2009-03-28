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

int32_t strs_find_str(char **strs, int n_strs, char *s);
int32_t strs_add_str(char ***pstrs, int *n_strs, char *s);
int32_t strs_find_add_str(char ***pstrs, int *n_strs, char *s);

#endif //STRS_H
