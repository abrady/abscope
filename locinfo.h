/***************************************************************************
 *     Copyright (c) 2009-2009, Aaron Brady
 *     All Rights Reserved
 *
 * Module Description:
 *
 *
 ***************************************************************************/
#ifndef LOCINFO_H
#define LOCINFO_H

#include "abscope.h"
#include "strs.h"

typedef struct LocInfo
{
    char *tok;
    char *file;
    int line;
} LocInfo;

typedef struct Parse
{
    // gathered info
    struct LocInfo *structs;
    int       n_structs;

    // state info
    FILE *fp;
    char *parse_file;
    int   parse_line;
    char  parse_error[512];

    // str pool
    StrPool pool;
} Parse;

int absfile_write_locinfos(char *fn, struct LocInfo *infos, int n_infos, StrPool *pool);
int absfile_read_locinfos(char *fn, struct LocInfo **pinfos, int *pn_infos, StrPool *pool);

int locinfo_vprintf(LocInfo *li,char *fmt,va_list args);
int locinfo_printf(LocInfo *li,char *fmt,...);

void add_locinfo(StrPool *pool, LocInfo **li, int *n, char *tok, char *fn, int line);

#endif //LOCINFO_H
