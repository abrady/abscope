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
    char *tag;
    char *context;
    char *file;
    int line;
} LocInfo;

typedef struct Parse
{
    struct LocInfo *locs;
    int       n_locs;
    StrPool pool;
} Parse;

int absfile_write_parse(char *fn, Parse *p);
int absfile_read_parse(char *fn, Parse *p);

int locinfo_vprintf(LocInfo *li,char *fmt,va_list args);
int locinfo_printf(LocInfo *li,char *fmt,...);

void parse_add_locinfo(Parse *p,char *tag, char *context, char *filename, int line);
int parse_print_search_tag(Parse *p,char *tag);


#endif //LOCINFO_H
