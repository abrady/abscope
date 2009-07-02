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
    char *referrer; // for refs, the function where this call happens
    char *context;
    char *file;
    int line;
} LocInfo;

typedef struct TagRef
{
    char *tag;
    LocInfo loc;
} TagRef;

typedef struct Parse
{
    StrPool strs;
    struct LocInfo *locs;
    int       n_locs;
} Parse;

    

int absfile_write_parse(char *fn, Parse *p);
int absfile_read_parse(char *fn, Parse *p);

int locinfo_vprintf(LocInfo *li,char *fmt,va_list args);
int locinfo_printf(LocInfo *li,char *fmt,...);

int parse_add_locinfo(Parse *p,char *filename, int line, char *tag, char *referrer, char *context);
int parse_add_locinfof(Parse *p,char *filename, int line, char *tag, char *referrer, char *context, ...);
int parse_add_locinfov(Parse *p,char *filename, int line, char *tag, char *referrer, char *context,va_list args);
void parse_copy_parse(Parse *dst, Parse *src);

int parse_print_search_tag(Parse *p,char *tag);
void parse_cleanup(Parse *p);

void locinfo_print_time();

int test_locinfo(void);

#endif //LOCINFO_H
