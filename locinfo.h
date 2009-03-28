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

typedef struct LocInfo
{
    char *tok;
    char *file;
    int line;
} LocInfo;

typedef struct FileLocInfo
{
    int32_t tok;
    int32_t file;
    int32_t line;
} FileLocInfo;

int absfile_write_locinfos(char *fn, struct LocInfo *infos, int n_infos);
int absfile_read_locinfos(char *fn, struct LocInfo **pinfos, int *pn_infos);

int locinfo_vprintf(LocInfo *li,char *fmt,va_list args);
int locinfo_printf(LocInfo *li,char *fmt,...);

#endif //LOCINFO_H
