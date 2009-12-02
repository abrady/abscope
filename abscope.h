/***************************************************************************
 *     Copyright (c) 2009-2009, Aaron Brady
 *     All Rights Reserved
 *
 * Module Description:
 *
 *
 ***************************************************************************/
#ifndef ABSCOPE_H
#define ABSCOPE_H

#include "abutil.h"
#include "locinfo.h"
#include "strs.h"

typedef struct Parse Parse;

File *absfile_open_write(char *fn);
File *absfile_open_read(char *fn);
void write_parse_tags(FILE *fp, char *name, Parse *p);


#endif //ABSCOPE_H
