/***************************************************************************
 *     Copyright (c) 2009-2009, Aaron Brady
 *     All Rights Reserved
 *
 * Module Description:
 *  Module for parsing C files into useful data
 *
 ***************************************************************************/
#ifndef C_PARSE_H
#define C_PARSE_H

#include "abscope.h"

#include "string.h"
#include "stdio.h"
#include "stdlib.h"

typedef struct CParse
{
    Parse structs;

    // state info
    FILE *fp;
    char *parse_file;
    int   parse_line;
    char  parse_error[512];
} CParse;

// *************************************************************************
// for parser
// *************************************************************************

void c_add_struct(CParse *ctxt, char *struct_name, int line);

// *************************************************************************
// invocation
// *************************************************************************
int c_ext(char *file);
int c_parse_file(CParse *cp, char *fn);
int c_on_processing_finished(CParse *cp);

int c_load(CParse *cp);
int c_findstructs(CParse *cp, char *sn);

extern int c_debug;
#endif //C_PARSE_H
