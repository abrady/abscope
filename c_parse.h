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
    // gathered info
    struct LocInfo *structs;
    int       n_structs;

    // state info
    FILE *fp;
    char *parse_file;
    int   parse_line;
} CParse;

// *************************************************************************
// for parser
// *************************************************************************

void add_struct_decl(struct CParse *ctxt, char *struct_name);

// *************************************************************************
// invocation
// *************************************************************************

int c_process_file(struct CParse *cp, char *fn);
int c_on_processing_finished(struct CParse *cp);

int c_load(struct CParse *cp);
int c_findstructs(CParse *cp, char *sn);

extern int c_debug;
#endif //C_PARSE_H
