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
    Parse funcs ;

    // state info
    FILE *fp;
    char *parse_file;
    int   parse_line;
    char  parse_error[512];

    // timing
    S64 lex_timing;
    S64 getc_timing;
} CParse;

// *************************************************************************
// for parser
// *************************************************************************

void c_add_struct  (CParse *ctxt, char *struct_name, int line);
void c_add_func(CParse *ctxt, char *name, int line);

// *************************************************************************
// invocation
// *************************************************************************
int c_ext(char *file);
int c_parse_file(CParse *cp, char *fn);
int c_on_processing_finished(CParse *cp);

int c_load(CParse *cp);

// *******************************************************************
//  queries
// *******************************************************************

typedef enum CQueryFlag
{
    CQueryFlag_None = 0,
    CQueryFlag_Struct = 1<<1,
    CQueryFlag_Func   = 2<<1,
} CQueryFlag;

int c_findstructs(CParse *cp, char *sn);
int c_findfuncs(CParse *cp, char *name);
int c_query(CParse *cp, char *tag, int query_flags);

void c_parse_print_time();
extern int c_debug;
#endif //C_PARSE_H
