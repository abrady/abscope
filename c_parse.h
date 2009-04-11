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


// *************************************************************************
// for parser
// *************************************************************************

void add_struct_decl(Parse *ctxt, char *struct_name, int line);

// *************************************************************************
// invocation
// *************************************************************************
int c_ext(char *file);
int c_process_file(Parse *cp, char *fn);
int c_on_processing_finished(Parse *cp);

int c_load(struct Parse *cp);
int c_findstructs(Parse *cp, char *sn);

extern int c_debug;
#endif //C_PARSE_H
