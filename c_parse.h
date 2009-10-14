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

typedef struct StackElt StackElt;

#define MAX_STACK 256



typedef struct CParse
{
    Parse structs;
    Parse structrefs;

    Parse funcs;
    Parse funcrefs;

    Parse defines;
    Parse enums;
    Parse vars;
    Parse srcfiles;
    Parse cryptic;
    
    // state info
    StackElt *stack;
    int m_stack;
    int n_stack;

    File *fp;
    char *parse_file;
    int   parse_line;
    char  parse_error[512];
    char  *parse_context;
    char  last_line[512];
    char  line[512];
    int   i_line;

    // timing
    S64 lex_timing;
    S64 getc_timing;
} CParse;


// *************************************************************************
// invocation
// *************************************************************************
int c_ext(char *file);
int c_parse_files(CParse *cp, DirScan *scan);
int c_parse_file(CParse *cp, char *fn);
int c_on_processing_finished(CParse *cp);

int c_load(CParse *cp);

// *******************************************************************
//  queries
// *******************************************************************

typedef enum CQueryFlag
{
    CQueryFlag_None       = 0,     // search for Foo matches:
    CQueryFlag_Structs    = 1<<1,  // struct Foo {}; enum Foo {}; 
    CQueryFlag_Structrefs = 1<<2,  // Foo a;
    CQueryFlag_Funcs      = 1<<3,  // int foo() {}
    CQueryFlag_Funcrefs   = 1<<4,  // int bar() { foo(); }
    CQueryFlag_Defines    = 1<<6,  // #define FOO
    CQueryFlag_Enums      = 1<<7,  // enum Bar { FOO } Foo;
    CQueryFlag_Srcfile    = 1<<8,  // foo.c
    CQueryFlag_Vars       = 1<<9,  
    CQueryFlag_Cryptic    = 1<<10, 
} CQueryFlag;

int c_findstructs(CParse *cp, char *sn);
int c_findfuncs(CParse *cp, char *name);
int c_query(CParse *cp, char *tag, int query_flags);
void c_parse_cleanup(CParse *p);

void c_parse_print_time();
extern int c_debug;

int c_parse_test();

#endif //C_PARSE_H
