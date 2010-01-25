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
#include "abhash.h"
#include "locinfo.h"

#include "string.h"
#include "stdio.h"
#include "stdlib.h"

typedef struct StackElt StackElt;

#define MAX_STACK 256

typedef struct SrcInfo
{
	time_t st_mtime;
} SrcInfo;

typedef enum ParseType
{
	ParseType_structs,
	ParseType_structrefs,
	ParseType_structmbrs,
	ParseType_funcs,
	ParseType_funcrefs,
	ParseType_defines,
	ParseType_enums,
	ParseType_vars,
	ParseType_srcfiles,
	ParseType_cryptic,
	ParseType_Count
} ParseType;

typedef struct CParse
{
	// parsed data
#pragma warning(disable:4201) // nonstandard extension : nameless struct/union
	union
	{
		struct
		{
			Parse structs;
			Parse structrefs;
			Parse structmbrs;
			Parse funcs;
			Parse funcrefs;
			Parse defines;
			Parse enums;
			Parse vars;
			Parse srcfiles;
			Parse cryptic;
		};
		Parse parses[ParseType_Count];
	};

	struct
	{
		HashTable srcinfo_from_str; 
	} project;

	StrPool *pool;

	// ------------------------------
    // state info (used during parsing)

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
int		c_ext(char *file);
void    c_parse_init(CParse *cp);
int		c_parse_files(CParse *cp, char **files, int n_files);
int		c_parse_file(CParse *cp, char *fn);
void	c_remove_files(CParse *cp, char **fns, int n_fns);
int		c_parse_write(CParse *cp);

int c_parse_load(CParse *cp);

// *******************************************************************
//  queries
// *******************************************************************

typedef enum CQueryFlag
{
    CQueryFlag_None       = 0,     // search for Foo matches:
    CQueryFlag_Structs    = 1<<0,  // struct Foo {}; enum Foo {}; 
    CQueryFlag_Structrefs = 1<<1,  // Foo a;
    CQueryFlag_Structmbrs = 1<<1,  // struct { int foo; };
    CQueryFlag_Funcs      = 1<<2,  // int foo() {}
    CQueryFlag_Funcrefs   = 1<<3,  // int bar() { foo(); }
    CQueryFlag_Defines    = 1<<4,  // #define FOO
    CQueryFlag_Enums      = 1<<5,  // enum Bar { FOO } Foo;
    CQueryFlag_Srcfile    = 1<<6,  // foo.c
    CQueryFlag_Vars       = 1<<7,  
    CQueryFlag_Cryptic    = 1<<8,
} CQueryFlag;

int c_query(CParse *cp, char *tag, int query_flags, LocInfoField flds);
void c_parse_cleanup(CParse *p);

void c_parse_print_time();
extern int c_debug;

int c_parse_test();

#endif //C_PARSE_H
