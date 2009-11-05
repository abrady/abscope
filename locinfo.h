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

typedef enum LocInfoField
{
	LocInfoField_None	  = 0,
	LocInfoField_tag	  = 1<<0,
	LocInfoField_referrer = 1<<1,
	LocInfoField_context  = 1<<2,
	LocInfoField_file	  = 1<<3,
	LocInfoField_line	  = 1<<4,
} LocInfoField;

// example LocInfos:
// typedef struct Foo // tag = Foo, ref = NULL, ctxt = struct Foo,
// {
//     int a;         // tag = a  , ref = int , ctxt = struct Foo,
//     char *b;       // tag = b  , ref = char, ctxt = struct Foo,
// } Foo;

// enum Bar           // tag = Bar, ref = NULL, ctxt = enum Bar
// {
//     FOO,           // tag = FOO, ref = 0   , ctxt = enum Bar 
// };

// void test_func()   // tag = test_func,ref = NULL, ctxt = func test_func 
// {
//     int a;         // ignored
//     Foo b;         // tag = b, ref = Foo, ctxt = func test_func
//     do
//     {
//         if(0)
//         {
//             Bar c; // tag = c, ref = Bar   , ctxt = func test_func
//         }
//     }while(0);    
//     foo();         // tag = foo, ref = test_func, ctxt = func test_func
//     b.a = 1;       // tag = a  , ref = b   , ctxt = func test_func
//     a   = 0;       // tag = a  , ref = NULL, ctxt = func test_func
// }

// Foo **bar2;        // tag = bar2, ref = Foo, ctxt = "global var"

// #define FOO()      // tag = FOO, ref = NULL, ctxt = NULL
typedef struct Parse Parse;

typedef struct LocInfo
{
    char *tag;
    char *referrer;
    char *context;
    char *file;
    int lineno;
    char *line;

    Parse *child;
    
    // scratch pointer
    struct LocInfo *ref;
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

// void locinfo_print(LocInfo *li);
// int locinfo_vprintf(LocInfo *li,char *fmt,va_list args);
// int locinfo_printf(LocInfo *li,char *fmt,...);

int parse_add_locinfo(Parse *p,char *filename, int lineno, char *line, char *tag, char *referrer, char *context);
int parse_add_locinfof(Parse *p,char *filename, int lineno, char *line, char *tag, char *referrer, char *context, ...);
int parse_add_locinfov(Parse *p,char *filename, int lineno, char *line, char *tag, char *referrer, char *context,va_list args);
int parse_locinfos_from_ref(Parse *p, char *ref, LocInfo ***res);
int parse_locinfos_from_context(Parse *p, char *ref, LocInfo ***res);
int parse_print_search(Parse *p,char *tag, LocInfoField flds);
void parse_cleanup(Parse *p);

void locinfo_print_time();
int locinfo_fields_from_str(char *s);

int test_locinfo(void);

#endif //LOCINFO_H
