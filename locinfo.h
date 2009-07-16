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
//     foo();         // tag = foo, ref = NULL, ctxt = func test_func
// }

// Foo **bar2;        // tag = bar2, ref = Foo, ctxt = "global var"

// #define FOO()      // tag = FOO, ref = NULL, ctxt = NULL

typedef struct LocInfo
{
    char *tag;
    char *referrer; // for refs, the function where this call happens
    char *context;
    char *file;
    int lineno;
    char *line;
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

void locinfo_print(LocInfo *li);
// int locinfo_vprintf(LocInfo *li,char *fmt,va_list args);
// int locinfo_printf(LocInfo *li,char *fmt,...);

int parse_add_locinfo(Parse *p,char *filename, int lineno, char *line, char *tag, char *referrer, char *context);
int parse_add_locinfof(Parse *p,char *filename, int lineno, char *line, char *tag, char *referrer, char *context, ...);
int parse_add_locinfov(Parse *p,char *filename, int lineno, char *line, char *tag, char *referrer, char *context,va_list args);

int parse_print_search_tag(Parse *p,char *tag);
void parse_cleanup(Parse *p);

void locinfo_print_time();

int test_locinfo(void);

#endif //LOCINFO_H
