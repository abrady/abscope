/***************************************************************************
 *     Copyright (c) 2009-2009, Aaron Brady
 *     All Rights Reserved
 *
 * Module Description:
 *
 *
 ***************************************************************************/
#ifndef ABUTIL_H
#define ABUTIL_H

#define _CRT_SECURE_NO_WARNINGS 1

#ifdef _DEBUG 
// http://msdn.microsoft.com/en-us/library/aa383745(VS.85).aspx
#define WIN32_LEAN_AND_MEAN 
#define _WIN32_WINNT 	0x0501
#include <windows.h>
//#include <winsock2.h>
#include <winbase.h>  // for IsDebuggerPresent
#endif

typedef unsigned __int64 U64;
typedef __int64 S64;
typedef volatile __int64 VS64;
typedef volatile unsigned __int64 VU64;


#define _CRT_SECURE_NO_WARNINGS 1
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <stdarg.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <io.h>

// todo: get the std types in here.
typedef int int32_t;

#include "strs.h"

#define DIMOF(A) (sizeof(A)/sizeof(*(A)))
#ifndef R_OK
#   define R_OK 0x04       // for access()
#endif

#ifndef S_ISREG
#   define S_ISREG(B) ((B)&_S_IFREG)
#   define S_ISDIR(B) ((B)&_S_IFDIR)
#endif

typedef struct DirScan
{
    char **files;
    int n_files;
} DirScan;

int file_exists(char *fname);
char* str_downcase(char *str);
int match_ext(char *fn, char *ext);

typedef BOOL (*dirscan_fp)(char *filename, void *ctxt);
void scan_dir(DirScan *d, const char *adir, int recurse_dir,dirscan_fp add_file_callback, void *callback_ctxt);


S64 timer_get();
S64 timer_diff(S64 timer_start);
S64 timer_diff_reset(S64 *timer_start);

double timer_elapsed(S64 timer);
double timer_diffelapsed(S64 timer);
#define TIMER_START() S64 timer_start = timer_get()
#define TIMER_END(DEST) DEST += timer_diff(timer_start);



#define TIME_ALLOCS
#ifdef TIME_ALLOCS
double alloc_time();
void *abmalloc (size_t size);
void *abcalloc (size_t size, size_t n);
void *abrealloc  (void *p, size_t size);
void abfree(void *p);

#undef calloc
#undef malloc
#undef realloc
#undef free
#define calloc  abcalloc
#define malloc  abmalloc
#define realloc abrealloc
#define free    abfree
#endif

#endif //ABUTIL_H
