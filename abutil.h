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

// http://msdn.microsoft.com/en-us/library/aa383745(VS.85).aspx
#define WIN32_LEAN_AND_MEAN 
#define _WIN32_WINNT 	0x0501
#include <windows.h>
#include "assert.h"
//#include <winsock2.h>

#ifdef _DEBUG 
#include <winbase.h>  // for IsDebuggerPresent
#define break_if_debugging() ((IsDebuggerPresent())?DebugBreak(),1:1)

#else

#define break_if_debugging() 0
#endif

#define abassert(C) (break_if_debugging(),assert(C))
typedef enum ErrorLvl
{
    ErrorLvl_None,
    ErrorLvl_Warn,
    ErrorLvl_Fatal,
} ErrorLvl;
int errorfv(ErrorLvl lvl, char *msg,va_list args);
int errorf (ErrorLvl lvl, char *msg, ...);

#define Errorf(MSG,...) errorf(ErrorLvl_Warn,MSG,__VA_ARGS__)
#define abassertmsg(C,msg,...) (break_if_debugging(),(C)?errorf(ErrorLvl_Fatal,msg,__VA_ARGS__):0)
#define STATIC_ASSERT(C) extern int (*static_assert_failed())[(C)?1:0]

typedef int S32;
typedef unsigned int U32;
typedef unsigned __int64 U64;
typedef __int64 S64;
typedef volatile __int64 VS64; 
typedef volatile unsigned __int64 VU64;
typedef unsigned char U8;
typedef signed char S8;

#define ABINLINE static __forceinline

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
#include "abfile.h"

#define DIMOF(A) (sizeof(A)/sizeof(*(A)))
#define SSTR(S) S,DIMOF(S)
#define ZeroStruct(ptr) memset((ptr), 0, sizeof(*(ptr)))
#define ZeroStructs(ptr,n) memset((ptr), 0, sizeof(*(ptr))*(n))
#define CopyStructs(dst,src,n) memmove(dst,src,(n)*sizeof(*(src)))

#ifndef R_OK
#   define R_OK 0x04       // for access()
#endif

#ifndef S_ISREG
#   define S_ISREG(B) ((B)&_S_IFREG)
#   define S_ISDIR(B) ((B)&_S_IFDIR)
#endif

#ifndef BOOL
#   define BOOL int
#   define FALSE 0
#   define TRUE 1
#endif

typedef struct DirScan
{
    char **files;
    int n_files;
} DirScan;

int file_exists(char *fname);
char *fname_nodir(char *fname);

char* str_downcase(char *str);
int match_ext(char *fn, char *ext); // pass "c" not ".c"

typedef BOOL (*dirscan_fp)(char *filename, void *ctxt);
void scan_dir(DirScan *d, const char *adir, int recurse_dir,dirscan_fp add_file_callback, void *callback_ctxt);

#define stracpy(DST,SRC) (strncpy((DST),SRC,DIMOF(DST)),(DST)[DIMOF(DST)-1]=0,(DST))
#define stricmp _stricmp
#define stracat(DST,S) ((strlen(DST)+1<DIMOF(DST))?strcat(DST,S):DST)
ABINLINE int strbeginswith(char *dest, char *src) {int n; if(!dest || !src) return FALSE; n = strlen(src); return strncmp(dest,src,n);}

ABINLINE void str_replacechar(char *str, char from, char to) {char *t = str; for(;;) {t = strchr(t,from); if(!t) return; *t++ = to;}}


#define DEREF(s,m) ((s)?(s)->m:0)
#define DEREF2(s,m,m2) ((s)?DEREF((s)->m,m2):0)

#ifndef MIN
#define MIN(a,b)	(((a)<(b)) ? (a) : (b))
#define MAX(a,b)	(((a)>(b)) ? (a) : (b))
#define MINMAX(a,min,max) ((a) < (min) ? (min) : (a) > (max) ? (max) : (a))
#endif


#define SIZEOF_MBR(typ, mbr)   (sizeof(((typ*)0)->mbr))
#define MBR_OFFSET(typ, mbr)   (intptr_t)&(((typ*)(0))->mbr)
#define OFFSET_PTR(p,n_bytes)  ((void*)((U8*)(p) + (n_bytes)))
#ifdef DEBUG
ABINLINE S64 timer_get()                        { S64 tmp; if(!QueryPerformanceCounter((LARGE_INTEGER*)&tmp)) return 0; return tmp;}
ABINLINE S64 timer_diff(S64 timer_start)        { S64 cur = timer_get(); return cur - timer_start; }
ABINLINE S64 timer_diff_reset(S64 *timer_start) { S64 diff; if(!timer_start) return 0; diff = timer_diff(*timer_start); *timer_start = timer_get(); return diff;} 
#else
#define timer_get() 0
#define timer_diff(T) (T)
#define timer_diff_reset(T) (T)
#endif

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

#define INRANGE(N,S,E) (((N)>=(S)) && ((N)<E))
#define INRANGE0(N,E) INRANGE(N,0,E)
#define POW_OF_2(N) (((N) & ((N)-1)) == 0)
ABINLINE void free_safe(void *p) { if(p) free(p);}
#endif //ABUTIL_H
