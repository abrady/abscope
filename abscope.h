/***************************************************************************
 *     Copyright (c) 2009-2009, Aaron Brady
 *     All Rights Reserved
 *
 * Module Description:
 *
 *
 ***************************************************************************/
#ifndef ABSCOPE_H
#define ABSCOPE_H

#define _CRT_SECURE_NO_WARNINGS 1

#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <stdarg.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <io.h>
#define R_OK 0x04       // for access()
#define S_ISREG(B) ((B)&_S_IFREG)
#define S_ISDIR(B) ((B)&_S_IFDIR)

#ifdef _DEBUG 
// http://msdn.microsoft.com/en-us/library/aa383745(VS.85).aspx
#define WIN32_LEAN_AND_MEAN 
#define _WIN32_WINNT 	0x0501
#include <windows.h>
//#include <winsock2.h>
#include <winbase.h>  // for IsDebuggerPresent
#endif


// todo: get the std types in here.
typedef int int32_t;

#include "locinfo.h"
#include "strs.h"

#define DIMOF(A) (sizeof(A)/sizeof(*(A)))

char* downcase(char *str);

FILE *absfile_open_write(char *fn);
FILE *absfile_open_read(char *fn);
int match_ext(char *fn, char *ext);

int file_exists(char *fname);

#endif //ABSCOPE_H
