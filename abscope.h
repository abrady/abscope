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


// todo: get the std types in here.
typedef int int32_t;


#include "locinfo.h"
#include "strs.h"

char* downcase(char *str);

FILE *absfile_open_write(char *fn);
FILE *absfile_open_read(char *fn);

#endif //ABSCOPE_H
