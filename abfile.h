/***************************************************************************
 *     Copyright (c) 2009-2009, Aaron Brady
 *     All Rights Reserved
 *
 * Module Description:
 *
 *
 ***************************************************************************/
#ifndef AB_FILE_H
#define AB_FILE_H

typedef enum FileMode
{
    FileMode_None,
    FileMode_CRT,
    FileMode_Mem,
} FileMode;

typedef struct MemFile
{
    char *p;
    S64 i;
    S64 n;
} MemFile;
typedef enum FileFlags
{
    File_R  = 1,
    File_W  = 2,
    File_RW = 3, // bitmask
} FileFlags;

typedef struct File
{
    FileMode mode;
    union
    {
        void *crt;
        MemFile mem;
    } fp;
} File;


File *MemFile_load(char const *fn);

File *abfopen(char const *fn,FileFlags mode);
void abfclose(File *f);
size_t abfread (void       *buf, size_t n1, size_t n2, File *f);
size_t abfwrite(void const *buf, size_t n1, size_t n2, File *f);

int abgetc(File *f);
int abungetc(int c, File *f);
S64 abfseek(File *f, S64 offset, int origin);

#ifndef NO_FILE_OVERRIDES

#endif //  NO_FILE_OVERRIDES 

#endif //AB_FILE_H
