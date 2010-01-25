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

typedef enum FileType
{
    FileType_CRT,
    FileType_Mem,
} FileType;

typedef struct MemFile
{
    char *p;
    size_t i;
    size_t n;
} MemFile;
typedef enum FileFlags
{
    File_R  = 1,
    File_W  = 2,
    File_RW = 3, // bitmask
} FileFlags;

typedef struct File
{
	char *fn;
    FileType type;
    union
    {
        void *crt;
        MemFile mem;
    } fp;
} File;


File *memfile_load(char const *fn);

File *abfopen(char const *fn, FileFlags mode, FileType type);
void abfclose(File *f);
size_t abfread (void       *buf, size_t n1, size_t n2, File *f);
size_t abfwrite(void const *buf, size_t n1, size_t n2, File *f);

int abgetc(File *f);
int abungetc(int c, File *f);
size_t abfseek(File *f, size_t offset, int origin);

int abfile_test();


#endif //AB_FILE_H
