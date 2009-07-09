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
    FileMode_CRT,
    FileMode_Mem,
    FileMode_None,
} FileMode;

typedef struct MemFile
{
    char *p;
    S64 i;
    S64 n;
} MemFile;


typedef struct File
{
    FileMode mode;
    union
    {
        FILE *crt;
        MemFile mem;
    } fp;
} File;


File *MemFile_load(char *fn); // read a file into memory

size_t abfread(void *buf, size_t n1, size_t n2, File *f);
int abfgetc(File *f);
S64 abfseek(File *f, S64 offset, int origin);

#ifndef NO_FILE_OVERRIDES

#define FILE File 
#define getc(f) abfgetc(f)
#define fseek(f, dist, whence) abfseek(f, dist, whence)
//#define fopen(name, how) x_fopen(name, how, __FILE__, __LINE__)
#define fclose(f) x_fclose(f)
#define ftell(f) x_ftell(f)
#define fread(buf, size, count, f) x_fread(buf, size, count, f)
#define fwrite(buf, size, count, f) x_fwrite(buf, size, count, f)
#define fgets(buf, size, f) x_fgets(buf, size, f)
#define vfprintf x_vfprintf
#define fprintf x_fprintf
#define ferror x_ferror
#define fflush(f) x_fflush(f)
// #define fscanf x_fscanf
#define fgetc(f) x_fgetc(f)
#define fputc(c, f) x_fputc(c, f)
#define setvbuf(f, buf, mode, size) x_setvbuf(f, buf, mode, size)


#endif //  NO_FILE_OVERRIDES 

#endif //AB_FILE_H
