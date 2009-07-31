/***************************************************************************
 *     Copyright (c) 2009-2009, Aaron Brady
 *     All Rights Reserved
 *
 * Module Description:
 *
 *
 ***************************************************************************/
#include "abutil.h"

File *MemFile_load(char const *fn)
{
    File *f;
    FILE *fp = fopen(fn,"rb");
    long n;
    if(!fp)
        return NULL;
    n = _filelength(_fileno(fp));
    if(n < 0)
    {
        fclose(fp);
        return NULL;
    }
    
    f = calloc(sizeof(*fp),1);
    f->mode = FileMode_Mem;
    f->fp.mem.n = n;
    f->fp.mem.p = malloc(n);
    if(1 != fread(f->fp.mem.p,n,1,fp))
    {
        free(f->fp.mem.p);
        free(f);
        f = NULL;
    }
    fclose(fp);
    return f;
}

File *abfopen(char const *fn,FileFlags mode)
{
    File *fp;
    char mode_str[8];
    char *m;
        
    if(mode == File_R)
        return MemFile_load(fn);
    *mode_str = 0;
    m = mode_str;
    if(mode & File_R)
        *m++ = 'r';
    if(mode & File_W)
        *m++ = 'w';
    *m++ = 'b';
    *m++ = 0;
    fp = calloc(sizeof(*fp),1);
    fp->mode = FileMode_CRT;
    fp->fp.crt = fopen(fn,mode_str);
    if(!fp->fp.crt)
    {
        free(fp);
        fp = NULL;
    }
    return fp;
}

void abfclose(File *f)
{
    if(!f)
        return;
    switch (f->mode)
    {
    case FileMode_Mem:
        free(f->fp.mem.p);
        break;
    case FileMode_CRT:
        fclose(f->fp.crt);
        break;
    default:
        abassert(0 && "unknown enum");
        break;
    }
    free(f);
}

size_t abfread(void *buf, size_t n1, size_t n2, File *f)
{
    size_t res;
    MemFile *mem;
    switch(f->mode)
    {
    case FileMode_CRT:
        return fread(buf,n1,n2,f->fp.crt);
        break;
    case FileMode_Mem:
        mem = &f->fp.mem;
        res = n1*n2;
        if(res > mem->n - mem->i)
            res -= (size_t)(mem->n - mem->i);
        memmove(buf,mem->p+mem->i,res);
        mem->i += res;
        return res;
        break;
    default:
        errorf(1,"unknown mode %i",f->mode);
    }
    return 0;
}

size_t abfwrite(void const *buf, size_t n1, size_t n2, File *f)
{
    if(f->mode != FileMode_CRT)
        return 0;
    return fwrite(buf,n1,n2,f->fp.crt);
}


int abgetc(File *f)
{
    MemFile *mem;
    switch(f->mode)
    {
    case FileMode_CRT:
        return fgetc(f->fp.crt);
        break;
    case FileMode_Mem:
        mem = &f->fp.mem;
        if(mem->i < mem->n)
            return mem->p[mem->i++];
        return EOF;
    default:
        Errorf("unknown mode %i in " __FUNCTION__,f->mode);
    }
    return EOF;
}

int abungetc(int c, File *f)
{
    MemFile *mem;
    switch(f->mode)
    {
    case FileMode_CRT:
        return ungetc(c,f->fp.crt);
        break;
    case FileMode_Mem:
        mem = &f->fp.mem;
        if(mem->i <= 0 || c == EOF)
            return EOF;
        return mem->p[--mem->i] = (char)c;
    default:
        Errorf("unknown mode %i in " __FUNCTION__,f->mode);
    }
    return 0;
}


// origin in {SEEK_CUR, SEEK_END, SEEK_SET}
S64 abfseek(File *f, S64 offset, int origin)
{
    MemFile *mem;
    switch(f->mode)
    {
    case FileMode_CRT:
        return _fseeki64(f->fp.crt,offset,origin);
        break;
    case FileMode_Mem:
        mem = &f->fp.mem;        

        if(origin == SEEK_SET)
            mem->i = offset;
        else
        {
            S64 i = mem->i;
            S64 n;
            if(origin == SEEK_END)
                i = mem->n;
            n = i + offset;
            if(i < 0)
                i = 0;
            if(i > mem->n)
                i = mem->n;
            mem->i = i;
        }
    default:
        Errorf("unknown mode %i in " __FUNCTION__,f->mode);
    }
    return 0;    
}

