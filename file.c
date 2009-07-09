/***************************************************************************
 *     Copyright (c) 2009-2009, Aaron Brady
 *     All Rights Reserved
 *
 * Module Description:
 *
 *
 ***************************************************************************/
#include "abutil.h"

#define NO_FILE_OVERRIDES
#include "file.h"



File *MemFile_load(char *fn)
{
    File *f;
    FILE *fp = fopen(fn,"rb");
    long n;
    if(!fp)
        return NULL;
    n = filelength(fp);
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
        fclose(fp);
        free(f->fp.mem.p);
        free(f);
        return NULL;
    }
    return f;
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
            res -= (mem->n - mem->i);
        memmove(buf,mem->p,res);
        mem->i += res;
        break;
    default:
        assert(0 && "unknown mode %i",f->mode);
    }
    return res;
}

int abfgetc(FileWrapper *f)
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
        assert(0 && "unknown mode %i in " __FUNCTION__,f->mode);
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
        assert(0 && "unknown mode %i in " __FUNCTION__,f->mode);
    }
    return 0;    
}

#define fclose(f) x_fclose(f)
