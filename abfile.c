/***************************************************************************
 *     Copyright (c) 2009-2009, Aaron Brady
 *     All Rights Reserved
 *
 * Module Description:
 *
 *
 ***************************************************************************/
#include "abutil.h"

File *memfile_load(char const *fn)
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
    if(!f)
    {
        fclose(fp);
        return NULL;
    }
            
    f->type = FileType_Mem;
    f->fp.mem.n = n;
    f->fp.mem.p = malloc(n);
    if(!f->fp.mem.p)
    {
        fclose(fp);
        return NULL;
    }

    if(1 != fread(f->fp.mem.p,n,1,fp))
    {
        free(f->fp.mem.p);
        free(f);
        f = NULL;
    }
    fclose(fp);
    return f;
}

File *abfopen(char const *fn,FileFlags mode, FileType type)
{
    File *fp;
    char mode_str[8];
    char *m;
        
    if(mode == File_R)
        return memfile_load(fn);
    *mode_str = 0;
    m = mode_str;
    if(mode & File_R)
        *m++ = 'r';
    if(mode & File_W)
        *m++ = 'w';
    *m++ = 'b';
    *m++ = 0;

    fp = calloc(sizeof(*fp),1);
    if(!fp)
        return NULL;

	fp->type = type;
	fp->fn = strdup(fn);
	switch(type)
	{
	case FileType_CRT:
		fp->fp.crt = fopen(fn,mode_str);
		if(!fp->fp.crt)
		{
			free(fp);
			fp = NULL;
		}
		break;
	case FileType_Mem:
		// ready to go
		break;
	default:
		errorf(1,"unknown mode %i",fp->type);
		break;
	};
    return fp;
}

void abfclose(File *f)
{
    if(!f)
        return;
    switch (f->type)
    {
    case FileType_Mem:
        free_safe(f->fp.mem.p);
        break;
    case FileType_CRT:
        fclose(f->fp.crt);
        break;
    default:
        abassert(0 && "unknown enum");
        break;
    }
	free(f->fn);
    free(f);
}

size_t abfread(void *buf, size_t size, size_t count, File *f)
{
    size_t n;
	size_t res;
    MemFile *mem;
	assert(f->type & File_R);
    switch(f->type)
    {
    case FileType_CRT:
        return fread(buf,size,count,f->fp.crt);
        break;
    case FileType_Mem:
        mem = &f->fp.mem;
        n = size*count;
		res = count;
        if(n > mem->n - mem->i)
		{
            n = (size_t)(mem->n - mem->i);
			res = n/size;
			n = res*size; // only read whole objects
		}
		if(n)
			memmove(buf,mem->p+mem->i,n);
        mem->i += n;
        return res;
        break;
    default:
        errorf(1,"unknown mode %i",f->type);
    }
    return 0;
}

// let's you write to a buffer in memory
static size_t mem_fwrite(void const *buf, size_t n1, size_t n2, File *f)
{
	char *p;
	MemFile *mem;
	size_t n;

	assert(f && buf);
	n = n1*n2;
	mem = &f->fp.mem;
	p = realloc(mem->p,mem->n + n);
	if(!abassert(p))
		return 0;
	memmove(p+mem->n,buf,n);

	mem->p = p;
	mem->n = mem->n + n;
	return n2;
}


size_t abfwrite(void const *buf, size_t n1, size_t n2, File *f)
{
	switch(f->type)
	{
	case FileType_Mem:
		return mem_fwrite(buf,n1,n2,f);
	case FileType_CRT:
		return fwrite(buf,n1,n2,f->fp.crt);
	default:
		abassertmsg(0,"unknown file type %i",f->type);
		return 0;
	};
}


int abgetc(File *f)
{
    MemFile *mem;
	if(!f)
		return EOF;
    switch(f->type)
    {
    case FileType_CRT:
        return fgetc(f->fp.crt);
        break;
    case FileType_Mem:
        mem = &f->fp.mem;
        if(mem->i < mem->n)
            return mem->p[mem->i++];
        return EOF;
    default:
        Errorf("unknown type %i in " __FUNCTION__,f->type);
    }
    return EOF;
}

int abungetc(int c, File *f)
{
    MemFile *mem;
    switch(f->type)
    {
    case FileType_CRT:
        return ungetc(c,f->fp.crt);
        break;
    case FileType_Mem:
        mem = &f->fp.mem;
        if(mem->i <= 0 || c == EOF)
            return EOF;
        return mem->p[--mem->i] = (char)c;
    default:
        Errorf("unknown type %i in " __FUNCTION__,f->type);
    }
    return 0;
}


// origin in {SEEK_CUR, SEEK_END, SEEK_SET}
size_t abfseek(File *f, size_t offset, int origin)
{
    MemFile *mem;
    switch(f->type)
    {
    case FileType_CRT:
        return _fseeki64(f->fp.crt,offset,origin);
        break;
    case FileType_Mem:
        mem = &f->fp.mem;        

        if(origin == SEEK_SET)
            mem->i = offset;
        else
        {
            size_t i = mem->i;
            size_t n;
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
        Errorf("unknown type %i in " __FUNCTION__,f->type);
    }
    return 0;    
}

#define TEST(COND) if(!(COND)) {printf("%s(%d):"#COND ": failed\n",__FILE__,__LINE__); break_if_debugging(); return -1;}

int abfile_test()
{
	File *fp;
	char tmp[] = "abcdefghijklmnopqrstuvwxyz";
	char tmp2[sizeof(tmp)];
	struct Foo 
	{
		int a;
		char b[16];
	} foos[3] = {
		{1,"one"},
		{2,"two"},
		{3,"three"},
	}, bars[DIMOF(foos)] = {0};
	
	printf("abfile test...");
	fp = abfopen("foo",File_W,FileType_Mem);
	TEST(fp);
	TEST(0==strcmp(fp->fn,"foo"));
	TEST(1 == abfwrite(tmp,sizeof(tmp),1,fp));
	TEST(sizeof(tmp) == abfread(tmp2,1,sizeof(tmp),fp));
	TEST(0==memcmp(tmp,tmp2,sizeof(tmp)));
	ZeroStruct(foos);
	abfclose(fp);

	fp = abfopen("bar",File_RW,FileType_Mem);
	TEST(fp);
	TEST(DIMOF(foos) == abfwrite(foos,sizeof(foos[0]),DIMOF(foos),fp));
	TEST(DIMOF(bars) == abfread(bars,sizeof(bars[0]),DIMOF(bars),fp));
	TEST(0==memcmp(foos,bars,sizeof(foos)));
	ZeroStruct(foos);
	abfclose(fp);

	// read too many
	fp = abfopen("bar",File_RW,FileType_Mem);
	TEST(fp);
	TEST(DIMOF(foos) == abfwrite(foos,sizeof(foos[0]),DIMOF(foos),fp));
	TEST(DIMOF(bars) == abfread(bars,sizeof(bars[0]),DIMOF(bars)+1,fp));
	TEST(0==memcmp(foos,bars,sizeof(foos)));
	ZeroStruct(foos);
	abfclose(fp);

	// read too many 2: a little extra data
	fp = abfopen("bar",File_RW,FileType_Mem);
	TEST(fp);
	TEST(1 == abfwrite(foos,sizeof(foos[0])+1,1,fp));
	TEST(1 == abfread(bars,sizeof(bars[0]),DIMOF(bars)+1,fp));
	TEST(0==memcmp(foos,bars,sizeof(foos)));
	ZeroStruct(foos);
	abfclose(fp);
		 
	printf("done.\n");

	return 0;
}
