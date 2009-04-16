/***************************************************************************
 *     Copyright (c) 2009-2009, Aaron Brady
 *     All Rights Reserved
 *
 * Module Description:
 *
 *
 ***************************************************************************/
#include "abutil.h"
#include "dirent.h"

int file_exists(char *fname)
{
	struct stat status;
	if(!stat(fname, &status) && status.st_mode & _S_IFREG)
        return 1;
	return 0;
}

int match_ext(char *fn, char *ext)
{
    char *fne = strrchr(fn,'.');
    return fne && 0 == strcmp(fne+1,ext);
}

void scan_dir(DirScan *d, const char *adir, int recurse_dir,dirscan_fp add_file_callback, void *callback_ctxt)
{
	DIR	*dirfile;
	int adir_len = strlen(adir);
    
	if ((dirfile = opendir(adir)) != NULL) {
		struct dirent *entry;
		char	path[MAX_PATH + 1];
		char	*file;
        
		while ((entry = readdir(dirfile)) != NULL)
        { 
            struct stat buf;
			if(!strcmp(".",entry->d_name) || !strcmp("..",entry->d_name))
                continue;
                
            sprintf(path,"%s/%.*s", adir, MAX_PATH-2-adir_len, entry->d_name);
                
            if (stat(path,&buf) != 0)
                continue;
                    
            file = entry->d_name;
            if (recurse_dir && S_ISDIR(buf.st_mode) )
                scan_dir(d, path, recurse_dir, add_file_callback, callback_ctxt);
            else if (_access(path, R_OK) == 0 && add_file_callback(path, callback_ctxt))
                strs_find_add_str(&d->files,&d->n_files,_strdup(path));
		}
		closedir(dirfile);
	}
    return;
}

char* str_downcase(char *str)
{
    char *p = str;
    while(*str)
    {
        *str = (char)tolower(*str); 
        str++;
    }
    return p;
}

S64 timer_get()
{
    S64 tmp;
    if(!QueryPerformanceCounter((LARGE_INTEGER*)&tmp))
        return 0;
    return tmp;
}

static S64 alloc_timer = 0;

#define ALLOC_TIMER_START() S64 timer_start = timer_get()
#define ALLOC_TIMER_END()   alloc_timer += timer_diff(timer_start)

S64 timer_diff(S64 timer_start)
{
    S64 cur = timer_get();
    return cur - timer_start;
}

#ifdef TIME_ALLOCS
#undef calloc
#undef malloc
#undef realloc
#undef free
#endif


void *abmalloc (size_t size)
{
    void *res;
    ALLOC_TIMER_START();
    res = malloc(size);
    ALLOC_TIMER_END();
    return res;
}

void *abcalloc (size_t size, size_t n)
{
    void *res;
    ALLOC_TIMER_START();
    res = calloc(size,n);
    ALLOC_TIMER_END();
    return res;
}

void *abrealloc (void *p, size_t size)
{
    void *res;
    ALLOC_TIMER_START();
    res = realloc(p,size);
    ALLOC_TIMER_END();
    return res;
}

void abfree(void *p)
{
    ALLOC_TIMER_START();
    free(p);
    ALLOC_TIMER_END();
}

double alloc_time()
{
    return timer_elapsed(alloc_timer);
}

double timer_diffelapsed(S64 timer)
{
    return timer_elapsed(timer_diff(timer));
}

double timer_elapsed(S64 timer)
{
    S64 freq = 0;
    if(!QueryPerformanceFrequency((LARGE_INTEGER*)&freq))
        return 0.;
    return timer / (double)freq;
}

S64 timer_diff_reset(S64 *timer_start)
{
    S64 diff;
    if(!timer_start)
        return 0;
    diff = timer_diff(*timer_start);
    *timer_start = timer_get();
    return diff;
}