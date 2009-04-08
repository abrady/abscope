/***************************************************************************
 *     Copyright (c) 2009-2009, Aaron Brady
 *     All Rights Reserved
 *
 * Module Description:
 *
 *
 ***************************************************************************/
#include "abscope.h"
#include "dirent.h"
#include "c_parse.h"
#include "c.tab.h"



typedef struct DirScan
{
    char **files;
    int n_files;
} DirScan;

int match_ext(char *fn, char *ext)
{
    char *fne = strrchr(fn,'.');
    return fne && 0 == strcmp(fne+1,ext);
}

/* scan a directory (recursively?) for source files */
static void
scan_dir(DirScan *d, const char *adir, BOOL recurse_dir)
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
                scan_dir(d, path, recurse_dir);
            else if (c_ext(path) && _access(path, R_OK) == 0)
                strs_find_add_str(&d->files,&d->n_files,_strdup(path));
		}
		closedir(dirfile);
	}
    return;
}

static void usage(int argc, char**argv)
{
    argc;
    printf("usage: %s <filename>\n",argv[0]);
    printf("processing options:\n"
           "-p\t\t: process the passed file\n"
           "-T    : run tests"
           "-D    : parse in debug mode"
#ifdef _DEBUG
           "-Z    : wait for debugger attach"
#endif
        );
}



char* downcase(char *str)
{
    char *p = str;
    while(*str)
    {
        *str = (char)tolower(*str); 
        str++;
    }
    return p;
}



static int32_t ABS_FILE_VERSION = 0x20090326;
FILE *absfile_open_write(char *fn)
{
    FILE *fp = fopen(fn,"wb");
    if(!fp)
        return 0;
    fwrite(&ABS_FILE_VERSION,sizeof(ABS_FILE_VERSION),1,fp);
    return fp;
}

FILE *absfile_open_read(char *fn)
{
    int32_t version = 0;
    FILE *fp = fopen(fn,"rb");
    if(!fp)
        return 0;
    fread(&version,sizeof(ABS_FILE_VERSION),1,fp);
    if(version != ABS_FILE_VERSION)
    {
        fprintf(stderr,"invalid version read from %s, should be %i, got %i",fn,version,ABS_FILE_VERSION);
        fclose(fp);
        return 0;
    }
    
    return fp;
}

#define TEST(COND) if(!(COND)) {printf(#COND "failed\n"); return -1;}
static int test()
{
    int i;
    DirScan dir_scan = {0};
    CParse cp = {0};
    static char *structs_to_find[] = {
        "dirent",
        "Foo",
        "Bar",
        "Baz",
    };
    static char *structs_not_to_find[] = {
        "Cat", // typedef only
    };

    printf("TESTING\n");
    printf("scanning ./test...");
    scan_dir(&dir_scan,"./test",1);
    printf("done.\n");

    printf("found %i files:\n",dir_scan.n_files);
    TEST(dir_scan.n_files == 3);
    for(i = 0; i < dir_scan.n_files;++i)
    {
        int r;
        printf("scanning %s",dir_scan.files[i]);
        r = c_process_file(&cp,dir_scan.files[i]);
        TEST(r>=0);
    }

    for(i = 0; i < DIMOF(structs_to_find); ++i)
        TEST(0<=c_findstructs(&cp,structs_to_find[i]));
    for(i = 0; i < DIMOF(structs_not_to_find); ++i)
        TEST(0<=c_findstructs(&cp,structs_not_to_find[i]));
    return 0;
}

typedef enum CQueryFlag
{
    CQueryFlag_None = 0,
    CQueryFlag_Struct = 1<<1,
    CQueryFlag_Func   = 2<<1,
} CQueryFlag;

int main(int argc, char **argv)
{
    DirScan dir_scan = {0};
    int interactive = 0;
    int i;
    struct CParse cp = {0};
    int process = 0;
    int c_query_flags = 0;
    char *query_str = 0;

    if(argc < 2)
    {
        return 1;
    }

    for(i = 1; i < argc; ++i)
    {
        char *a = argv[i];
        switch(*a++){
        case '-':
        {

            switch(*a++){
#ifdef _DEBUG
            case 'Z':
                fprintf(stderr,"waiting for debugger...");
                while(!IsDebuggerPresent())
                    Sleep(1);
                fprintf(stderr,"done.\n");
                break;
#endif
            case 'D':
                c_debug = 1;
                break;
            case 'p':   // add a file to process
                strs_find_add_str(&dir_scan.files,&dir_scan.n_files,argv[++i]);
                process = 1;
                break;
            case 'Q':               // Query for something 
                query_str = argv[++i];
                switch(*a++)
                {
                case 's':
                    c_query_flags |= CQueryFlag_Struct;
                    break;
                default:
                    fprintf(stderr, "unknown query option %c in %s\n",*(a-1), argv[i]);
                    return -1;
                };
                break;
            case 'R':
                scan_dir(&dir_scan,argv[++i],1);
                process = 1;
                break;
            case 'T':
                return test();
            };
        }
        break;
        };
    }

    if(process)
    {
        int res = 0;
        printf("processing %i files\n", dir_scan.n_files);
        for(i = 0; i < dir_scan.n_files; ++i)
        {
            char *fn = dir_scan.files[i];
            if(c_ext(fn))
                res += c_process_file(&cp,fn);
        }
        
        res += c_on_processing_finished(&cp);
        return res;
    }
    
    if(c_load(&cp)<0)
        return -1;    
    if(c_query_flags) {
        if(c_query_flags & CQueryFlag_Struct)
            c_findstructs(&cp,query_str);
    }
    else if(interactive){
        int c = getchar();
        char s[128];
        switch(c){
        case 's':
            puts("enter struct to search for:");
            gets(s);
            printf("searching for %s:\n",s);
            c_findstructs(&cp,s);
        };
    }
    return 0;
}
