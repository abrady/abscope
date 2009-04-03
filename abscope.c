/***************************************************************************
 *     Copyright (c) 2009-2009, Aaron Brady
 *     All Rights Reserved
 *
 * Module Description:
 *
 *
 ***************************************************************************/
#include "abscope.h"
#include "c_parse.h"
#include "c.tab.h"
#include "dirent.h"



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


int main(int argc, char **argv)
{
    DirScan dir_scan = {0};
    int interactive = 0;
    int i;
    struct CParse cp = {0};
    int process = 0;
    char *find_struct = 0;

    if(argc < 2)
    {
        return 1;
    }

    for(i = 1; i < argc; ++i)
    {
        char *a = argv[i];
        switch(*a){
        case '-':
        {
            a++;
            switch(*a){
            case 'D':
                c_debug = 1;
                break;
            case 'p':   // add a file to process
                strs_find_add_str(&dir_scan.files,&dir_scan.n_files,argv[++i]);
                process = 1;
                break;
            case 's':
                find_struct = argv[++i];
                break;
            case 'R':
                scan_dir(&dir_scan,argv[++i],1);
                process = 1;
                break;
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
    
    c_load(&cp);
    
    if(find_struct) {
        c_findstructs(&cp,find_struct);
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
    printf("\ndone.");
    return 0;
}
