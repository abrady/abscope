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

static void usage(int argc, char**argv)
{
    argc;
    printf("usage: %s <filename>\n",argv[0]);
    printf("processing options:\n"
           "-p\t\t: process the passed file\n"
           "-T    : run tests\n"
           "-D[pt]: debug (p)arse, (t)iming "
           "-Q[qf]: query for (s)tructs, (f)unctions"
#ifdef _DEBUG
           "-Z    : wait for debugger attach"
#endif
        );
}

BOOL dirscan_accept_file(char *path, void *ignored)
{
    ignored;
    return c_ext(path);
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

#define TEST(COND) if(!(COND)) {printf(#COND ": failed\n"); return -1;}

static int test_strpool(void)
{
    StrPool pool = {0};
    char *p;
    int i;
    
    TEST(!strpool_find_str(&pool,"abc"));
    p=strpool_find_add_str(&pool,"abc");
    TEST(p);
    TEST(0==strcmp("abc",strpool_find_add_str(&pool,"abc")));
    TEST(p == strpool_find_add_str(&pool,"abc"));
    
    for(i = 0; i < 100; ++i)
    {
        char tmp[128];
        sprintf(tmp,"%i",i);
        strpool_find_add_str(&pool,tmp);
    }
    p = pool.end;
    for(i = 0; i < 100; ++i)
    {
        char tmp[128];
        sprintf(tmp,"%i",i);
        strpool_find_add_str(&pool,tmp);
    }
    TEST(p == pool.end);
    free(pool.strs);
    return 0;
}

static int test_locinfo(void)
{
    Parse p = {0}; 
    Parse p2 = {0}; 
    printf("testing locinfo...");
    parse_add_locinfo(&p,"foo","bar","baz",0xaabbccdd);
    parse_add_locinfo(&p,"alpha","beta","delta",0xaabbccdd);
    TEST(0==absfile_write_parse("test.absfile",&p));
    TEST(0==absfile_read_parse("test.absfile",&p2));
    TEST(p2.n_locs == 2);
    TEST(0==strcmp(p2.locs[0].tag,"foo"));
    TEST(0==strcmp(p2.locs[0].context,"bar"));
    TEST(0==strcmp(p2.locs[0].file,"baz"));
    TEST(0==strcmp(p2.locs[1].tag,"alpha"));
    TEST(0==strcmp(p2.locs[1].context,"beta"));
    TEST(0==strcmp(p2.locs[1].file,"delta"));    
    printf("done.\n");
    free(p.locs);
    free(p.pool.strs);
    free(p2.locs);
    free(p2.pool.strs);
    return 0;
}

static int abscope_test()
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

    TEST(0==test_strpool());
    TEST(0==test_locinfo());
    
    printf("scanning ./test...");
    scan_dir(&dir_scan,"./test",1,&dirscan_accept_file,NULL);
    printf("done.\n");

    printf("found %i files:\n",dir_scan.n_files);
    TEST(dir_scan.n_files >= 3);
    for(i = 0; i < dir_scan.n_files;++i)
    {
        int r;
        printf("scanning %s",dir_scan.files[i]);
        r = c_parse_file(&cp,dir_scan.files[i]);
        TEST(r>=0);
    }

    for(i = 0; i < DIMOF(structs_to_find); ++i)
        TEST(0<c_findstructs(&cp,structs_to_find[i]));
    for(i = 0; i < DIMOF(structs_not_to_find); ++i)
        TEST(0<c_findstructs(&cp,structs_not_to_find[i]));
    return 0;
}

static CParse g_cp;

int main(int argc, char **argv)
{
    int res = 0;
    int print_timers = 0;
    S64 timer_start;
    DirScan dir_scan = {0};
    int i;
    struct CParse *cp = &g_cp;
    int process = 0;
    int c_query_flags = 0;
    char *query_str = 0;

    if(argc < 2)
        return -1;

    timer_start = timer_get();

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
                while(*a && !isspace(*a))
                {
                    switch(*a)
                    {
                    case 'p':
                        c_debug = 1;
                        break;
                    case 't':
                        print_timers = 1;
                        break;
                    default:
                        printf("unrecognized switch %d\n",*a);
                        break;
                    };
                    a++;
                };
                break;
            case 'p':   // add a file to process
                strs_find_add_str(&dir_scan.files,&dir_scan.n_files,argv[++i]);
                process = 1;
                break;
            case 'Q':               // Query for something 
                query_str = argv[++i];
                while(!isspace(*a))
                {
                    switch(*a)
                    {
                    case 's':
                        c_query_flags |= CQueryFlag_Struct;
                        break;
                    case 'f':
                        c_query_flags |= CQueryFlag_Func;
                        break;
                    default:
                        fprintf(stderr, "unknown query option %c in %s\n",*(a-1), argv[i]);
                        return -1;
                    };
                    a++;
                }                
                break;
            case 'R':
                i++;
                scan_dir(&dir_scan,(argv[i] && *argv[i] != '-')?argv[i]:".",1,dirscan_accept_file,NULL);
                process = 1;
                break;
            case 'T':
                return abscope_test();
            case 'f':
                i++;
                // c_debug = 1;
                if(0!=c_parse_file(cp,argv[i]))
                {
                    fprintf(stderr,"failed to parse %s",argv[i]);
                    return -1;
                }
                c_on_processing_finished(cp);
                i++;
                break;
            };
        }
        break;
        };
    }

    if(process)
    {
        printf("processing %i files\n", dir_scan.n_files);
        if(print_timers)
        {
            printf("scanning took %f seconds\n", timer_diffelapsed(timer_start));
            timer_start = timer_get();
        }
        
        for(i = 0; i < dir_scan.n_files; ++i)
        {
            char *fn = dir_scan.files[i];
            if(c_ext(fn))
                res += c_parse_file(cp,fn);
        }
        
        res += c_on_processing_finished(cp);
        goto end;
    }
    

    if(query_str)
    {
        if(c_load(cp)<0)
            return -1;
        res = c_query(cp,query_str,c_query_flags);
        goto end;
    }
    
end:
    if(print_timers)
    {
        printf("process took %f\n"
               "queries took %f\n"
               "allocs took %f seconds\n", 
               timer_diffelapsed(timer_start), 
               locinfo_time(),
               alloc_time());
        c_parse_print_time(cp);
    }
    return res;
}
