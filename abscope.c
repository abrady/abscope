/***************************************************************************
 *     Copyright (c) 2009-2009, Aaron Brady
 *     All Rights Reserved
 *
 * Module Description:
 *
 *
 ***************************************************************************/
#include "abscope.h"
#include "abhash.h"
#include "abtree.h"
#include "c_parse.h"

int g_verbose = 0;

static void usage(int argc, char**argv)
{
    argc;
    printf("usage: %s <filename>\n",argv[0]);
    printf("processing options:\n"
           "-R [dir]\t\t: process the passed dirctory recursively\n"
           "-E\t\t: exclude the matching directory\n"
           "-p\t\t: add the passed file to those processed\n"
           "-f\t\t: process just this file and exit\n"
           "-v\t\t: verbose output\n"
           "-T\t\t: run tests\n"
           "-D[pt]\t\t: debug (p)arse, (t)iming\n"
           "-Q[sSfFdeca]\t\t: query for (s)tructs, (S)tructrefs (f)unctions (F)uncrefs (d)efines sr(c)file (e)nums (a)ll\n"
#ifdef _DEBUG
           "-Z\t\t: wait for debugger attach\n"
#endif
        );
}

BOOL dirscan_accept_file(char *path, char **exclude_dirs)
{
    char *exc;
    while(*exclude_dirs)
    {
        exc = *exclude_dirs++;
        if(strstr(path,exc))
            return 0;
    }
    return c_ext(path);
}

static int32_t ABS_FILE_VERSION = 0x20090715;
File *absfile_open_write(char *fn)
{
    File *fp = abfopen(fn,File_W);
    if(!fp)
        return 0;
    if(1 != abfwrite(&ABS_FILE_VERSION,sizeof(ABS_FILE_VERSION),1,fp))
    {
        abfclose(fp);
        return 0;
    }
    
    return fp;
}

File *absfile_open_read(char *fn)
{
    int32_t version = 0;
    File *fp = abfopen(fn,File_R);
    if(!fp)
        return 0;
    abfread(&version,sizeof(ABS_FILE_VERSION),1,fp);
    if(version != ABS_FILE_VERSION)
    {
        fprintf(stderr,"invalid version read from %s, should be %x, got %x\n",fn,version,ABS_FILE_VERSION);
        abfclose(fp);
        return 0;
    }
    
    return fp;
}

#define TEST(COND) if(!(COND)) {printf("%s(%d):"#COND ": failed\n",__FILE__,__LINE__); break_if_debugging(); return -1;}


static int abscope_test()
{
    printf("TESTING\n");

    TEST(0==avltree_test());
    TEST(0==hash_test()); 
    TEST(0==strpool_test());
    TEST(0==test_locinfo());
    TEST(0==c_parse_test());

    return 0;
}

static CParse g_cp;

static int c_query_flags_from_str(char *a_in)
{
    char *a = a_in;
    int c_query_flags = 0;
    if(!a)
        return 0;
    while(*a && !isspace(*a))
    {
        
        switch(*a)
        {
        case 'd':
            c_query_flags |= CQueryFlag_Defines;
            break;
        case 'e':
            c_query_flags |= CQueryFlag_Enums;
            break;
        case 's':
            c_query_flags |= CQueryFlag_Structs;
            break;
        case 'S':
            c_query_flags |= CQueryFlag_Structrefs;
            break;
        case 'f':
            c_query_flags |= CQueryFlag_Funcs;
            break;
        case 'F':
            c_query_flags |= CQueryFlag_Funcrefs;
            break;
        case 'c':
            c_query_flags |= CQueryFlag_Srcfile;
            break;
        case 'a':
            c_query_flags = 0xffffffff;
            break;
        case 'v':
            c_query_flags |= CQueryFlag_Vars;
            break;
        default:
            fprintf(stderr, "unknown query option %c in %s\n",*a, a_in);
            return 0;
        };
        a++;
    }
    return c_query_flags;
}


int main(int argc, char **argv)
{
    BOOL loop_query = 0;
    S64 query_timer;
    double query_timer_load = 0;
    double query_timer_query = 0;

    int res = 0;
    int print_timers = 0;
    S64 timer_start;
    DirScan dir_scan = {0};
    char *dir_scan_dir = NULL;
    int i;
    struct CParse *cp = &g_cp;
    int process = 0;
    int c_query_flags = 0;
    char *query_str = 0;
    char *exclude_dirs[128] = {0};
    int n_exclude_dirs = 0;

    if(argc < 2)
    {
        usage(argc,argv);
        return -1;
    }

    timer_start = timer_get();

    for(i = 1; i < argc; ++i)
    {
        char *a = argv[i];
        switch(*a++){
        case '-':
        {

            switch(*a++){
            case '?':
            case 'h':
                usage(argc,argv);
                return 0;
                break;
#ifdef _DEBUG
            case 'Z':
                fprintf(stderr,"waiting for debugger...");
                while(!IsDebuggerPresent())
                    Sleep(1);
                fprintf(stderr,"done.\n");
                break;
#endif
            case 'v':
                g_verbose = 1;
                break;
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
            case 'E':
                exclude_dirs[n_exclude_dirs++] = argv[++i];
                break;
            case 'p':   // add a file to process
                strs_find_add_str(&dir_scan.files,&dir_scan.n_files,argv[++i]);
                process = 1;
                break;
            case 'Q':               // Query for something 
                query_str = argv[++i];
                c_query_flags = c_query_flags_from_str(a);
                break;
            case 'R':
                i++;
                dir_scan_dir = (i < argc && argv[i] && *argv[i] != '-')?argv[i]:".";
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

    if(dir_scan_dir)
        scan_dir(&dir_scan,dir_scan_dir,1,dirscan_accept_file,exclude_dirs);

    if(process)
    {
        printf("processing %i files\n", dir_scan.n_files);
        if(print_timers)
        {
            printf("scanning dirs took %f seconds\n", timer_diffelapsed(timer_start));
            timer_start = timer_get();
        }
        
        res += c_parse_files(cp,&dir_scan);
        res += c_on_processing_finished(cp);
    }
    
    if(query_str)
    {
        char query_buf[1024];
        char flag_buf[32];
        if(0==strcmp(query_str,"-"))
            loop_query = TRUE;

        query_timer = timer_get();
        if(c_load(cp)<0)
            return -1;
        query_timer_load = timer_diffelapsed(query_timer);
        
        do
        {
            if(loop_query)
            {
                scanf_s("%s %s",flag_buf, DIMOF(flag_buf), query_buf, DIMOF(query_buf));
                c_query_flags = c_query_flags_from_str(flag_buf);
                query_str = query_buf;
            }
            
            
            query_timer = timer_get();
            res = c_query(cp,query_str,c_query_flags);
            query_timer_query = timer_diffelapsed(query_timer);
        } while(loop_query);
    }

    if(print_timers)
    {
        printf("data load took %f.2\n"
               "query run took %f.2\n"
               "allocs took %f.2 seconds\n"
               "total process time: %f.2\n"
               ,query_timer_load
               ,query_timer_query
               ,alloc_time()
               ,timer_diffelapsed(timer_start)
            );
        locinfo_print_time();
        c_parse_print_time(cp);
    }
    return res;
}
