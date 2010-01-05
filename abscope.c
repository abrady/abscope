/***************************************************************************
 *     Copyright (c) 2009-2009, Aaron Brady
 *     All Rights Reserved
 *
 * Module Description:
 * 
 * Todos:
 * - sort locinfos
 * - fixup of refs is slowest part of load. save those
 ***************************************************************************/
#include "abutil.h"
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
		   "-t\t\t: write list of loaded tags out\n"
           "-T\t\t: run tests\n"
           "-D[pt]\t\t: debug (p)arse, (t)iming\n"
           "-Q[sSfFdecpa]\t\t: query for (s)tructs, (S)tructrefs (f)unctions (F)uncrefs (d)efines sr(c)file (e)nums Cry(p)tic (a)ll\n"
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

extern int abarray_test();

static int abscope_test()
{
    printf("TESTING\n");

    TEST(0==abarray_test());
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
        case 'v':
            c_query_flags |= CQueryFlag_Vars;
            break;
        case 'p':
            c_query_flags |= CQueryFlag_Cryptic;
            break;
        case 'a':
            c_query_flags = 0xffffffff;
            break;
        default:
            fprintf(stderr, "unknown query option %c in %s\n",*a, a_in);
            return 0;
        };
        a++;
    }
    return c_query_flags;
}

void write_parse_tags(FILE *fp, char *name, Parse *p)
{
	int i;
	fprintf(fp,"(%s (\n",name);
	for(i = 0; i < ali_size(&p->locs); ++i)
	{
		LocInfo *l = p->locs+i;
		if(!l->tag || !*l->tag)
			continue;
		fprintf(fp,"\"%s\" ",l->tag);
		if(2==i%3)
			fprintf(fp,"\n");
	}
	fprintf(fp,"))\n");
}


int main(int argc, char **argv)
{
	int write_tags = 0;
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
	int field_flags = 0;
    char *query_str = 0;
    char *exclude_dirs[128] = {0};
    int n_exclude_dirs = 0;

    abinit();

    if(argc < 2)
    {
        usage(argc,argv);
        return -1;
    }

#ifdef _DEBUG
	for(i = 0; i<argc; ++i)
	{
		if(0==strcmp(argv[i],"-Z"))
		{
			fprintf(stderr,"waiting for debugger...");
			while(!IsDebuggerPresent())
				Sleep(1);
			break_if_debugging();
			fprintf(stderr,"done.\n");
		}
		else if(0==strcmp(argv[i],"-v"))
		{
			g_verbose = 1;
		}
	}
#endif		

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
            case 't':
				write_tags = 1;
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

	if(c_load(cp)<0)
	{
		fprintf(stderr,"failed to load tags. exiting");
		return -1;
	}

	if(write_tags)
	{
		FILE *fp = fopen("tags.el","w");
		if(!fp)
		{
			fprintf(stderr,"could't open tags.el for writing, err:%s\n",last_error());
			return -1;
		}
		
		fprintf(fp,"'(\n");
		write_parse_tags(fp,"structs",&cp->structs);
//		write_parse_tags(fp,"structrefs",&cp->structrefs);
		write_parse_tags(fp,"funcs",&cp->funcs);
//		write_parse_tags(fp,"funcrefs",&cp->funcrefs);
		write_parse_tags(fp,"defines",&cp->defines);
		write_parse_tags(fp,"enums",&cp->enums);
//		write_parse_tags(fp,"vars",&cp->vars);
		write_parse_tags(fp,"srcfiles",&cp->srcfiles);
		write_parse_tags(fp,"cryptic",&cp->cryptic);
		fprintf(fp,"\n)\n");
		fclose(fp);
	}

    if(query_str)
    {
        char cmd_buf[32];
        char query_buf[1024];
        char fld_buf[32];
        char flag_buf[32];
        if(0==strcmp(query_str,"-"))
            loop_query = TRUE;

        query_timer = timer_get();
        query_timer_load = timer_diffelapsed(query_timer);
        
        do
        {
            if(loop_query) // todo: should really use a proper lexer here.
            {
                *query_buf = 0; *flag_buf = 0; *cmd_buf = 0;
                // cmd type (only one at the moment, but hey)
                if(1 != scanf_s("%s",SSTR(cmd_buf)) || 0 != strcmp("Query",cmd_buf))
                    continue;
                // cmd args
                if(3 != scanf_s("%s %s %s",SSTR(flag_buf),SSTR(fld_buf),SSTR(query_buf)))
                    continue;
                // end of command
                if(1 == scanf_s("%s",SSTR(cmd_buf)) && 0 != strcmp(cmd_buf,"End"))
                    continue;
                c_query_flags = c_query_flags_from_str(flag_buf);
                field_flags = locinfo_fields_from_str(fld_buf);
                query_str = query_buf;
            }
            
            
            query_timer = timer_get();
            res = c_query(cp,query_str,c_query_flags,field_flags);
            query_timer_query = timer_diffelapsed(query_timer);
        } while(loop_query);
    }

    if(print_timers)
    {
        printf("data load took %f.2\n"
               "query run took %f.2\n"
               "total process time: %f.2\n"
               ,query_timer_load
               ,query_timer_query
               ,timer_diffelapsed(timer_start)
            );
        locinfo_print_time();
        c_parse_print_time(cp);
    }
	c_parse_cleanup(cp);
    return res;
}
