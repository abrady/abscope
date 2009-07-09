/***************************************************************************
 *     Copyright (c) 2009-2009, Aaron Brady
 *     All Rights Reserved
 *
 * Module Description:
 *
 *
 ***************************************************************************/
#include "locinfo.h"
#include "abserialize.h"

static S64 locinfo_timer = 0;
static S64 locinfo_read_timer = 0;
static S64 locinfo_parse_find_add_str_timer = 0;
#define LOCINFO_TIMER_START TIMER_START
#define LOCINFO_TIMER_END() TIMER_END(locinfo_timer)

static void locinfo_cleanup(LocInfo *l)
{
    free(l->tag);
    free(l->referrer);
    free(l->context);
    free(l->file);
    ZeroStruct(l);
}


static ABINLINE int locinfo_write(FILE *fp, LocInfo *l)
{
    int res = 0;
    res += string_binwrite(fp,l->tag);
    res += string_binwrite(fp,l->referrer);
    res += string_binwrite(fp,l->context);
    res += string_binwrite(fp,l->file);
    res += int_binwrite(fp,l->line);
    return res;
}

static ABINLINE int locinfo_read(FILE *fp, LocInfo *l)
{
    int res = 0;
    TIMER_START();
    res += string_binread(fp,&l->tag);
    res += string_binread(fp,&l->referrer);
    res += string_binread(fp,&l->context);
    res += string_binread(fp,&l->file);
    res += int_binread(fp,&l->line);
    TIMER_END(locinfo_read_timer);
    return res;
}

static int parse_write(FILE *fp, Parse *p)
{
    char *d;
    char *strdata;
    int n_strdata;
    StrPool sp = {0};
    int i;
    int res = 0;
    
    if(!p)
        return 0;

    // ----------------------------- 
    // build contiguous strs block.

    n_strdata = 0;
    for(i = 0; i < p->strs.n_strs && 0==res; ++i)
    {
        char *s = p->strs.strs[i];
        int n = strlen(s) + 1;
        n_strdata += n;
    }
    strdata = malloc(n_strdata);
    d = strdata;
    for(i = 0; i < sp.n_strs && 0==res; ++i)
    {
        char *s = sp.strs[i];
        int n = strlen(s) + 1;
        memmove(d,s,n);
        strpool_find_add_str(&sp,d);
        d+= n;
    }

    res += ptr_binwrite(fp,strdata);           // 0 write strs addr
    res += mem_binwrite(fp,strdata,d-strdata); // 1 write strs data 

    res += int_binwrite(fp,p->n_locs);         // 2 write num locs
    for(i = 0; i < p->n_locs; ++i)
    {
        LocInfo *li = p->locs + i;
        LocInfo tmp = {0};
        tmp.tag      = strpool_find_str(&sp,li->tag);
        tmp.referrer = strpool_find_str(&sp,li->referrer);
        tmp.context  = strpool_find_str(&sp,li->context);
        tmp.file     = strpool_find_str(&sp,li->file);
        tmp.line     = li->line;
        res += fwrite(&tmp,sizeof(tmp),1,fp);   // 3 write locs
    }
    strpool_cleanup(&sp);
    free(strdata);
    return res;
}

static int parse_read(FILE *fp, Parse *p)
{
    int i;
    int n;
    intptr_t strs_start;
    char *strdata = NULL;
    char *strdata_end;
    int res = 0;
    
    res += ptr_binread(fp,&strs_start);         // 0 read start address
    res += mem_binread(fp,&strdata,&n);         // 1 read str length
    strdata_end = strdata + n;
    
    res += mem_binread(fp,&p->locs,&p->n_locs); // 2,3 read numlocs and locs

    strpool_add_strblock(&p->strs,strdata,strdata_end);

    for(i = 0; i<p->n_locs; ++i)
    {
        LocInfo *li = p->locs + i;
        li->tag = strdata + (int)(li->tag - strs_start);
        li->referrer = strdata + (int)(li->referrer - strs_start);
        li->context = strdata + (int)(li->context - strs_start);
        li->file = strdata + (int)(li->file - strs_start);
    }
}


int absfile_write_parse(char *fn, Parse *p)
{
    int i;
    FILE *fp;

    TIMER_START();

    fp = absfile_open_write(fn);
    if(!fp)
    {
        fprintf(stderr,"failed to open %s for writing locinfos\n",fn);
        return -1;
    }
    
    // write infos
    fwrite(&p->n_locs,sizeof(p->n_locs),1,fp);
    for(i = 0; i < p->n_locs; ++i)
    {
        if(0!=locinfo_write(fp,p->locs+i))
        {
            fprintf(stderr,"failed to write loc %i to %s",i,fn);
            return -2;
        }
    }
    
    fclose(fp);
    TIMER_END(locinfo_timer);
    return 0;
}

int absfile_read_parse(char *fn, Parse *p)
{
    int i;
    int n_alloc;
    FILE *fp = absfile_open_read(fn);
    TIMER_START();

    if(!fp || !p)
    {
        fprintf(stderr,"couldn't open file %s to read locinfos\n",fn);
        TIMER_END(locinfo_timer);
        return -1;
    }


    fread(&p->n_locs,sizeof(p->n_locs),1,fp);
    n_alloc = sizeof(*p->locs)*p->n_locs;
    p->locs = realloc(p->locs,n_alloc);
    ZeroStructs(p->locs,p->n_locs);
    for(i = 0; i < p->n_locs; ++i)
    {
        LocInfo *l = p->locs + i;
        if(locinfo_read(fp,l) < 0)
        {
            fprintf(stderr,"couldn't read locinfo %i from %s",i,fn);
            TIMER_END(locinfo_timer);
            return -1;
        }
        
    }


    TIMER_END(locinfo_timer);
    return 0;
}

void locinfo_print(LocInfo *li)
{
    char *referrer = li->referrer ? li->referrer : li->tag;
    char *ctxt = li->context?li->context:"";
    printf("** [[file:%s::%i][%s]] %s\n", li->file, li->line, referrer, ctxt);
}


int locinfo_vprintf(LocInfo *li,char *fmt,va_list args)
{
    locinfo_print(li);
    return vprintf(fmt,args);
}

int locinfo_printf(LocInfo *li,char *fmt,...)
{
    int r;
    va_list vl;
    va_start(vl,fmt);
    r = locinfo_vprintf(li,fmt,vl);
    va_end(vl);
    return r;
}

static char* parse_find_add_str(Parse *p, char *s)
{
    TIMER_START();
    char *r = strpool_find_add_str(&p->strs,s);
    TIMER_END(locinfo_parse_find_add_str_timer);
    return r;
}

int parse_add_locinfo(Parse *p,char *filename, int line, char *tag, char *referrer, char *context)
{
    return parse_add_locinfof(p,filename,line,tag,referrer,"%s",context);
}


int parse_add_locinfof(Parse *p,char *filename, int line, char *tag, char *referrer, char *context,...)
{
    int r;
    va_list vl;
    va_start(vl,context);
    r = parse_add_locinfov(p,filename,line,tag,referrer,context,vl);
    va_end(vl);
    return r;
}

int parse_add_locinfov(Parse *p,char *filename, int line, char *tag, char *referrer, char *context,va_list args)
{
    char buf[128];
    LocInfo *l;
    TIMER_START();
    *buf = 0;
    if(context)
    {
        vsnprintf(buf,DIMOF(buf),context,args);
        buf[DIMOF(buf)-1] = 0;
    }
    
    p->locs    = realloc(p->locs,sizeof(*p->locs)*(++p->n_locs));
    l          = p->locs+p->n_locs-1;
    l->tag     = parse_find_add_str(p,tag);
    l->referrer= parse_find_add_str(p,referrer);
    l->context = parse_find_add_str(p,buf);
    l->file    = parse_find_add_str(p,filename);
    l->line    = line;

    TIMER_END(locinfo_timer);
    return l - p->locs;
}


int parse_print_search_tag(Parse *p,char *tag)
{
    int res = 0;
    int i;
    TIMER_START();
    for(i = 0; i < p->n_locs; ++i)
    {
        LocInfo *li = p->locs+i;
        if(0 == stricmp(tag,li->tag))
        {
            res++;
            locinfo_print(li);
        }
    }
    TIMER_END(locinfo_timer);
    return res;
}

void locinfo_print_time()
{
    printf("locinfo total:\t\t\t%f\n"
           "\tfind_add_str:\t\t\t%f\n"
           "\tlocinfo_read:\t\t\t%f\n"
           ,timer_elapsed(locinfo_timer)
           ,timer_elapsed(locinfo_parse_find_add_str_timer)
           ,timer_elapsed(locinfo_read_timer)
        );
}

void parse_copy_parse(Parse *dst, Parse *src)
{
    int i;
    if(!dst || !src)
        return;
    for(i = 0; i < src->n_locs; ++i)
        parse_add_locinfof(dst,src->locs[i].file,src->locs[i].line,src->locs[i].tag,src->locs[i].referrer,src->locs[i].context);
}

void parse_cleanup(Parse *p)
{
    int i;
    for(i = 0; i<p->n_locs; ++i)
        locinfo_cleanup(p->locs+i);
    free(p->locs);
}

#define TEST(COND) if(!(COND)) {printf("%s(%d):"#COND ": failed\n",__FILE__,__LINE__); break_if_debugging(); return -1;}

int test_locinfo(void)
{
    Parse p = {0}; 
    Parse p2 = {0}; 
    printf("testing locinfo...");
    parse_add_locinfo(&p,"filename",0xaabbccdd,"foo","bar","baz");
    parse_add_locinfo(&p,"fn2",0xaabbccee,"alpha","beta","delta");
    TEST(0==absfile_write_parse("test.absfile",&p));
    TEST(0==absfile_read_parse("test.absfile",&p2));
    TEST(p2.n_locs == 2);
    TEST(0==strcmp(p2.locs[0].tag,"foo"));
    TEST(0==strcmp(p2.locs[0].referrer,"bar"));
    TEST(0==strcmp(p2.locs[0].context,"baz"));
    TEST(0==strcmp(p2.locs[0].file,"filename"));
    TEST(p2.locs[0].line == 0xaabbccdd);

    TEST(0==strcmp(p2.locs[1].tag,"alpha"));
    TEST(0==strcmp(p2.locs[1].referrer,"beta"));
    TEST(0==strcmp(p2.locs[1].context,"delta"));
    TEST(0==strcmp(p2.locs[1].file,"fn2"));
    TEST(p2.locs[1].line == 0xaabbccee);
    printf("done.\n");
    parse_cleanup(&p);
    parse_cleanup(&p2);
    return 0;
}
