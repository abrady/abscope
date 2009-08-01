/***************************************************************************
 *     Copyright (c) 2009-2009, Aaron Brady
 *     All Rights Reserved
 *
 * Module Description:
 *
 *
 ***************************************************************************/
#include "locinfo.h"
#include "pcre.h"
#include "abserialize.h"

static S64 locinfo_timer = 0;
static S64 locinfo_read_timer = 0;
static S64 locinfo_write_timer = 0;
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


static ABINLINE int locinfo_write_eachfield(File *fp, LocInfo *l)
{
    int res = 0;
    TIMER_START();
    res += string_binwrite(fp,l->tag);
    res += string_binwrite(fp,l->referrer);
    res += string_binwrite(fp,l->context);
    res += string_binwrite(fp,l->file);
    res += int_binwrite(fp,l->lineno);
    res += string_binwrite(fp,l->line);
    TIMER_END(locinfo_write_timer);
    return res;
}

static ABINLINE int locinfo_read_eachfield(File *fp, LocInfo *l)
{
    int res = 0;
    TIMER_START();
    res += string_binread(fp,&l->tag);
    res += string_binread(fp,&l->referrer);
    res += string_binread(fp,&l->context);
    res += string_binread(fp,&l->file);
    res += int_binread(fp,&l->lineno);
    res += string_binread(fp,&l->line);
    TIMER_END(locinfo_read_timer);
    return res;
}

#define locinfo_read locinfo_read_eachfield
#define locinfo_write locinfo_write_eachfield

static int parse_write(File *fp, Parse *p)
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
        tmp.lineno   = li->lineno;
        tmp.line     = strpool_find_str(&sp,li->line);
        res += abfwrite(&tmp,sizeof(tmp),1,fp);   // 3 write locs
    }
    strpool_cleanup(&sp);
    free(strdata);
    return res;
}

static int parse_read(File *fp, Parse *p)
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
    File *fp;

    TIMER_START();

    fp = absfile_open_write(fn);
    if(!fp)
    {
        fprintf(stderr,"failed to open %s for writing locinfos\n",fn);
        return -1;
    }
    
    // write infos
    abfwrite(&p->n_locs,sizeof(p->n_locs),1,fp);
    for(i = 0; i < p->n_locs; ++i)
    {
        if(0!=locinfo_write(fp,p->locs+i))
        {
            fprintf(stderr,"failed to write loc %i to %s",i,fn);
            return -2;
        }
    }
    
    abfclose(fp);
    TIMER_END(locinfo_timer);
    return 0;
}

int absfile_read_parse(char *fn, Parse *p)
{
    int i;
    int n_alloc;
    File*fp = absfile_open_read(fn);
    TIMER_START();

    if(!fp || !p)
    {
        fprintf(stderr,"couldn't open file %s to read locinfos\n",fn);
        TIMER_END(locinfo_timer);
        return -1;
    }


    abfread(&p->n_locs,sizeof(p->n_locs),1,fp);
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

void locinfo_print(LocInfo *li, char *in)
{
    char *referrer = li->referrer ? li->referrer : li->tag;
    char *ctxt = li->context?li->context:"";
    if(!in)
        in = "";
    
    printf("%sLocInfo\n"
              "%s\tFile %s\n"
              "%s\tLineno %i\n"
              "%s\tLine %s\n"
              "%s\tTag %s\n"  
              "%s\tRefName  %s\n"
              "%s\tCtxt %s\n"
           ,in, in,li->file, in,li->lineno, in,li->line, in,li->tag, in,referrer, in,ctxt);
    if(li->ref && 0)
    {
        char tmp[128];
        printf("%s\tRef ",in);
        sprintf_s(SSTR(tmp),"%s\t",in);
        locinfo_print(li->ref,tmp);
    }
    printf("%sEnd\n",in);
}


// int locinfo_vprintf(LocInfo *li,char *fmt,va_list args)
// {
//     locinfo_print(li);
//     return vprintf(fmt,args);
// }

// int locinfo_printf(LocInfo *li,char *fmt,...)
// {
//     int r;
//     va_list vl;
//     va_start(vl,fmt);
//     r = locinfo_vprintf(li,fmt,vl);
//     va_end(vl);
//     return r;
// }

static char* parse_find_add_str(Parse *p, char *s)
{
    char *r = NULL;
    TIMER_START();
    if(s)
    {
        // dup strings is okay, get more speed this way
        r = _strdup(s); // strpool_find_add_str(&p->strs,s);
        p;
        str_replacechar(r,'\n',' ');
    }
    TIMER_END(locinfo_parse_find_add_str_timer);
    return r;
}

int parse_add_locinfo(Parse *p,char *filename, int lineno, char *line, char *tag, char *referrer, char *context)
{
    if(context)
        return parse_add_locinfof(p,filename,lineno,line,tag,referrer,"%s",context);
    return parse_add_locinfof(p,filename,lineno,line,tag,referrer,NULL);
}


int parse_add_locinfof(Parse *p,char *filename, int lineno, char *line, char *tag, char *referrer, char *context, ...)
{
    int r;
    va_list vl;
    va_start(vl,context);
    r = parse_add_locinfov(p,filename,lineno,line,tag,referrer,context,vl);
    va_end(vl);
    return r;
}

int parse_add_locinfov(Parse *p,char *filename, int lineno, char *line, char *tag, char *referrer, char *context_in,va_list args)
{
    char buf[128];
    char *ctxt = context_in;
    LocInfo *l;
    TIMER_START();
    if(ctxt)
    {
        vsnprintf(buf,DIMOF(buf),ctxt,args);
        buf[DIMOF(buf)-1] = 0;
        ctxt = buf;
    }
    
    p->locs    = realloc(p->locs,sizeof(*p->locs)*(++p->n_locs));
    l          = p->locs+p->n_locs-1;
    ZeroStruct(l);
    l->tag     = parse_find_add_str(p,tag);
    l->referrer= parse_find_add_str(p,referrer);
    l->context = parse_find_add_str(p,ctxt);
    l->file    = parse_find_add_str(p,filename);
    l->lineno  = lineno;
    l->line    = parse_find_add_str(p,line);

    TIMER_END(locinfo_timer);
    return l - p->locs;
}


int parse_print_search_tag(Parse *p,char *tag)
{
    char *err_str = 0;
    int err_val = 0;
    pcre *re;
    int res = 0;
    int i;
    int matches[10];
    TIMER_START();

    re = pcre_compile(tag, PCRE_CASELESS, &err_str, &err_val, NULL);
    if(!re)
    {
        if(err_str)
            pcre_free(err_str);
        return -1;
    }

    for(i = 0; i < p->n_locs; ++i)
    {
        LocInfo *li = p->locs+i; 
#define TAG_MATCH(T) (T && pcre_exec(re,NULL,T,strlen(T),0,0,matches,DIMOF(matches))>=0)
        if(TAG_MATCH(li->tag) || TAG_MATCH(li->referrer))
        {
            res++;
            locinfo_print(li,NULL);
        }
#undef TAG_MATCH
    }
    TIMER_END(locinfo_timer);

    pcre_free(re);
    return res;
}

void locinfo_print_time()
{
    printf("locinfo total:\t\t\t%f\n"
           "\tfind_add_str:\t\t\t%f\n"
           "\tlocinfo_read:\t\t\t%f\n"
           "\tlocinfo_write:\t\t\t%f\n"
           ,timer_elapsed(locinfo_timer)
           ,timer_elapsed(locinfo_parse_find_add_str_timer)
           ,timer_elapsed(locinfo_read_timer)
           ,timer_elapsed(locinfo_write_timer)
        );
}

void parse_cleanup(Parse *p)
{
    int i;
    for(i = 0; i<p->n_locs; ++i)
        locinfo_cleanup(p->locs+i);
    free(p->locs);
}


static int parse_locinfos_from_str_field(Parse *p, char *s, int off, LocInfo ***res)
{
    int n = 0;
    int i;
    if(!p || !res || !s || !INRANGE0(off,sizeof(LocInfo)))
        return 0;
    
    for(i = 0; i<p->n_locs; ++i)
    {
        LocInfo *li = p->locs + i;
        char *s_li = *(char**)OFFSET_PTR(li,off);
        if(s_li && 0 == stricmp(s_li,s))
        {
            (*res) = realloc((*res),++n*sizeof(*res));
            (*res)[n-1] = li;
        }
    }
    return n;
}

int parse_locinfos_from_ref(Parse *p, char *ref, LocInfo ***res)
{
    return parse_locinfos_from_str_field(p,ref,MBR_OFFSET(LocInfo,referrer),res);
}

int parse_locinfos_from_context(Parse *p, char *ref, LocInfo ***res)
{
    return parse_locinfos_from_str_field(p,ref,MBR_OFFSET(LocInfo,context),res);
}


#define TEST(COND) if(!(COND)) {printf("%s(%d):"#COND ": failed\n",__FILE__,__LINE__); break_if_debugging(); return -1;}

int test_locinfo(void)
{
    Parse p = {0}; 
    Parse p2 = {0}; 
    printf("testing locinfo...");
    parse_add_locinfo(&p,"filename",0xaabbccdd,"line0","foo","bar","baz");
    parse_add_locinfo(&p,"fn2",0xaabbccee,"line1","alpha","beta","delta");
    TEST(0==absfile_write_parse("test.absfile",&p));
    TEST(0==absfile_read_parse("test.absfile",&p2));
    TEST(p2.n_locs == 2);

    TEST(0==strcmp(p2.locs[0].tag,"foo"));
    TEST(0==strcmp(p2.locs[0].referrer,"bar"));
    TEST(0==strcmp(p2.locs[0].context,"baz"));
    TEST(0==strcmp(p2.locs[0].file,"filename"));
    TEST(p2.locs[0].lineno == 0xaabbccdd);
    TEST(0==strcmp(p2.locs[0].line,"line0"));

    TEST(0==strcmp(p2.locs[1].tag,"alpha"));
    TEST(0==strcmp(p2.locs[1].referrer,"beta"));
    TEST(0==strcmp(p2.locs[1].context,"delta"));
    TEST(0==strcmp(p2.locs[1].file,"fn2"));
    TEST(p2.locs[1].lineno == 0xaabbccee);
    TEST(0==strcmp(p2.locs[1].line,"line1"));

    printf("done.\n");
    parse_cleanup(&p);
    parse_cleanup(&p2);
    return 0;
}

