/***************************************************************************
 *     Copyright (c) 2009-2009, Aaron Brady
 *     All Rights Reserved
 *
 * Module Description:
 *
 *
 ***************************************************************************/
#include "locinfo.h"

static S64 locinfo_timer = 0;
static S64 locinfo_fixup_strs_timer = 0;
static S64 locinfo_parse_find_add_str_timer = 0;

static void fixup_strs(Parse *p,char *old_base)
{
    int i;
    TIMER_START();
    for(i = 0; i < p->n_locs; ++i)
    {
        LocInfo *l = p->locs + i;
        l->tag = l->tag?(p->pool.strs + (int)(l->tag - old_base)):NULL;
        l->referrer = l->referrer?(p->pool.strs + (int)(l->referrer - old_base)):NULL;
        l->context = l->context?(p->pool.strs + (int)(l->context - old_base)):NULL;
        l->file = l->file?(p->pool.strs + (int)(l->file - old_base)):NULL;
    }
    TIMER_END(locinfo_fixup_strs_timer);
}


int absfile_write_parse(char *fn, Parse *p)
{
    FILE *fp;
    int32_t strs_len = 0;
    TIMER_START();

    fp = absfile_open_write(fn);
    if(!fp)
    {
        fprintf(stderr,"failed to open %s for writing locinfos\n",fn);
        return -1;
    }
    
    // write strings
    strs_len = p->pool.end - p->pool.strs;          
    fwrite(&p->pool.strs,sizeof(p->pool.strs),1,fp); // 1. write start ptr
    fwrite(&strs_len,sizeof(strs_len),1,fp);     // 2. mem block size
    fwrite(p->pool.strs,strs_len,1,fp);            // 3. data
    
    // write infos
    fwrite(&p->n_locs,sizeof(p->n_locs),1,fp);
    fwrite(p->locs,sizeof(*p->locs),p->n_locs,fp);
    
    fclose(fp);
    TIMER_END(locinfo_timer);
    return 0;
}

int absfile_read_parse(char *fn, Parse *p)
{
    int res = 0;
    int nread;
    int n_alloc;
    char *pool_base;
    int32_t strdata_len = 0;
    FILE *fp = absfile_open_read(fn);
    TIMER_START();

    if(!fp || !p)
    {
        fprintf(stderr,"couldn't open file %s to read locinfos\n",fn);
        return -1;
    }

    // read the string data
    fread(&pool_base,sizeof(pool_base),1,fp);             // 1. pointer
    fread(&strdata_len,sizeof(strdata_len),1,fp); // 2. data len

    if(!pool_base || !strdata_len)
    {
        fprintf(stderr,"warning: no locations read from file %s\n",fn);
        return 0;
    }
    
    p->pool.strs = realloc(p->pool.strs,strdata_len);
    p->pool.end  = p->pool.strs + strdata_len;
    fread(p->pool.strs,strdata_len,1,fp);           // 3. data
    
    // read the on-file location info
    fread(&p->n_locs,sizeof(p->n_locs),1,fp);
    n_alloc = sizeof(*p->locs)*p->n_locs;
    p->locs = realloc(p->locs,n_alloc);
    nread  = fread(p->locs,n_alloc,1,fp);
    if(nread != 1)
    {
        fprintf(stderr,"read %i infos, expected %i",nread,p->n_locs);
        res = -4;
        goto cleanup;
        
    }
    fixup_strs(p,pool_base);
    
cleanup:
    TIMER_END(locinfo_timer);
    return res;
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
    char *old_base = p->pool.strs;
    char *r = strpool_find_add_str(&p->pool,s);
    if(p->pool.strs != old_base)   // if we realloc'd, fixup.
        fixup_strs(p,old_base);
    TIMER_END(locinfo_parse_find_add_str_timer);
    return r;
}

LocInfo *parse_add_locinfo(Parse *p,char *filename, int line, char *tag, char *referrer, char *context)
{
    return parse_add_locinfof(p,filename,line,tag,referrer,"%s",context);
}


LocInfo *parse_add_locinfof(Parse *p,char *filename, int line, char *tag, char *referrer, char *context,...)
{
    LocInfo *r;
    va_list vl;
    va_start(vl,context);
    r = parse_add_locinfov(p,filename,line,tag,referrer,context,vl);
    va_end(vl);
    return r;
}

LocInfo *parse_add_locinfov(Parse *p,char *filename, int line, char *tag, char *referrer, char *context,va_list args)
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
    return l;
}


int parse_print_search_tag(Parse *p,char *tag)
{
    int res = 0;
    int i;
    TIMER_START();
    for(i = 0; i < p->n_locs; ++i)
    {
        LocInfo *li = p->locs+i;
        if(0 == strcmp(tag,li->tag))
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
    printf("locinfo\ntotal:\t\t%f\n"
           "find_add_str:\t\t%f\n"
           "fixup_strs:\t\t%f\n",
           timer_elapsed(locinfo_timer),
           timer_elapsed(locinfo_parse_find_add_str_timer),
           timer_elapsed(locinfo_fixup_strs_timer));
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
    free(p->locs);
    strpool_cleanup(&p->pool);
}
