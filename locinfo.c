/***************************************************************************
 *     Copyright (c) 2009-2009, Aaron Brady
 *     All Rights Reserved
 *
 * Module Description:
 *
 *
 ***************************************************************************/
#include "locinfo.h"

static void fixup_strs(Parse *p,char *old_base)
{
    int i;
    for(i = 0; i < p->n_locs; ++i)
    {
        LocInfo *l = p->locs + i;
        l->tag = p->pool.strs + (int)(l->tag - old_base);
        l->context = p->pool.strs + (int)(l->context - old_base);
        l->file = p->pool.strs + (int)(l->file - old_base);
    }
}


int absfile_write_parse(char *fn, Parse *p)
{
    FILE *fp;
    int32_t strs_len = 0;
    
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
    return 0;
}

int absfile_read_parse(char *fn, Parse *p)
{
    int res = 0;
    int nread;
    char *pool_base;
    int32_t strdata_len = 0;
    FILE *fp = absfile_open_read(fn);
    
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
        fprintf(stderr,"unable to read pool_base pointer %p or data len %i",pool_base,strdata_len);
        return -2;
    }
    
    p->pool.strs = realloc(p->pool.strs,strdata_len);
    p->pool.end  = p->pool.strs + strdata_len;
    fread(p->pool.strs,strdata_len,1,fp);           // 3. data
    
    // read the on-file location info
    fread(&p->n_locs,sizeof(p->n_locs),1,fp);
    p->locs = realloc(p->locs,sizeof(*p->locs)*p->n_locs);
    nread  = fread(p->locs,sizeof(*p->locs),p->n_locs,fp);
    if(nread != p->n_locs)
    {
        fprintf(stderr,"read %i infos, expected %i",nread,p->n_locs);
        res = -4;
        goto cleanup;
        
    }
    fixup_strs(p,pool_base);
    
cleanup:
    return res;
}

void locinfo_print(LocInfo *li)
{
    printf("** [[file:%s::%i]] %s", li->file, li->line, li->tag, li->context?li->context:"");
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
    char *old_base = p->pool.strs;
    char *r = strpool_find_add_str(&p->pool,s);
    if(p->pool.strs != s)   // if we realloc'd, fixup.
        fixup_strs(p,old_base);
    return r;
}

void parse_add_locinfo(Parse *p,char *tag, char *context, char *filename, int line)
{
    LocInfo *l;
    p->locs    = realloc(p->locs,sizeof(*p->locs)*(++p->n_locs));
    l          = p->locs+p->n_locs-1;
    l->tag     = parse_find_add_str(p,tag);
    l->context = parse_find_add_str(p,context);
    l->file    = parse_find_add_str(p,filename);
    l->line    = line;
}


int parse_print_search_tag(Parse *p,char *tag)
{
    int res = 0;
    int i;
    for(i = 0; i < p->n_locs; ++i)
    {
        LocInfo *li = p->locs+i;
        if(0 == strcmp(tag,li->tag))
        {
            res++;
            locinfo_print(li);
        }
    }
    return res;
}

