/***************************************************************************
 *     Copyright (c) 2009-2009, Aaron Brady
 *     All Rights Reserved
 *
 * Module Description:
 *
 *
 ***************************************************************************/
#include "locinfo.h"

int absfile_write_locinfos(char *fn, struct LocInfo *infos, int n_infos, StrPool *pool)
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
    strs_len = pool->end - pool->strs;          
    fwrite(&pool->strs,sizeof(pool->strs),1,fp); // 1. write start ptr
    fwrite(&strs_len,sizeof(strs_len),1,fp);     // 2. mem block size
    fwrite(pool->strs,strs_len,1,fp);            // 3. data
    
    // write infos
    fwrite(&n_infos,sizeof(n_infos),1,fp);
    fwrite(infos,sizeof(*infos),n_infos,fp);
    
    fclose(fp);
    return 0;
}

int absfile_read_locinfos(char *fn, struct LocInfo **pinfos, int *pn_infos, StrPool *pool)
{
    int res = 0;
    int nread;
    char *start;
    int32_t strdata_len = 0;
    int32_t n_strs = 0;
    FILE *fp = absfile_open_read(fn);
    int i;
    
    if(!fp || !pool)
    {
        fprintf(stderr,"couldn't open file %s to read locinfos\n",fn);
        return -1;
    }

    // read the string data
    fread(&start,sizeof(start),1,fp);             // 1. pointer
    fread(&strdata_len,sizeof(strdata_len),1,fp); // 2. data len

    if(!n_strs || !strdata_len)
    {
        fprintf(stderr,"unable to read number strs %i or data len %i",n_strs,strdata_len);
        return -2;
    }
    
    pool->strs = realloc(pool->strs,strdata_len);
    pool->end  = pool->strs + strdata_len;
    fread(pool->strs,strdata_len,1,fp);           // 3. data
    
    // read the on-file location info
    fread(pn_infos,sizeof(*pn_infos),1,fp);
    *pinfos = realloc(*pinfos,sizeof(**pinfos)*(*pn_infos));
    nread  = fread(*pinfos,sizeof(**pinfos),(*pn_infos),fp);
    if(nread != *pn_infos)
    {
        fprintf(stderr,"read %i infos, expected %i",nread,*pn_infos);
        res = -4;
        goto cleanup;
        
    }
    
    // fixup strings
    for(i = 0; i < *pn_infos; ++i)
    {
        LocInfo *info = (*pinfos)+i;
        info->tok  = pool->strs + (int)(info->tok  - start);
        info->file = pool->strs + (int)(info->file - start);
    }
    
cleanup:
    return res;
}

int locinfo_vprintf(LocInfo *li,char *fmt,va_list args)
{
    printf("** [[file:%s::%i]] %s", li->file, li->line, "(context)");
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

void add_locinfo(StrPool *pool, LocInfo **li, int *n, char *tok, char *fn, int line)
{
    LocInfo *l;
    (*li) = (LocInfo*)realloc(*li,sizeof(**li)*++(*n));
    l = (*li)+(*n)-1;
    l->tok  = strpool_find_add_str(pool,tok);
    l->file = strpool_find_add_str(pool,fn);
    l->line = line;
}
