/***************************************************************************
 *     Copyright (c) 2009-2009, Aaron Brady
 *     All Rights Reserved
 *
 * Module Description:
 *
 *
 ***************************************************************************/
#include "locinfo.h"

int absfile_write_locinfos(char *fn, struct LocInfo *infos, int n_infos)
{
    FILE *fp;
    int32_t n_strs = 0;
    char **strs = 0;
    FileLocInfo *finfos;
    int strs_len;
    int i;
    
    
    fp = absfile_open_write(fn);
    if(!fp)
    {
        fprintf(stderr,"failed to open %s for writing locinfos\n",fn);
        return -1;
    }
    
    finfos = malloc(sizeof(*finfos)*n_infos);
    
    strs_len = 0;
    n_strs = 0;
    for(i = 0; i < n_infos; ++i)
    {
        FileLocInfo *f = finfos+i;
        LocInfo *info = infos+i;
        
        f->tok = strs_find_add_str(&strs,&n_strs,info->tok);
        f->file = strs_find_add_str(&strs,&n_strs,info->file);        
        f->line = info->line;
    }
    
    for(i = 0; i < n_strs; ++i)
        strs_len += strlen(strs[i]) + 1;
    
    // write strings
    fwrite(&n_strs,sizeof(n_strs),1,fp);
    fwrite(&strs_len,sizeof(strs_len),1,fp);
    for(i = 0; i < n_strs; ++i)
    {
        int len = strlen(strs[i])+1;
        fwrite(strs[i],len,1,fp);
    }
    
    // write infos
    fwrite(&n_infos,sizeof(n_infos),1,fp);
    fwrite(finfos,sizeof(FileLocInfo),n_infos,fp);
    
    free(strs);
    free(finfos);
    fclose(fp);
    return 0;
}

int absfile_read_locinfos(char *fn, struct LocInfo **pinfos, int *pn_infos)
{
    int nread;
    char *p;
    char *strdata;
    char **strs;
    int32_t strdata_len = 0;
    int32_t n_strs = 0;
    FILE *fp = absfile_open_read(fn);
    int i;
    FileLocInfo *finfos = 0;
    
    if(!fp)
        return -1;
    fread(&n_strs,sizeof(n_strs),1,fp);
    fread(&strdata_len,sizeof(strdata_len),1,fp);
    if(!n_strs || !strdata_len)
    {
        fprintf(stderr,"unable to read number strs %i or data len %i",n_strs,strdata_len);
        return -2;
    }
    
    // read the string data
    strdata = malloc(strdata_len);
    strs = malloc(sizeof(*strs)*n_strs);
    fread(strdata,strdata_len,1,fp);
    p = strdata;
    for(i = 0;i < n_strs;++i)
    {
        char *pt;
        strs[i] = p;
        pt = strchr(p,0);
        if(!pt)
        {
            fprintf(stderr,"couldn't advance to next string at element %i, previous string %s",i,p);
            return -3;
        }
        p = pt + 1;
    }
    
    // read the on-file location info
    fread(pn_infos,sizeof(*pn_infos),1,fp);
    finfos = malloc(sizeof(*finfos)*(*pn_infos));
    nread  = fread(finfos,sizeof(*finfos),*pn_infos,fp);
    if(nread != *pn_infos)
    {
        fprintf(stderr,"read %i infos, expected %i",nread,*pn_infos);
        return -4;
    }
    
    *pinfos = realloc(*pinfos,sizeof(*pinfos)*(*pn_infos));
    for(i = 0; i < *pn_infos; ++i)
    {
        FileLocInfo *finfo = finfos + i;
        LocInfo *info = (*pinfos)+i;
        info->line = finfo->line;
        info->tok = strs[finfo->tok];
        info->file = strs[finfo->file];
    }
    
    free(strs);
    free(finfos);
    return 0;
}

int locinfo_vprintf(LocInfo *li,char *fmt,va_list args)
{
    printf("%s(%i): ", li->file, li->line);
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
