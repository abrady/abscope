/***************************************************************************
 *     Copyright (c) 2009-2009, Aaron Brady
 *     All Rights Reserved
 *
 * Module Description:
 *
 *
 ***************************************************************************/
#include "c_parse.h"
#include "abscope.h"
#include "locinfo.h"

// from c.tab.c
extern int c_parse(Parse *ctxt);
extern int c_debug;

void add_struct_decl(Parse *ctxt, char *struct_name, int line)
{
    add_locinfo(&ctxt->pool,&ctxt->structs,&ctxt->n_structs,struct_name,ctxt->parse_file,line);
}

int c_process_file(Parse *cp, char *fn)
{
    int parse_res = 0;
    cp->parse_file = fn;
    cp->parse_line = 1;
    cp->fp = fopen(cp->parse_file,"rb");
    
    if(!cp->fp)
    {
        printf("couldn't open file %s\n",cp->parse_file);
        return -1;
    }
    if(0 != (parse_res = c_parse(cp)))
    {
        printf("failed to parse file %s, error(%i):%s\n",cp->parse_file,parse_res,cp->parse_error);
    }
    fclose(cp->fp);
    return parse_res;
}

int c_on_processing_finished(Parse *cp)
{   
    int res = 0;
    
    printf("%i structs:\n",cp->n_structs);
    res += absfile_write_locinfos("c_structs.abs",cp->structs,cp->n_structs,&cp->pool);

    return res;
}

int c_load(Parse *cp)
{
    return absfile_read_locinfos("c_structs.abs",&cp->structs,&cp->n_structs,&cp->pool);
}

int c_findstructs(Parse *cp, char *sn)
{
    int i;
    for(i = 0; i < cp->n_structs; ++i)
    {
        LocInfo *li = cp->structs+i;
        if(0 == strcmp(sn,li->tok))
            locinfo_printf(li,"%s",li->tok);
    }
    return 0;
}

int c_ext(char *file)
{
    return match_ext(file,"c")
        || match_ext(file,"h");
}
