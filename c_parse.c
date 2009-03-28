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

// from c.tab.c
extern int c_parse(struct CParse *ctxt);
extern int c_debug;



void add_locinfo(CParse *ctxt, LocInfo **li, int *n, char *tok)
{
    LocInfo *l;
    (*li) = (LocInfo*)realloc(*li,sizeof(**li)*++(*n));
    l = (*li)+(*n)-1;
    l->tok = _strdup(tok);
    l->file = ctxt->parse_file;
    l->line = ctxt->parse_line;
}

void add_struct_decl(struct CParse *ctxt, char *struct_name) 
{
    add_locinfo(ctxt,&ctxt->structs,&ctxt->n_structs,struct_name);
}

int c_process_file(struct CParse *cp, char *fn)
{
    int parse_res;
    cp->parse_file = fn;
    cp->parse_line = 1;
    cp->fp = fopen(cp->parse_file,"rb");
    
    if(!cp->fp)
    {
        printf("couldn't open file %s\n",cp->parse_file);
        return 1;
    }
    if(0 != (parse_res = c_parse(cp)))
    {
        printf("failed to parse file %s, error %i\n",cp->parse_file,parse_res);
        return parse_res;
    }
    return 0;
}

int c_on_processing_finished(struct CParse *cp)
{   
    int res = 0;
    
    printf("%i structs:\n",cp->n_structs);
    res += absfile_write_locinfos("c_structs.abs",cp->structs,cp->n_structs);

    return res;
}

int c_load(struct CParse *cp)
{
    int res = 0;
    absfile_read_locinfos("c_structs.abs",&cp->structs,&cp->n_structs);
    return res;
}


int c_findstructs(CParse *cp, char *sn)
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
