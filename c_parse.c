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
extern int c_parse(CParse *ctxt);
extern int c_debug;

void c_add_struct(CParse *ctxt, char *struct_name, int line)
{
    parse_add_locinfo(&ctxt->structs,struct_name,NULL,ctxt->parse_file,line);
}

int c_parse_file(CParse *cp, char *fn)
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

int c_on_processing_finished(CParse *cp)
{   
    int res = 0;
    
    printf("%i structs\n",cp->structs.n_locs);
    res += absfile_write_parse("c_structs.abs",&cp->structs);

    printf("%i funcs\n",cp->funcs.n_locs);
    res += absfile_write_parse("c_funcs.abs",&cp->funcs);

    return res;
}

int c_load(CParse *cp)
{
    int res = 0;
    if(file_exists("c_structs.abs"))
        res += absfile_read_parse("c_structs.abs",&cp->structs);
    return res;
}

#define FIND_START() S64 timer_start = timer_get()
#define FIND_END() cp->parse_timing =+ timerdiff(timer_start)

int c_findstructs(CParse *cp, char *sn)
{
    return parse_print_search_tag(&cp->structs,sn);
}

int c_ext(char *file)
{
    return match_ext(file,"c")
        || match_ext(file,"h");
}

void c_add_func(CParse *ctxt, char *name, int line)
{
    parse_add_locinfo(&ctxt->funcs,name,NULL,ctxt->parse_file,line);
}

int c_findfuncs(CParse *cp, char *name)
{
    return parse_print_search_tag(&cp->funcs,name);
}

int c_query(CParse *cp, char *tag, int query_flags)
{
    int res = 0;
    if(query_flags & CQueryFlag_Struct)
        res += c_findstructs(cp,tag);
    if(query_flags & CQueryFlag_Func)
        res += c_findfuncs(cp,tag);
    return res;
}

void c_parse_print_time(CParse *cp)
{
    printf("lexing took %f overall:\n"
           "getc took %f\n",
           timer_elapsed(cp->lex_timing), 
           timer_elapsed(cp->getc_timing));
}
