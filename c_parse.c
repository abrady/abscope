/***************************************************************************
 *     Copyright (c) 2009-2009, Aaron Brady
 *     All Rights Reserved
 *
 * Module Description:
 * todo:
 * - build a call tree
 * - logging
 * - add 'files', 'func decls'
 * - 'reward_CalculateKillCredit' doesn't parse properly : lex 0.f as a float number
 ***************************************************************************/
#include "c_parse.h"
#include "abhash.h"
#include "abscope.h"
#include "locinfo.h"

int c_parse(CParse *ctxt);
extern int g_verbose;

int c_parse_files(CParse *cp, DirScan *scan)
{
    int i;
    int res = 0;
    for(i = 0; i < scan->n_files; ++i)
    {
        char *fn = scan->files[i];
        if(g_verbose)
            printf("%s\n",fn);
        
        if(c_ext(fn))
            res += c_parse_file(cp,fn);
    }
    return res;
}

int c_parse_file(CParse *cp, char *fn)
{
    int parse_res = 0;
    cp->parse_file = fn;
    cp->parse_line = 1;
    cp->fp = abfopen(cp->parse_file,File_R);
    
    if(!cp->fp)
    {
        printf("couldn't open file %s\n",cp->parse_file);
        return -1;
    }
    if(0 != (parse_res = c_parse(cp)))
    {
        printf("failed to parse file %s, error(%i):%s\n",cp->parse_file,parse_res,cp->parse_error);
    }
    parse_add_locinfof(&cp->srcfiles,fn,1,fn,fname_nodir(fn),fn,"file %s",fn);
    abfclose(cp->fp);
    return parse_res;
}

static void fixup_refs(Parse *c, Parse *p)
{
    int i;
    int j;
    HashTable ht = {0};
    ht.cmpfp = stricmp;
    
    hash_resize(&ht,p->n_locs*2);
    for(j = 0; j < p->n_locs; ++j)
    {
        LocInfo *q = p->locs + j;
        hash_insert(&ht,q->tag,q);
    }    

    for(i = 0; i < c->n_locs; ++i)
    {
        LocInfo *l = c->locs + i;
        LocInfo *q;
        q = hash_find(&ht,l->referrer);
        if(!q)
            continue;
        l->ref = q;
    }
    hash_cleanup(&ht,NULL);
}

static void c_do_fixups(CParse *cp)
{
    int i;
    // funcrefs  . referrer = caller
    // structrefs. referrer = type
    fixup_refs(&cp->funcrefs,&cp->funcs);
    fixup_refs(&cp->structrefs,&cp->structs);
    for(i = 0; i < cp->structs.n_locs; ++i)
    {
//        int j;
        LocInfo *s = cp->structs.locs + i;
        if(!s->child)
            continue;
        fixup_refs(s->child,&cp->structs);
//         for(j = 0; j < s->child->n_locs; ++j)
//         {
//             LocInfo *c = s->child->locs + j;
//             c->ref = s;
//         }
    }
}


int c_on_processing_finished(CParse *cp)
{   
    int res = 0;

    c_do_fixups(cp);

    printf("%i structs\n",cp->structs.n_locs);
    res += absfile_write_parse("c_structs.abs",&cp->structs);

    printf("%i structrefs\n",cp->structrefs.n_locs);
    res += absfile_write_parse("c_structrefs.abs",&cp->structrefs);

    printf("%i funcs\n",cp->funcs.n_locs);
    res += absfile_write_parse("c_funcs.abs",&cp->funcs);

    printf("%i funcrefs\n",cp->funcrefs.n_locs);
    res += absfile_write_parse("c_funcrefs.abs",&cp->funcrefs);

    printf("%i defines\n",cp->defines.n_locs);
    res += absfile_write_parse("c_defines.abs",&cp->defines);

    printf("%i enums\n",cp->enums.n_locs);
    res += absfile_write_parse("c_enums.abs",&cp->enums);

    printf("%i vars\n",cp->vars.n_locs);
    res += absfile_write_parse("c_vars.abs",&cp->vars);

    printf("%i srcfiles\n",cp->srcfiles.n_locs);
    res += absfile_write_parse("c_srcfiles.abs",&cp->srcfiles);

    return res;
}

int c_load(CParse *cp)
{
    int res = 0;
    if(file_exists("c_structs.abs"))
        res += absfile_read_parse("c_structs.abs",&cp->structs);
    if(file_exists("c_funcs.abs"))
        res += absfile_read_parse("c_funcs.abs",&cp->funcs);
    if(file_exists("c_funcrefs.abs"))
        res += absfile_read_parse("c_funcrefs.abs",&cp->funcrefs);
    if(file_exists("c_structrefs.abs"))
        res += absfile_read_parse("c_structrefs.abs",&cp->structrefs);
    if(file_exists("c_defines.abs"))
        res += absfile_read_parse("c_defines.abs",&cp->defines);
    if(file_exists("c_enums.abs"))
        res += absfile_read_parse("c_enums.abs",&cp->enums);
    if(file_exists("c_vars.abs"))
        res += absfile_read_parse("c_vars.abs",&cp->vars);
    if(file_exists("c_srcfiles.abs"))
        res += absfile_read_parse("c_srcfiles.abs",&cp->srcfiles);
    c_do_fixups(cp);
    return res;
}

#define FIND_START() S64 timer_start = timer_get()
#define FIND_END() cp->parse_timing =+ timerdiff(timer_start)

int c_findstructs(CParse *cp, char *sn)
{
    return parse_print_search_tag(&cp->structs,sn);
}

int c_findstructrefs(CParse *cp, char *sn)
{
    return parse_print_search_tag(&cp->structrefs,sn);
}

int c_findfuncs(CParse *cp, char *name)
{
    return parse_print_search_tag(&cp->funcs,name);
}

int c_findfuncrefs(CParse *cp, char *sn)
{
    return parse_print_search_tag(&cp->funcrefs,sn);
}

int c_finddefines(CParse *cp, char *sn)
{
    return parse_print_search_tag(&cp->defines,sn);
}

int c_findenums(CParse *cp, char *sn)
{
    return parse_print_search_tag(&cp->enums,sn);
}

int c_findsrcfiles(CParse *cp, char *sn)
{
    return parse_print_search_tag(&cp->srcfiles,sn);
}

int c_findvars(CParse *cp, char *sn)
{
    return parse_print_search_tag(&cp->vars,sn);
}


int c_query(CParse *cp, char *tag, int query_flags)
{
    int res = 0;
    printf("'(\n");
    if(query_flags & CQueryFlag_Structs)
        res += c_findstructs(cp,tag);
    if(query_flags & CQueryFlag_Enums)
        res += c_findenums(cp,tag);
    if(query_flags & CQueryFlag_Funcs)
        res += c_findfuncs(cp,tag);
    if(query_flags & CQueryFlag_Defines)
        res += c_finddefines(cp,tag);
    if(query_flags & CQueryFlag_Structrefs)
        res += c_findstructrefs(cp,tag);
    if(query_flags & CQueryFlag_Funcrefs)
        res += c_findfuncrefs(cp,tag);
    if(query_flags & CQueryFlag_Srcfile)
        res += c_findsrcfiles(cp,tag);
    if(query_flags & CQueryFlag_Vars)
        res += c_findvars(cp,tag);
    printf("(QUERY_DONE))\n\n");
    fflush(stdout);
    return res;
}

void c_parse_print_time(CParse *cp)
{
    printf("c_parse total %f:\n"
//           "getc took %f\n"
           ,timer_elapsed(cp->lex_timing)
//           ,timer_elapsed(cp->getc_timing)
        );
}

int c_ext(char *file)
{
    return match_ext(file,"c")
        || match_ext(file,"h");
}

// *******************************************************************
// Begin parsing block 
// *******************************************************************

typedef enum c_tokentype 
{
    PARSE_ERROR = 256,
    CHAR_LITERAL,
    TOK,
    STR,
    FUNC_HEADER,

    // expressions
//     EXPR,                
//     EXPR_CONDITIONAL,         // (a && b || a && c) ? a : b
//     EXPR_ASSIGN         // unary expr = conditionaal expr
//     EXPR_PRIMARY,        // TOK, constant, string ( expr )
//     PAREN_EXPR,  // e.g. (a,1) or (a || b)
    
    // c keywords, don't reorder!!! see INTRINSIC_TYPE macro
    TYPEDEF,
    EXTERN,
    STATIC,
    AUTO,
    REGISTER,
    CHAR_TOK,
    SHORT_TOK,
    INT_TOK,
    LONG_TOK,
    SIGNED,
    UNSIGNED,
    FLOAT_TOK,
    DOUBLE,
    VOID_TOK,
//     CONST,
//     VOLATILE,
    STRUCT,
    UNION,
    ENUM,
    ELLIPSIS,
    CASE,
    DEFAULT,
    IF,
    ELSE,
    SWITCH,
    WHILE,
    DO,
    FOR,
    GOTO,
    CONTINUE,
    BREAK,
    RETURN,
//    CONSTANT,
    SIZEOF,
    PTR_OP,
    INC_OP,
    DEC_OP,
    LEFT_OP,
    RIGHT_OP,
    LE_OP,
    GE_OP,
    EQ_OP,
    NE_OP,
    AND_OP,
    OR_OP,
    MUL_ASSIGN,
    DIV_ASSIGN,
    MOD_ASSIGN,
    ADD_ASSIGN,
    SUB_ASSIGN,
    LEFT_ASSIGN,
    RIGHT_ASSIGN,
    AND_ASSIGN,
    XOR_ASSIGN,
    OR_ASSIGN,
    TYPE_NAME,

    // not really C types
    POUND_DEFINE,
    POUND_INCLUDE,

    // cryptic src macros
    AST,
    AUTO_COMMAND,
    EIGNORE,
} c_tokentype;
#define C_KWS_START TYPEDEF
#define IS_INTRINSIC_TYPE(T) INRANGE(T,CHAR_TOK,VOID_TOK+1)
#define INTRINSIC_TYPE CHAR_TOK: case SHORT_TOK: case INT_TOK:case LONG_TOK:case SIGNED:case UNSIGNED:case FLOAT_TOK:case DOUBLE:case VOID_TOK

#define TOK_ERROR(ELT,FMT,...) parser_error(p,ELT,FMT,__VA_ARGS__)



#define PREV_TOK(A) ((p->n_stack >= 2) && top[-1].tok == A)
#define PREV_TOKS2(A,B) ((p->n_stack >= 3) && top[-2].tok == A && top[-1].tok == B)
#define PREV_TOKS3(A,B,C) ((p->n_stack >= 4) && top[-3].tok == A && top[-2].tok == B && top[-1].tok == C)

#define POP_TO(N) {                             \
        assert(INRANGE0(N,p->n_stack+1));       \
        p->n_stack = N;                         \
        if(p->n_stack < p->m_stack)             \
            p->m_stack = p->n_stack; }

#define NEXT_TOK()                                                      \
    if(p->n_stack >= MAX_STACK)                                         \
    {                                                                   \
        parser_error(p,top,"out of room on stack in %s. aborting.",__FUNCTION__); \
        break;                                                          \
    }                                                                   \
    top = get_tok(p);                                                   \
    if(!top->tok)                                                       \
        break;



typedef struct StackElt
{
    union Lex
    {
        char str[56];  // arbitrary, 32 was a little too short
        int num;
        struct Strs
        {
            char **s;
            int n;
        } strs;
        Parse locs;
    } l;
    c_tokentype tok;
    int lineno;
} StackElt;

#define MAX_STACK 256

static LocInfo* c_add_struct(CParse *p, char *struct_name, int lineno)
{
    int i = parse_add_locinfof(&p->structs,p->parse_file,lineno,p->line,struct_name,NULL,"struct %s", struct_name);
    return p->structs.locs + i;
}

static void c_add_structmbr(CParse *p, LocInfo *s, StackElt *mbr, StackElt *mbr_type)
{
    if(!s->child)
        s->child = calloc(sizeof(Parse),1);
    parse_add_locinfof(s->child,p->parse_file,mbr->lineno,p->line,mbr->l.str,mbr_type->l.str,"struct %s", s->tag);
}


static void c_add_enum_decl(CParse *p, char *enum_name, int lineno)
{
    parse_add_locinfof(&p->structs,p->parse_file,lineno,p->line,enum_name,NULL,"enum %s", enum_name);
}

static void c_add_enum(CParse *p, char *enum_name, char *enum_typename, int lineno)
{
    parse_add_locinfo(&p->enums,p->parse_file,lineno,p->line,enum_name,"0",enum_typename);  // todo: enum value
}

static void c_add_structref(CParse *p, StackElt *elt, char *type_name, char *func_ctxt)
{
    parse_add_locinfo(&p->structrefs,p->parse_file,elt->lineno,p->line,elt->l.str,type_name,func_ctxt);
}

static void c_add_funcdef(CParse *p, int lineno, char *name, char *func_line)
{
    parse_add_locinfof(&p->funcs,p->parse_file,lineno,func_line,name,NULL,"func %s",name);
}

static char* strip_type(char *s)
{
    char *t = s ? strchr(s,' ') : 0;
    if(t)
        return t + 1;
    return s;
}

static void c_add_funcref(CParse *p, StackElt *elt, char *ctxt)
{
    parse_add_locinfo(&p->funcrefs,p->parse_file,elt->lineno,p->line,elt->l.str,strip_type(ctxt),ctxt); 
}

static void c_add_define(CParse *p, char *define, int lineno)
{
    parse_add_locinfo(&p->defines,p->parse_file,lineno,p->line,define,NULL,NULL);
}

// a->b => tag = b, ref = a
static void c_add_var(CParse *p, StackElt *elt, char *ref, char *func_ctxt)
{
    parse_add_locinfo(&p->vars,p->parse_file,elt->lineno,p->line,elt->l.str,ref,func_ctxt);
}

int c_debug;
int c_lex(CParse *p, StackElt *top);

static ABINLINE StackElt* get_tok(CParse *p)
{
    StackElt *top;
    if(p->m_stack < p->n_stack)
        return p->stack + p->m_stack++;
    ++p->m_stack;
    top = p->stack + p->n_stack++;
    ZeroStruct(top);
    top->tok = c_lex(p,top);
    return top;
}

// saves existing elements above the one on the stack.
static StackElt* unget_tok(CParse *p)
{
    if(p->m_stack <= 0)
        return NULL;
    return p->stack + --p->m_stack;
}

// unget to a point on the stck
static StackElt* unget_tok_to(CParse *p, int to)
{
    StackElt*r = NULL;
    if(p->m_stack <= 0)
        return NULL;
    while(p->m_stack > to)
    {
        r = unget_tok(p);
        if(!r)
            break;
    }
    return r;
}

static void reduce_to(CParse *p, int to, StackElt *top)
{
    StackElt* r;
    POP_TO(to);
    r = unget_tok(p);
    if(r)
    {
        if(top)
            *r = *top;
        else 
            ZeroStruct(r);
    }
}

static int parser_error(CParse *p, StackElt *s, char *fmt,...)
{
    int r;
    va_list vl;
//    if(!c_debug)
//        return 0;
    fprintf(stderr,"%s(%i):",p->parse_file,DEREF(s,lineno));
    va_start(vl,fmt);
    r = vfprintf(stderr,fmt,vl);
    va_end(vl);
    fprintf(stderr,"\n");

    break_if_debugging();
    
    return r;
}


// parse until the first occurance of one of the characters in the
// passed string. leaves that token on the stack.
static ABINLINE void parse_to_chars(CParse *p, char *toks)
{
    StackElt *top = 0;
    int n_stack_in = p->n_stack;
    int n;
    int i;
    if(!DEREF(p,stack) || !toks)
        return;
    
    n = strlen(toks);
    for(;;)
    {
        NEXT_TOK();
        for(i = 0; i < n; ++i)
            if(toks[i] == top->tok)
                goto done;
    }
done:
    reduce_to(p,n_stack_in,top);
}


// matches pairing tokens like '{' and '}'
static ABINLINE void parse_to_tok(CParse *p, int tok, int open_tok)
{
    StackElt *top = 0;
    int n_open = 1;
    int n_stack_in;

    if(!DEREF(p,stack))
        return;

    n_stack_in = p->n_stack;
    while(n_open)
    {
        NEXT_TOK();
        if(top->tok == open_tok)
            n_open++;
        else if(top->tok == tok)
            n_open--;        
        POP_TO(n_stack_in);
    }
}

// expressions:
// assignment expression
// conditional expression
// expression, expression
// transition to this from:
// - expression_stmt: expression;
// -  
static void parse_expr(CParse *p, char *ctxt, char terminating_tok, char terminating_tok2)  
{
    StackElt *top = NULL;
    int n_stack_in = p->n_stack;
    
    for(;;)
    {
        NEXT_TOK();        
        if(top->tok==terminating_tok || top->tok == terminating_tok2)
            break;
        switch(top->tok)
        {
        case '(':
            if(top[-1].tok == TOK)
                c_add_funcref(p,top-1,ctxt);
            parse_expr(p,ctxt,')',0);
            break;
        case ')':
            POP_TO(n_stack_in);
            break;
        case '}':
            POP_TO(n_stack_in);
            break;
        case TOK: // keep this around
        {
            char *ref = NULL;
            if(top[-1].tok == PTR_OP || top[-1].tok == '.' && *top[-1].l.str)      // a.b or a->b
                ref = top[-1].l.str;
            c_add_var(p,top,ref,ctxt);  // add b w/ref to a
        }
        break;
        case PTR_OP:
        case '.':
            if(top[-1].tok == TOK)      // a. or a->
                stracpy(top->l.str, top[-1].l.str);
            break;
        case '?':
            // parse ? to : then keep going
            parse_expr(p,ctxt,':',0);
            break;
        case ':':
            break;
        default:
            break;
        };
    }
    reduce_to(p,n_stack_in,top);
}


static void parse_enum_body(CParse *p, char *enum_typename)
{
    StackElt *top = NULL;
    int n_stack_in = p->n_stack;
    for(;;)
    {
        NEXT_TOK();
        
        switch(top->tok)
        {
        case '}': 
        case ',':
            if(top[-1].tok == TOK)
                c_add_enum(p,top[-1].l.str,enum_typename,top[-1].lineno);
            if(top->tok == '}')
                goto done;
            POP_TO(n_stack_in);
            break;
        case '=':
            parse_expr(p,enum_typename,',','}');
            break;
        }
    }
done:
    POP_TO(n_stack_in);
}

// 
//static void extract_vars(CParse *p, StackElt *s, StackElt *e)

static void parse_initializer(CParse *p, char *ctxt)
{
    StackElt *top = NULL;
    int n_stack_in = p->n_stack;
    for(;;)
    {
        NEXT_TOK();        
        if(top->tok == ';' || top->tok == ',')
            break;
        switch(top->tok)
        {
        case '{':
            parse_to_tok(p,'}','{');
            POP_TO(n_stack_in);
            break;
        default:
            unget_tok(p);
            parse_expr(p,ctxt,',',';');
        };
    }
    reduce_to(p,n_stack_in,top);
}


static void parse_arglist(CParse *p, char *ctxt)
{
    StackElt *top = NULL;
    int n_stack_in = p->n_stack;
    for(;;)
    {
        NEXT_TOK();        
        if(top->tok == ')')
            break;
        
        switch(top->tok)
        {
        case ',':
            if(top[-1].tok == TOK)
                c_add_var(p,top-1,NULL,ctxt);
            break;
        case '(':
            if(top[-1].tok == TOK)
                c_add_funcref(p,top-1,ctxt);
            break;
        }       
    }
    POP_TO(n_stack_in);
}


// var decls:
// 1. storage  class: static auto register 
// 2. type specifier: int, char, Foo
// 3. declerator(s) : *bar, baz[10], (*fp)(params)
static void parse_var_decls(CParse *p, char *ctxt)
{
    StackElt *last_vartype = NULL;
    StackElt *last_tok = NULL;
    StackElt *top = NULL;
    int n_stack_in = p->n_stack;
    for(;;)
    {
        NEXT_TOK();        
        if(top->tok == '}')
            goto done;
        
        switch(top->tok)
        {
        case '{':
            if(top[-1].tok == '=') // struct or array init
                parse_to_tok(p,'}','{');
            else
                goto done;
            break;
        case INTRINSIC_TYPE:
        case TOK:
            if(!last_vartype)
                last_vartype = top;
            last_tok = top;
            break;
        case '=':
            parse_initializer(p,ctxt);
            break;
        case ',':                       // fall thru
        case ';': 
            if(last_tok == last_vartype) // use last_tok as Foo a[]; is valid
                goto done; // might be single stmt: foo; or ;
            if(last_vartype && last_tok)
                c_add_structref(p,last_tok,last_vartype->l.str,ctxt);

            if(top->tok == ';')
            {
                last_vartype = NULL;
                POP_TO(n_stack_in);
            }
            
            break;
        case '(':
            if(!last_vartype || last_vartype == top-1) 
                goto done; // just a function call or some error
            break;
        case '*':
            // todo: count derefs
//             if(last_vartype)
//                 stracat(last_vartype->l.str,"*");
            break;
            // statement detection, for ending var decl ability 
        case IF: case ELSE: case SWITCH: case WHILE: case DO:
        case FOR: case GOTO: case CONTINUE: case BREAK: case RETURN:
            goto done;
        }
    }
done:
    unget_tok_to(p,n_stack_in);
}

static void parse_func_body(CParse *p, char *func_ctxt)
{
    StackElt *top = NULL;
    int n_stack_in = p->n_stack;

    parse_var_decls(p,func_ctxt);
    for(;;)
    {
        NEXT_TOK();

        if(top->tok == '}')
            return;

        switch(top->tok)
        {
        case '{':
            parse_func_body(p,func_ctxt);
            POP_TO(n_stack_in);
            break;
        case '(':
            parse_expr(p,func_ctxt,')',0);
            if(top[-1].tok == TOK)
                c_add_funcref(p,top-1,func_ctxt);
            POP_TO(n_stack_in);
            break;
        case '=':
            unget_tok_to(p,n_stack_in);
            parse_expr(p,func_ctxt,'=',0);
            top = get_tok(p);
            assert(top->tok == '=');
            parse_expr(p,func_ctxt,';',0);
            POP_TO(n_stack_in);
            break;
        case ';':
            POP_TO(n_stack_in);
        default:
            // todo: if(top[-1].tok == TOK) -> add var
            break;
        }
    }
    POP_TO(n_stack_in);
}

static void parse_struct_body(CParse *p, LocInfo *struct_loc)
{
    StackElt *top = NULL;
    StackElt *first_vartype = 0;
    int n_stack_in = p->n_stack;

    for(;;)
    {
        if(p->n_stack == n_stack_in)
            first_vartype = NULL;
        
        NEXT_TOK();        
        if(top->tok == '}')
        {
            parse_to_tok(p,';',0);
            break;
        }
        switch(top->tok)
        {
        case '(':
            if(top[-1].tok == AST)
                parse_to_tok(p,')','('); // ignore for now
            POP_TO(n_stack_in);
            break;
        case ';':
            if(first_vartype && first_vartype < top-1 && top[-1].tok == TOK)
            {
                c_add_structref(p,top-1,first_vartype->l.str,struct_loc->context);
                c_add_structmbr(p,struct_loc,top-1,first_vartype);
            }
            
            POP_TO(n_stack_in);
            break;
        case '{':
            parse_to_tok(p,'}','{');
            break;
        default:
            if(!first_vartype && (top->tok == TOK || IS_INTRINSIC_TYPE(top->tok)))
                first_vartype = top;
        }
    }
    POP_TO(n_stack_in);
}


int c_parse(CParse *p)
{
    char ctxt[128];
    StackElt stack[MAX_STACK] = {0}; 
    StackElt *top;
    int res = 0;
    char *s;
//    c_debug = 1;
    p->stack = stack;
    p->m_stack = 0;
    p->n_stack = 0;
    
    for(;;)
    {
        NEXT_TOK();
        switch(top->tok)
        {
        case '{':
            if(PREV_TOKS3(TYPEDEF,STRUCT, TOK)) // struct decl
            {
                LocInfo *l;
                l = c_add_struct(p,top[-1].l.str,top[-1].lineno);
                parse_struct_body(p,l);
                POP_TO(0);
            }
            else if(PREV_TOKS2(ENUM,TOK)) // struct decl
            {
                s = top[-1].l.str;
                c_add_enum_decl(p,s,top[-1].lineno);
                parse_enum_body(p,s);
                parse_to_tok(p,';',0);
                POP_TO(0);
            }
            else if(PREV_TOK(FUNC_HEADER)) // function def
            {
                char *func_name = top[-1].l.str;

                c_add_funcdef(p,top[-1].lineno,func_name,p->last_line);
                sprintf(ctxt,"func %s",func_name);
                parse_func_body(p,ctxt);
                POP_TO(0);
            }

            break;
        case '(':
            if(PREV_TOK(TOK)) // function decl/def
            {
                StackElt hdr = {0};
                // don't care about args yet
                //parse_arglist(p,stack,n_stack);
                parse_to_tok(p,')','(');

                hdr.tok = FUNC_HEADER;
                hdr.lineno = top->lineno;
                strcpy(hdr.l.str,top[-1].l.str);

                ZeroStruct(&stack[0]);
                stack[0] = hdr;
                POP_TO(1);
            }
            else
            {
                parse_to_tok(p,')','(');
                POP_TO(0); // dunno what this is
            }            
            break;
        case ';':
            if(stack[0].tok == TOK) // global variable
                c_add_structref(p,top-1,stack[0].l.str,"global var");
            POP_TO(0);
            break;
        case '=':
            // todo: some kind of global var init
            parse_to_tok(p,';',0);
            break;
        case POUND_DEFINE:
            c_add_define(p,top->l.str, top->lineno);
            POP_TO(0);
            break;
        case POUND_INCLUDE:
            // need to fix c_lex to get the string for this
            POP_TO(0);
            break;
        default:
            break;
        };
    }
    p->stack = NULL;
    return res;
}

typedef struct KwTokPair { char *kw; int tok; } KwTokPair;
typedef struct OpTokPair { char kw[4]; int tok;} OpTokPair;    
static const KwTokPair kws[] = 
{
    { "AUTO_COMMAND",      AUTO_COMMAND },
//        { "NOCONST",           NOCONST_DECL },
//        { "const",             CONST_DECL },
    { "typedef", TYPEDEF },
    { "extern", EXTERN },
    { "static", STATIC },
    { "auto", AUTO },
    { "register", REGISTER },
    { "char", CHAR_TOK },
    { "short", SHORT_TOK },
    { "int", INT_TOK },
    { "long", LONG_TOK },
    { "signed", SIGNED },
    { "unsigned", UNSIGNED },
    { "float", FLOAT_TOK },
    { "double", DOUBLE },
//        { "const", CONST },
//        { "volatile", VOLATILE }, ignored, see ignored_kws
    { "void", VOID_TOK },
    { "struct", STRUCT },
    { "union", UNION },
    { "enum", ENUM },
    { "case", CASE },
    { "default", DEFAULT },
    { "if", IF },
    { "else", ELSE },
    { "switch", SWITCH },
    { "while", WHILE },
    { "do", DO },
    { "for", FOR },
    { "goto", GOTO },
    { "continue", CONTINUE },
    { "break", BREAK },
    { "return", RETURN },
    { "sizeof", SIZEOF },
    { "AST", AST},
};

static const OpTokPair ops[] = 
{
    { "...", ELLIPSIS },
    { "->", PTR_OP },
    { "--", DEC_OP },
    { "-=", SUB_ASSIGN },
    { "++", INC_OP },
    { "+=", ADD_ASSIGN },
    { "<<", LEFT_OP },
    { ">>", RIGHT_OP },
    { "<=", LE_OP },
    { ">=", GE_OP },
    { "==", EQ_OP },
    { "!=", NE_OP },
    { "&&", AND_OP },
    { "||", OR_OP },
    { "*=", MUL_ASSIGN },
    { "/=", DIV_ASSIGN },
    { "%=", MOD_ASSIGN },
    { "<<=", LEFT_ASSIGN },
    { ">>=", RIGHT_ASSIGN },
    { "&=", AND_ASSIGN },
    { "^=", XOR_ASSIGN },
    { "|=", OR_ASSIGN },
};

static char const *ignored_kws[] = 
{
    "ATH_ARG",
    "NN_PTR_GOOD",
    "const",
    "volatile",
    "EIGNORE"
};


// todo: parse '==' properly
int c_lex(CParse *p, StackElt *top)
{
    int c;
    char tok[4096];
    char *i;
    int j;
    int newline;
    S64 timer_start = timer_get();
//    S64 timer_getc;
// timing each getch call has too much overhead
// #define GETC() ((timer_getc = timer_get()),(p->line[(p->i_line++)%DIMOF(p->line)] = (char)(c = getc(p->fp))),(p->getc_timing += timer_diff(timer_getc)),c)

#define GETC() ((p->line[(p->i_line++)%DIMOF(p->line)] = (char)(c = abgetc(p->fp))),c)
#define UNGETC() (p->i_line--,abungetc(c, p->fp)) 
#define LEX_RET(VAL) { p->lex_timing += timer_diff(timer_start); \
        p->line[(p->i_line)%DIMOF(p->line)] = 0;           \
        return VAL;                                                 \
    }

// think of the gotos as a tail recursion, otherwise
// every comment, preprocessor, etc. would push unnecessary
// contxt onto the stack.
yylex_start:
    *tok = 0;
    i = tok;
    newline = 0;
    while ((GETC()) != EOF && isspace(c))
    {
        if(c == '\n')
        {
            p->i_line %= DIMOF(p->line);
            while(isspace(p->line[--p->i_line]))
                ; // empty
            p->line[p->i_line+1] = 0;
            p->i_line = 0;
            strcpy(p->last_line,p->line);
            p->parse_line++;
            newline = 1;
        }
    }
    top->lineno = p->parse_line;

    if(newline && c == '#')
    {
        int last = 0;
        int found_pound = 0;
        c = c_lex(p, top);

        if(!c)
            LEX_RET(0);
        
        if(c == TOK)
        {
            if(0 == strcmp(top->l.str,"define"))
                found_pound = POUND_DEFINE;
            if(0 == strcmp(top->l.str,"include"))
                found_pound = POUND_INCLUDE;
        }

        if(found_pound)
            c = c_lex(p, top);
        
        // eat preprocessor (could do in grammar, but what the hey)
        // if line ends with \, continue to eat.
        for(;;)
        {
            while((GETC()) != '\n' && c != EOF)
            {
                if(!isspace(c))
                    last = c;
            }
            p->i_line--;
            if(last == '\\')
                p->parse_line++;
            else
                break;
        }
        UNGETC();
        if(found_pound)
            LEX_RET(found_pound);
        goto yylex_start;
    }
    else if(c == '/') // take care of comments
    {
        c=GETC();
        if(c == '/')
        {
            while((GETC()) != '\n' && c != EOF)
                ; // empty
            UNGETC();
            goto yylex_start;
        }
        else if(c == '*')
        {
            for(;;)
            {
                while((GETC()) != '*' && c != EOF)
                {
                    if(c=='\n')
                        p->parse_line++;
                }
                    
                if((c=GETC()) == '/')
                    break;
                UNGETC();
            }
            goto yylex_start;
        }
        UNGETC();
        LEX_RET('/');
    }
    else if(c == '\'')     // 'a', '\'', '\\'
    {
        GETC();
        if(c == '\\')
            GETC();
        top->l.num = c;
        GETC();
        if(c != '\'')
            UNGETC(); // malformed, oh-well
        if(c_debug)
            fprintf(stderr,"CHAR_LITERAL(%c)\n",top->l.num);
        LEX_RET(CHAR_LITERAL);
    }
    else if(c == '"')
    {
        while((c=GETC()) != EOF)
        {
            if(c == '\\') // \n, \t, \\, \<newline> etc.
            {
                *i++ = (char)c;
                GETC();
                while(c != EOF && c == ' ' || c == '\t' || c == '\r' || c == '\n')
                {
                    if(c == '\n')
                        p->parse_line++;
                    GETC();
                }

                if(c == EOF)
                    break;
                else
                    *i++ = (char)c;
            }
            else if(c == '"')
                break;
            *i++ = (char)c;
        }
        *i = 0;
        stracpy(top->l.str,tok);
        if(c != '"')
            UNGETC();
        if(c_debug)
            fprintf(stderr,"STR(%s)\n",top->l.str);
        LEX_RET(STR);
    }

    // parse the token
    UNGETC();
    while(isalnum(GETC()) || c == '_')
        *i++ = (char)c;
    *i = 0;
    if(!*tok) // no characters grabbed. return
    {
        OpTokPair const *mos[DIMOF(ops)] = {0};
        int n_mos = 0;
        if(c_debug)
            fprintf(stderr,"character token(%c)\n",c);
        // special multi-char non-words
        for(j = 0; j < DIMOF(ops); ++j)
        {
            if(*ops[j].kw == c)
                mos[n_mos++] = ops + j;
        }

        // try to match operators like <<=
        for(j = 0; j < n_mos; ++j)
        {
            char const *kw = mos[j]->kw;
            int n_kw = strlen(kw);
            int k;
            for(k = 1; k < n_kw; ++k)
            {
                if(kw[k] != GETC())
                    break;
            }
            // match found
            if(k == n_kw)
                LEX_RET(mos[j]->tok);

            // unwind the extra chars fetched
            UNGETC();
            for(k--;k>0;--k)
            {
                c = kw[k];
                UNGETC();
            }
            c = kw[0];
        }

        *i++ = (char)c;
        *i++ = 0;
        LEX_RET(c == EOF?0:c);
    }
    
    UNGETC();

    for(j = 0; j < DIMOF(ignored_kws); ++j)
        if(0 == strcmp(tok,ignored_kws[j]))
            goto yylex_start;

    stracpy(top->l.str,tok);
    for(j = 0; j < sizeof(kws)/sizeof(*kws); ++j)
    {
        if(0 == strcmp(kws[j].kw,tok))
        {
            if(c_debug)
                fprintf(stderr,"keyword(%s)\n",tok);
            LEX_RET(kws[j].tok);
        }
    }
    if(c_debug)
        fprintf(stderr,"TOK(%s)\n",tok);
    LEX_RET(TOK);
}

#define TEST(COND) if(!(COND)) {fprintf(stderr,#COND ": failed\n"); break_if_debugging(); return -1;}

BOOL dirscan_accept_c_files(char *path, char **ctxt)
{
    ctxt; // ignored
    return path && (match_ext(path,"c") || match_ext(path,"h"));
}


int c_parse_test()
{
    CParse cp = {0};
    CParse cp2 = {0};
    LocInfo *li;
    LocInfo *li_end;
    LocInfo **pli;
    LocInfo **lis = NULL;
    int n_lis = 0;
    int start_line = 0;
    static char *structs_to_find[] = {
        "dirent",
        "Foo",
        "Bar",
        "Baz",
    };
    static char *structs_not_to_find[] = {
        "Cat", // typedef only
    };
    
    // ----------------------------------------
    // parse a test file
    

    TEST(0==c_parse_file(&cp2,"test/bar.c")); // scratch test environment for easy debugging. foo.c is where solid tests go
    c_do_fixups(&cp2);

    TEST(0==c_parse_file(&cp,"test/foo.c"));
    c_do_fixups(&cp);

#define INIT_LI(p) { LocInfo *l = p.locs; int n = p.n_locs; li = p.locs; li_end = l + n; }

#define TEST_LI(TAG,REF,CTXT) TEST(li < li_end);                \
    TEST(0==strcmp(li->tag,TAG));                               \
    TEST(0==strcmp(li->referrer,REF));                          \
    TEST(0==strbeginswith(li->context,CTXT));                   \
    li++;

#define TEST_LR(T,R,C) TEST(li < li_end); TEST(li->ref&&!strcmp(li->ref->tag,R)); TEST_LI(T,R,C);

    INIT_LI(cp.structrefs);
    start_line = li->lineno;
    TEST_LI("a",          "int",       "struct Foo");
    TEST(li->lineno == start_line + 1);
    TEST_LI("b",          "char",      "struct Foo");
    TEST(li->lineno == start_line + 6);
    TEST_LI("bar_a",      "int",       "struct Bar");
    TEST_LI("baz_b",      "char",      "struct Bar");
    TEST_LI("a",          "int",       "func test_func");
    TEST_LR("b",          "Foo",       "func test_func");
    TEST_LR("c",          "Bar",       "func test_func");
    TEST_LR("bar2",       "Foo",       "global var");
    TEST_LI("hNameMsg",   "Message",  "struct Foo2");
    TEST_LI("iSortID",    "U32",       "struct Foo2");
    TEST_LI("bSearchable","bool",      "struct Foo2");
    TEST_LI("eType",       "ItemType","struct Foo2");
    TEST_LR("pBar",        "Bar",      "func test_func3");
    TEST_LR("pBaz",        "Bar",      "func test_func3");
    TEST_LI("foo",         "U32",      "func test_func3");
    TEST(cp.structrefs.n_locs == 15);

    // structs
    TEST(cp.structs.n_locs == 4);
    li = cp.structs.locs + 0;
    TEST(0==strcmp(li->tag,"Foo"));
    TEST(li->referrer == NULL);
    TEST(0==strcmp(li->context,"struct Foo"));
    TEST(0==stricmp(li->file,"test/Foo.c"));
    TEST(li->child);
    INIT_LI((*li->child));
    TEST_LI("a", "int", "struct Foo");
    TEST_LI("b", "char", "struct Foo");
    TEST(li == li_end);

    li = cp.structs.locs + 1;
    TEST(0==strcmp(li->tag,"Bar"));
    TEST(0==stricmp(li->file,"test/Foo.c"));
    TEST(0==strcmp(li->context,"struct Bar"));
    TEST(li->child && li->child->n_locs == 2);
    li = li->child->locs;
    TEST_LI("bar_a", "int",  "struct Bar");
    TEST_LI("baz_b", "char", "struct Bar");

    li = cp.structs.locs + 2;
    TEST(0==strcmp(li->tag,"Baz"));
    TEST(0==strcmp(li->context,"enum Baz"));

    li++;
    TEST(0==strcmp(li->tag,"Foo2"));
    TEST(0==strcmp(li->context,"struct Foo2"));
    TEST(li->child);

    INIT_LI((*li->child));
    TEST_LI("hNameMsg", "Message",  "struct Foo2");
    TEST_LI("iSortID", "U32", "struct Foo2");
    TEST_LI("bSearchable", "bool", "struct Foo2");
    TEST_LI("eType", "ItemType", "struct Foo2");
    TEST(li == li_end);

    // func decls
    TEST(cp.funcs.n_locs >= 3);
    li = cp.funcs.locs + 0;
    TEST(0==strcmp(li->tag,"test_func"));
    TEST(0==stricmp(li->file,"test/Foo.c"));
    TEST(0==strcmp(li->context,"func test_func"))
    
    li = cp.funcs.locs + 1;
    TEST(0==strcmp(li->tag,"CommonAlgoTables_Load"));
    TEST(0==stricmp(li->file,"test/Foo.c"));
    TEST(0==strcmp(li->context,"func CommonAlgoTables_Load"));
    
    TEST(cp.defines.n_locs == 1);
    li = cp.defines.locs + 0;
    TEST(0==strcmp(li->tag,"FOO"));
    TEST(0==stricmp(li->file,"test/Foo.c"));
    TEST(NULL==li->context);
    TEST(NULL==li->referrer);
    
    TEST(cp.enums.n_locs == 3);
    li = cp.enums.locs;
    TEST(0==strcmp(li[0].tag,"Bar_A"));
    TEST(0==strcmp(li[0].context,"Baz"));
    TEST(0==strcmp(li[1].tag,"Bar_B"));    
    TEST(0==strcmp(li[1].context,"Baz"));
    TEST(0==strcmp(li[2].tag,"Bar_C"));    
    TEST(0==strcmp(li[2].context,"Baz"));

    TEST(cp.funcrefs.n_locs == 4);
    li = cp.funcrefs.locs;
    TEST(0==strcmp(li->tag,"test_foo"));
    TEST(0==strcmp(li->context ,"func test_func"));
    li++;
    TEST(0==strcmp(li->tag,"foo"));
    TEST(0==strcmp(li->context ,"func test_func2"));

    li++;
    TEST(0==strcmp(li->tag,"strcmp"));
    TEST(0==strcmp(li->referrer,"test_func2"));

    li++;
    TEST(0==strcmp(li->tag,"GET_REF"));
    TEST(0==strcmp(li->context ,"func test_func3"));

    n_lis = parse_locinfos_from_context(&cp.vars,"func test_func3",&lis);
    TEST(n_lis == 12);
    pli = lis;
    li_end = (*pli) + n_lis;
    TEST(0==strcmp(pli[0]->context,"func test_func3"));
    TEST(0==strcmp((*pli++)->tag,"pFoo"));
    TEST(0==strcmp((*pli++)->tag,"GET_REF"));
    TEST(0==strcmp((*pli++)->tag,"pFoo"));
    TEST(0==strcmp((*pli++)->tag,"hFoo"));
    TEST(0==strcmp((*pli++)->tag,"NULL"));
    TEST(0==strcmp((*pli++)->tag,"pDef"));
    TEST(0==strcmp((*pli++)->tag,"eContents"));
    TEST(0==strcmp((*pli++)->tag,"Store_All"));
    TEST(0==strcmp((*pli++)->tag,"pDef"));
    TEST(0==strcmp((*pli++)->tag,"bSellEnabled"));
    TEST(0==strcmp((*pli++)->tag,"foo"));
    TEST(0==strcmp((*pli++)->tag,"eBar"));

    INIT_LI(cp.srcfiles);
    TEST_LI("foo.c", "test/foo.c", "file test/foo.c");
    TEST(li == li_end);
    
    // TODO: do a final lineno test

    return 0;
}
