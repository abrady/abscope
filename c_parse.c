/***************************************************************************
 *     Copyright (c) 2009-2009, Aaron Brady
 *     All Rights Reserved
 *
 * Module Description:
 * todo:
 * - fast load
 * - build a call tree
 * - logging
 * - add 'files', 'func decls'
 * - 'reward_CalculateKillCredit' doesn't parse properly : lex 0.f as a float number
 ***************************************************************************/
#include "c_parse.h"
#include "abscope.h"
#include "locinfo.h"

int c_parse(CParse *ctxt);

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

int c_findsrcfile(CParse *cp, char *sn)
{
    int res = 0;
    int i;
    AvlTree t = {0};
    Parse *ps[128];
    int n = 0;
    ps[n++] = &cp->structs;
    ps[n++] = &cp->structrefs;
    ps[n++] = &cp->funcs;
    ps[n++] = &cp->funcrefs;
    ps[n++] = &cp->defines;
    ps[n++] = &cp->enums;
    ps[n++] = &cp->vars;
    for(i = 0; i<n; ++i)
    {
        int j;
        for(j = 0; j<ps[i]->n_locs; ++j)
        {
            LocInfo *li = ps[i]->locs + j;
            char *fn = fname_nodir(li->file);
            if(0 == stricmp(fn,sn) && !avltree_find(&t,li->file))
            {
                LocInfo tmp = {0};
                tmp.file = li->file;
                tmp.tag = fname_nodir(li->file);
                tmp.referrer = tmp.tag;
                tmp.lineno = 1; 
                tmp.line = tmp.file;
                locinfo_print(&tmp);
                avltree_insert(&t,li->file);
                res++;
            }
        }
    }
    
    avltree_cleanup(&t,0);
    return res;
}



int c_query(CParse *cp, char *tag, int query_flags)
{
    int res = 0;
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
        res += c_findsrcfile(cp,tag);
    if(query_flags & CQueryFlag_Vars)
        res += parse_print_search_tag(&cp->vars,tag);
    printf("QUERY_DONE\n\n");
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
//     EXPR,                // assign expr*
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
} c_tokentype;
#define C_KWS_START TYPEDEF
#define IS_INTRINSIC_TYPE(T) INRANGE(T,CHAR_TOK,DOUBLE+1)


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

static void c_add_struct(CParse *ctxt, char *struct_name, int lineno)
{
    parse_add_locinfof(&ctxt->structs,ctxt->parse_file,lineno,ctxt->line,struct_name,NULL,"struct %s", struct_name);
}

static void c_add_enum_decl(CParse *ctxt, char *enum_name, int lineno)
{
    parse_add_locinfof(&ctxt->structs,ctxt->parse_file,lineno,ctxt->line,enum_name,NULL,"enum %s", enum_name);
}

static void c_add_enum(CParse *ctxt, char *enum_name, char *enum_typename, int lineno)
{
    parse_add_locinfo(&ctxt->enums,ctxt->parse_file,lineno,ctxt->line,enum_name,"0",enum_typename);  // todo: enum value
}

static void c_add_structref(CParse *ctxt, StackElt *elt, char *type_name, char *func_ctxt)
{
    parse_add_locinfo(&ctxt->structrefs,ctxt->parse_file,elt->lineno,ctxt->line,elt->l.str,type_name,func_ctxt);
}

static void c_add_funcdef(CParse *ctxt, int lineno, char *name, char *func_line)
{
    parse_add_locinfof(&ctxt->funcs,ctxt->parse_file,lineno,func_line,name,NULL,"func %s",name);
}

static void c_add_funcref(CParse *ctxt, StackElt *elt, char *funcref_ctxt)
{
    parse_add_locinfo(&ctxt->funcrefs,ctxt->parse_file,elt->lineno,ctxt->line,elt->l.str,funcref_ctxt,funcref_ctxt); 
}

static void c_add_define(CParse *ctxt, char *define, int lineno)
{
    parse_add_locinfo(&ctxt->defines,ctxt->parse_file,lineno,ctxt->line,define,NULL,NULL);
}

// a->b => tag = b, ref = a
static void c_add_var(CParse *ctxt, StackElt *elt, char *ref, char *func_ctxt)
{
    parse_add_locinfo(&ctxt->vars,ctxt->parse_file,elt->lineno,ctxt->line,elt->l.str,ref,func_ctxt);
}


static int parser_error(CParse *ctxt, StackElt *s, char *fmt,...)
{
    int r;
    va_list vl;
//    if(!c_debug)
//        return 0;
    fprintf(stderr,"%s(%i):",ctxt->parse_file,DEREF(s,lineno));
    va_start(vl,fmt);
    r = vfprintf(stderr,fmt,vl);
    va_end(vl);
    fprintf(stderr,"\n");

    break_if_debugging();
    
    return r;
}

#define TOK_ERROR(ELT,FMT,...) parser_error(ctxt,ELT,FMT,__VA_ARGS__)

int c_debug;
int c_lex(CParse *ctxt, StackElt *top);


#define PREV_TOK(A) ((n_stack >= 2) && top[-1].tok == A)
#define PREV_TOKS2(A,B) ((n_stack >= 3) && top[-2].tok == A && top[-1].tok == B)
#define PREV_TOKS3(A,B,C) ((n_stack >= 4) && top[-3].tok == A && top[-2].tok == B && top[-1].tok == C)
//#define TOP (stack + n_stack - 1)

#define PUSH() ((top = (stack + n_stack++)),ZeroStruct(top))

#define NEXT_TOK()                                                   \
    PUSH();                                                          \
    top->tok=c_lex(p,top);                                           \
    if(!top->tok)                                                    \
        break;



// pushes all refs and reflist into a single reflist and puts it at 
// location 'start' 
// static void reduce_reflist(int start, StackElt **pstack, int *pn_stack)
// {
//     StackElt *stack = *pstack;
//     int n_stack = *pn_stack;
//     StackElt res = {0}; 
//     int i;

//     if(n_stack <= start)
//         return;
    
//     res.tok = VAR_DECL_LIST;
//     while(n_stack > start)
//     {
//         StackElt *top = stack + n_stack - 1;
//         switch(top->tok)
//         {
//         case VAR_DECL:
//             strs_find_add_str(&res.l.strs.s,&res.l.strs.n,top->l.str);
//             break;
//         case VAR_DECL_LIST:
//             for(i = 0; i < top->l.strs.n; ++i)
//                 strs_find_add_str(&res.l.strs.s,&res.l.strs.n,top->l.strs.s[i]);
//             break;
//         };
//         n_stack--;
//     }
//     *TOP = res;
//     *pn_stack = n_stack;
//     *pstack = stack;
// }

// parse until the first occurance of one of the characters in the
// passed string
static ABINLINE void parse_to_chars(CParse *p, StackElt *stack, int n_stack, char *toks)
{
    StackElt *top = 0;
    int n;
    int i;
    if(!stack || !p || !toks)
        return;

    if(n_stack == MAX_STACK)
    {
        parser_error(p,top,"out of room on stack in %s. aborting.",__FUNCTION__);
        return;
    }
    
    n = strlen(toks);
    PUSH();
    for(;;)
    {
        top->tok = c_lex(p,top);
        if(!top->tok)
            break;
        for(i = 0; i < n; ++i)
            if(toks[i] == top->tok)
                return;
    }
}


// matches pairing tokens like '{' and '}'
static ABINLINE void parse_to_tok(CParse *p, StackElt *stack, int n_stack, int tok, int open_tok)
{
    StackElt *top = 0;
    int n_open = 1;

    if(!stack || !p)
        return;

    if(n_stack == MAX_STACK)
    {
        parser_error(p,top,"out of room on stack in %s. aborting.",__FUNCTION__);
        return;
    }
    
    PUSH();
    while(n_open)
    {
        top->tok = c_lex(p,top);
        if(!top->tok)
            break;
        else if(top->tok == open_tok)
            n_open++;
        else if(top->tok == tok)
            n_open--;
    }
}

static void parse_enum_body(CParse *p, StackElt *stack, int n_stack, char *enum_typename )
{
    StackElt *top = NULL;
    for(;;)
    {
        if(n_stack == MAX_STACK)
        {
            parser_error(p,top,"out of room on stack in %s. aborting.",__FUNCTION__);
            break;
        }
        
        NEXT_TOK();
        
        if(top->tok == '}')
            return;
        
        switch(top->tok)
        {
        case TOK:
            c_add_enum(p,top->l.str,enum_typename,top->lineno);
            parse_to_chars(p,stack,n_stack,",}");
            if(top[1].tok == '}')
                return;
            break;
        case ',':
            break;
        default:
            parse_to_tok(p,stack,n_stack,'}',0); // something's wrong
            return;
        }
    }
}

// 
//static void extract_vars(CParse *p, StackElt *s, StackElt *e)

// expressions:
// assignment expression
// conditional expression
// expression, expression
// transition to this from:
// - expression_stmt: expression;
// -  
static void parse_expr(CParse *p, StackElt *stack, int n_stack, char *ctxt, char terminating_tok) 
{
    StackElt *top = NULL;
    int n_stack_in = n_stack;

    for(;;)
    {
        if(n_stack == MAX_STACK)
        {
            parser_error(p,top,"out of room on stack in %s. aborting.",__FUNCTION__);
            break;
        }
        
        NEXT_TOK();        
        if(top->tok == terminating_tok)
            return;
        switch(top->tok)
        {
        case '(':
            if(top[-1].tok == TOK)
                c_add_funcref(p,top-1,ctxt);
            parse_expr(p,stack,n_stack,ctxt,')');
            n_stack = n_stack_in;
            break;
        case TOK: // keep this around
        {
            char *ref = NULL;
            int tok = top[-1].tok;
            if((tok == PTR_OP || tok == '.') && top[-2].tok == TOK)
                ref = top[-2].l.str;
            c_add_var(p,top,ref,ctxt);
        }
        break;
        case PTR_OP:
            break;
//             // assignment
//         case '=':
//             // conditional exprs
//         case '?':
//         case ':':
//             // logical ops
//         case AND_OP:
//         case OR_OP:
//             // binary ops
//         case '|':
//         case '^':
//         case '&':
//             // equality
//         case LE_OP:
//         case GE_OP:
//         case EQ_OP:
//         case NE_OP:
//             // shift
//         case LEFT_OP:
//         case RIGHT_OP:
//         case '+':
//         case '-':
            // blah blah blah. all I care about is function calls
        default:
            n_stack = n_stack_in;
            break;
        };
    }
}

static void parse_var_decls(CParse *p, StackElt *stack, int n_stack, char *func_ctxt)
{
    StackElt *top = NULL;
    StackElt *type = NULL;
    StackElt *var = NULL;
    int n_stack_in = n_stack;
    for(;;)
    {
        if(n_stack == MAX_STACK)
        {
            parser_error(p,top,"out of room on stack in %s. aborting.",__FUNCTION__);
            break;
        }
        
        NEXT_TOK();        
        if(top->tok == ';')
            return;    
        switch(top->tok)
        {
        case TOK:
            if(!type)
            {
                type = top;
                n_stack_in = n_stack; // don't reduce past the type decl
            }
            else
            {
                c_add_structref(p,top,type->l.str,func_ctxt);
                n_stack = n_stack_in;
            }
            break;
        case '=':
            parse_expr(p,stack,n_stack,func_ctxt);
            n_stack = n_stack_in;
        }
    }
}

static void parse_func_body(CParse *p, StackElt *stack, int n_stack, char *func_ctxt)
{
    StackElt *top = NULL;
    int var_decls_allowed = 1;
    int n_stack_in = n_stack;
    for(;;)
    {
        if(n_stack == MAX_STACK)
        {
            parser_error(p,top,"out of room on stack in %s. aborting.",__FUNCTION__);
            break;
        }
        
        NEXT_TOK();        
        if(top->tok == '}')
            return;
        switch(top->tok)
        {
        case '{':
            if(top[-1].tok == '=') // struct or array init
                parse_to_tok(p,stack,n_stack,'}','{');
            else
            {
                parse_func_body(p,stack,n_stack,func_ctxt);
                n_stack = n_stack_in;
                var_decls_allowed = 0;
            }
            break;
        case '(':
            parse_expr(p,stack,n_stack,func_ctxt,')');
            if(top[-1].tok == TOK) {
                c_add_funcref(p,top-1,func_ctxt);
                n_stack-=2;
            }
            else
                n_stack--;
            break;
        case TOK:
            break;
        case ';':
            // var decls:
            // 1. storage  class: static auto register 
            // 2. type specifier: int, char, Foo
            // 3. declerator(s) : *bar, baz[10], (*fp)(params)
            if(var_decls_allowed)
            {
                StackElt *t = stack+n_stack_in;
                StackElt *type = NULL;
                int intrinsic_decl = 0;
                
                for(;t < top; t++)
                {
                    if(t->tok == '=')
                    {
                        do
                        {
                            t++;
                        } while(t < top && t->tok != ',');
                        continue;
                    }
                    else if(!type && IS_INTRINSIC_TYPE(t->tok))
                    {
                        intrinsic_decl = 1;
                        break; // don't bother with "int a;" decls
                    }
                    else if(!type && (t->tok == TOK))
                        type = t;
                    else if(type && t->tok == TOK)
                        c_add_structref(p,t,type->l.str,func_ctxt);
                }
                // if no type, good chance that this is an expression
                // statement, e.g a = 0;
                if(!intrinsic_decl && (!type || type+1 == top))
                    var_decls_allowed = 0;
            }
            n_stack = n_stack_in;
            break;
            // ====================
            // statement detection, for ending var decl ability 
        case IF:
        case ELSE:
        case SWITCH:
        case WHILE:
        case DO:
        case FOR:
        case GOTO:
        case CONTINUE:
        case BREAK:
        case RETURN:
            var_decls_allowed = 0;
            break;
        }
    }
}

static void parse_struct_body(CParse *p, StackElt *stack, int n_stack, char *struct_name)
{
    StackElt *top = NULL;
    StackElt *first_vartype = 0;
    int n_stack_in = n_stack;

    for(;;)
    {
        if(n_stack == MAX_STACK)
        {
            parser_error(p,top,"out of room on stack in %s. aborting.",__FUNCTION__);
            break;
        }
        if(n_stack == n_stack_in)
            first_vartype = NULL;
        
        NEXT_TOK();        
        if(top->tok == '}')
        {
            parse_to_tok(p,stack,n_stack,';',0);
            return;
        }
        switch(top->tok)
        {
        case '(':
            if(top[-1].tok == AST)
                parse_to_tok(p,stack,n_stack,')','('); // ignore for now
            n_stack = n_stack_in;
            break;
        case ';':
            if(first_vartype && first_vartype < top-1 && top[-1].tok == TOK) 
                c_add_structref(p,top-1,first_vartype->l.str,struct_name);
            n_stack = n_stack_in;
            break;
        case '{':
            parse_to_tok(p,stack,n_stack,'}','{');
            break;
        default:
            if(!first_vartype && (top->tok == TOK || IS_INTRINSIC_TYPE(top->tok)))
                first_vartype = top;
        }
    }
}


int c_parse(CParse *p)
{
    char ctxt[128];
    StackElt stack[MAX_STACK] = {0}; 
    StackElt *top = stack;
    int n_stack = 0;
    int res = 0;
    char *s;
//    c_debug = 1;
    for(;;)
    {
        if(n_stack == DIMOF(stack))
        {
            parser_error(p,top,"out of room on stack. aborting.");
            res = -1;
            break;
        }

        NEXT_TOK();
        switch(top->tok)
        {
        case '{':
            if(PREV_TOKS3(TYPEDEF,STRUCT, TOK)) // struct decl
            {
                c_add_struct(p,top[-1].l.str,top[-1].lineno);
                sprintf(ctxt,"struct %s",top[-1].l.str);
                parse_struct_body(p,stack,n_stack,ctxt);
                n_stack = 0;
            }
            else if(PREV_TOKS2(ENUM,TOK)) // struct decl
            {
                s = top[-1].l.str;
                c_add_enum_decl(p,s,top[-1].lineno);
                parse_enum_body(p,stack,n_stack,s);
                parse_to_tok(p,stack,n_stack,';',0);
                n_stack = 0;
            }
            else if(PREV_TOK(FUNC_HEADER)) // function def
            {
                char *func_name = top[-1].l.str;

                c_add_funcdef(p,top[-1].lineno,func_name,p->last_line);
                sprintf(ctxt,"func %s",func_name);
                parse_func_body(p,stack,n_stack,ctxt);
                
                // todo: cleanup
                n_stack = 0;
            }

            break;
        case '(':
            if(PREV_TOK(TOK)) // function decl/def
            {
                StackElt hdr = {0};
                // don't care about args yet
                //parse_arglist(p,stack,n_stack);
                parse_to_tok(p,stack,n_stack,')','(');

                hdr.tok = FUNC_HEADER;
                hdr.lineno = top->lineno;
                strcpy(hdr.l.str,top[-1].l.str);

                ZeroStruct(&stack[0]);
                stack[0] = hdr;
                n_stack = 1;
            }
            else
            {
                parse_to_tok(p,stack,n_stack,')','(');
                n_stack = 0; // dunno what this is
            }            
            break;
        case ';':
            if(stack[0].tok == TOK) // global variable
                c_add_structref(p,top-1,stack[0].l.str,"global var");
            n_stack = 0;
            break;
        case '=':
            // todo: some kind of global var init
            parse_to_tok(p,stack,n_stack,';',0);
            n_stack = 0;
            break;
        case POUND_DEFINE:
            c_add_define(p,top->l.str, top->lineno);
            n_stack = 0;
            break;
        case POUND_INCLUDE:
            n_stack = 0; // need to fix c_lex to get the string for this
            break;
        default:
            break;
        };
    }
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
    "volatile"
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

#define GETC() ((p->line[(p->i_line++)%DIMOF(p->line)] = (char)(c = getc(p->fp))),c)
#define UNGETC() (p->i_line--,ungetc(c, p->fp)) 
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
    LocInfo *li;
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
    

    TEST(0==c_parse_file(&cp,"test/foo.c")); // todo: embed and write out if not existing.

#define TEST_LI(TAG,REF,CTXT)     TEST(0==strcmp(li->tag,TAG)); \
    TEST(0==strcmp(li->referrer,REF));                          \
    TEST(0==strbeginswith(li->context,CTXT));                   \
    li++;

    TEST(cp.structrefs.n_locs == 13);
    li = cp.structrefs.locs;
    TEST(li->lineno == start_line + 3);
    TEST_LI("a",          "int",       "struct Foo");
    TEST(li->lineno == start_line + 4);
    TEST_LI("b",          "char",      "struct Foo");
    TEST(li->lineno == start_line + 9);
    TEST_LI("bar_a",      "int",       "struct Bar");
    TEST_LI("baz_b",      "char",      "struct Bar");
    TEST_LI("b",          "Foo",       "func test_func");
    TEST_LI("c",          "Bar",       "func test_func");
    TEST_LI("bar2",       "Foo",       "global var");
    TEST_LI("hNameMsg",   "Message",  "struct Foo2");
    TEST_LI("iSortID",    "U32",       "struct Foo2");
    TEST_LI("bSearchable","bool",      "struct Foo2");
    TEST_LI("eType",       "ItemType","struct Foo2");
    TEST_LI("pBar",        "Bar",      "func test_func3");
    TEST_LI("pBaz",        "Bar",      "func test_func3");
    TEST_LI("foo",         "U32",      "func test_func3");


    // structs
    TEST(cp.structs.n_locs == 4);
    li = cp.structs.locs + 0;
    TEST(0==strcmp(li->tag,"Foo"));
    TEST(li->referrer == NULL);
    TEST(0==strcmp(li->context,"struct Foo"));
    TEST(0==stricmp(li->file,"test/Foo.c"));
    
    li = cp.structs.locs + 1;
    TEST(0==strcmp(li->tag,"Bar"));
    TEST(0==stricmp(li->file,"test/Foo.c"));
    TEST(0==strcmp(li->context,"struct Bar"));
    
    li = cp.structs.locs + 2;
    TEST(0==strcmp(li->tag,"Baz"));
    TEST(0==strcmp(li->context,"enum Baz"));

    li++;
    TEST(0==strcmp(li->tag,"Foo2"));
    TEST(0==strcmp(li->context,"struct Foo2"));

    
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
    TEST(0==strcmp(li->referrer,"func test_func2"));

    li++;
    TEST(0==strcmp(li->tag,"GET_REF"));
    TEST(0==strcmp(li->context ,"func test_func3"));

    n_lis = parse_locinfos_from_context(&cp.vars,"func test_func3",&lis);
    TEST(n_lis == 7);
    pli = lis;
    TEST(0==strcmp(pli[0]->context,"func test_func3"));
    TEST(0==strcmp((*pli++)->tag,"pFoo"));
    TEST(0==strcmp((*pli++)->tag,"hFoo"));
    TEST(0==strcmp((*pli++)->tag,"pDef"));
    TEST(0==strcmp((*pli++)->tag,"eContents"));
    TEST(0==strcmp((*pli++)->tag,"Store_All"));
    TEST(0==strcmp((*pli++)->tag,"pDef"));
    TEST(0==strcmp((*pli++)->tag,"bSellEnabled"));
    TEST(0==strcmp((*pli++)->tag,"eBar"));

    // TODO: do a final lineno test

    return 0;
}
