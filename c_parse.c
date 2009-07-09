/***************************************************************************
 *     Copyright (c) 2009-2009, Aaron Brady
 *     All Rights Reserved
 *
 * Module Description:
 * todo:
 * - parse structs for members
 * - build aa call tree
 * - search list of files
 * - fast load
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

int c_findfuncsrcfile(CParse *cp, char *sn)
{
    int res = 0;
    AvlNode *node;
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
    for(i = 0; i<n; ++i)
    {
        int j;
        for(j = 0; j<ps[i]->n_locs; ++j)
        {
            LocInfo *li = ps[i]->locs + j;
            char *fn = fname_nodir(li->file);
            if(0 == stricmp(fn,sn) && !avltree_find(&t,li->file))
                avltree_insert(&t,li->file);
        }
    }
    node = t.root;
    while(node)
    {
        if(node->left && node->left->p)
        {
            node = node->left;
        }
        else if(node->right && node->right->p)
        {
            node = node->right;
        }
        else
        {
            printf("** [[file:%s::%i][%s]] file %s\n", node->p,1, fname_nodir(node->p), node->p);
            node->p = NULL;
            node = node->up;
            res++;
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
        res += c_findfuncsrcfile(cp,tag);
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
    AUTO_COMMAND,
    CHAR_LITERAL,
    TOK,
    STR,
    VAR_DECL,
    VAR_DECL_LIST,
    ARGLIST,
    FUNC_HEADER,
    
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

    // not really a C type
    POUND_DEFINE,
} c_tokentype;
#define C_KWS_START TYPEDEF
#define IS_INTRINSIC_TYPE(T) INRANGE(T,CHAR_TOK,DOUBLE+1)

typedef enum c_vartypee {
    VT_NONE,
    VT_STR,
    VT_NUM,
    VT_REF,
    VT_REFLIST,
};


int c_tokentype_vars[] = 
{
    VT_NONE, //ERROR,
    VT_NONE, //TYPEDEF,
    VT_NONE, //EXTERN,
    VT_NONE, //STATIC,
    VT_NONE, //STRUCT,
    VT_NONE, //AUTO_COMMAND,
    VT_NONE, //CHAR_LITERAL,
    VT_STR, //TOK,
    VT_STR, //STR,
    VT_REF, //VAR_DECL,
    VT_REFLIST, // VAR_DECL_LIST
    VT_REFLIST, // ARGLIST
};


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
    int line;
} StackElt;

#define MAX_STACK 256

static void c_add_struct(CParse *ctxt, char *struct_name, int line)
{
    parse_add_locinfof(&ctxt->structs,ctxt->parse_file,line,struct_name,NULL,"struct %s", struct_name);
}

static void c_add_enum_decl(CParse *ctxt, char *enum_name, int line)
{
    parse_add_locinfof(&ctxt->structs,ctxt->parse_file,line,enum_name,NULL,"enum %s", enum_name); // putting this in structs, should probably rename 'structs' to 'types' 
}

static void c_add_enum(CParse *ctxt, char *enum_name, char *enum_typename, int line)
{
    parse_add_locinfo(&ctxt->enums,ctxt->parse_file,line,enum_name,enum_typename,ctxt->line); 
}

static void c_add_structref(CParse *ctxt, StackElt *elt, char *type_name, char *func_name)
{
    parse_add_locinfo(&ctxt->structrefs,ctxt->parse_file,elt->line,type_name,func_name,ctxt->line); 
    // todo: add var info
//    parse_add_locinfo(&ctxt->vars,ctxt->parse_file,elt->line,elt->l.str,type_name,ctxt->line); 
}

// static void c_add_structref(CParse *ctxt, int line, char *referent, char *referrer, char *varinfo)
// {
//     parse_add_locinfo(&ctxt->structrefs,ctxt->parse_file,line,referent,OR(referrer,""),OR(varinfo,""));
// }

static void c_add_funcdef(CParse *ctxt, int line, char *name, char *func_line)
{
    parse_add_locinfo(&ctxt->funcs,ctxt->parse_file,line,name,NULL,func_line);
}

static void c_add_funcref(CParse *ctxt, StackElt *elt, char *funcref_ctxt)
{
    parse_add_locinfo(&ctxt->funcrefs,ctxt->parse_file,elt->line,elt->l.str,funcref_ctxt,ctxt->line); 
}

static void c_add_structmember(CParse *ctxt, StackElt *elt, char *member_type, char *parent_struct)
{
    parse_add_locinfof(&ctxt->structrefs,ctxt->parse_file,elt->line,elt->l.str,member_type,"struct %s: %s", parent_struct,ctxt->line); 
}

static void c_add_structmemberref(CParse *ctxt, StackElt *elt, char *member_type, char *parent_struct)
{
    parse_add_locinfof(&ctxt->structrefs,ctxt->parse_file,elt->line,elt->l.str,member_type,"struct %s: %s", parent_struct,ctxt->line); 
}

static void c_add_define(CParse *ctxt, char *define, int line)
{
    parse_add_locinfo(&ctxt->defines,ctxt->parse_file,line,define,NULL,ctxt->line);
}


static int parser_error(CParse *ctxt, StackElt *s, char *fmt,...)
{
    int r;
    va_list vl;
//    if(!c_debug)
//        return 0;
    fprintf(stderr,"%s(%i):",ctxt->parse_file,DEREF(s,line));
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
    top->tok=c_lex(ctxt,top);                                        \
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
static ABINLINE void parse_to_chars(CParse *ctxt, StackElt *stack, int n_stack, char *toks)
{
    StackElt *top = 0;
    int n;
    int i;
    if(!stack || !ctxt || !toks)
        return;

    if(n_stack == MAX_STACK)
    {
        parser_error(ctxt,top,"out of room on stack in %s. aborting.",__FUNCTION__);
        return;
    }
    
    n = strlen(toks);
    PUSH();
    for(;;)
    {
        top->tok = c_lex(ctxt,top);
        if(!top->tok)
            break;
        for(i = 0; i < n; ++i)
            if(toks[i] == top->tok)
                return;
    }
}


// matches pairing tokens like '{' and '}'
static ABINLINE void parse_to_tok(CParse *ctxt, StackElt *stack, int n_stack, int tok, int open_tok)
{
    StackElt *top = 0;
    int n_open = 1;

    if(!stack || !ctxt)
        return;

    if(n_stack == MAX_STACK)
    {
        parser_error(ctxt,top,"out of room on stack in %s. aborting.",__FUNCTION__);
        return;
    }
    
    PUSH();
    while(n_open)
    {
        top->tok = c_lex(ctxt,top);
        if(!top->tok)
            break;
        else if(top->tok == open_tok)
            n_open++;
        else if(top->tok == tok)
            n_open--;
    }
}

static void parse_enum_body(CParse *ctxt, StackElt *stack, int n_stack, char *enum_typename )
{
    StackElt *top = NULL;
    for(;;)
    {
        if(n_stack == MAX_STACK)
        {
            parser_error(ctxt,top,"out of room on stack in %s. aborting.",__FUNCTION__);
            break;
        }
        
        NEXT_TOK();
        
        if(top->tok == '}')
            return;
        
        switch(top->tok)
        {
        case TOK:
            c_add_enum(ctxt,top->l.str,enum_typename,top->line);
            parse_to_chars(ctxt,stack,n_stack,",}");
            if(top[1].tok == '}')
                return;
            break;
        case ',':
            break;
        default:
            parse_to_tok(ctxt,stack,n_stack,'}',0); // something's wrong
            return;
        }
    }
}

// expressions:
// assignment expression
// conditional expression
// expression, expression
static void parse_paren_expression(CParse *ctxt, StackElt *stack, int n_stack, char *func_name)
{
    StackElt *top = NULL;
    int n_stack_in = n_stack;

    for(;;)
    {
        if(n_stack == MAX_STACK)
        {
            parser_error(ctxt,top,"out of room on stack in %s. aborting.",__FUNCTION__);
            break;
        }
        
        NEXT_TOK();        
        if(top->tok == ')')
            return;
        switch(top->tok)
        {
        case '(':
            if(top[-1].tok == TOK)
                c_add_funcref(ctxt,top-1,func_name);
            parse_paren_expression(ctxt,stack,n_stack,func_name);
            n_stack = n_stack_in;
            break;
        case TOK: // keep this around
            
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

static void parse_func_body(CParse *ctxt, StackElt *stack, int n_stack, char *func_name)
{
    int i;
    StackElt *top = NULL;
    int var_decls_allowed = 1;
    int n_stack_in = n_stack;
    for(;;)
    {
        if(n_stack == MAX_STACK)
        {
            parser_error(ctxt,top,"out of room on stack in %s. aborting.",__FUNCTION__);
            break;
        }
        
        NEXT_TOK();        
        if(top->tok == '}')
            return;
        switch(top->tok)
        {
        case '{':
            parse_func_body(ctxt,stack,n_stack,func_name);
            n_stack = n_stack_in;
            var_decls_allowed = 0;
            break;
        case '(':
            parse_paren_expression(ctxt,stack,n_stack,func_name);
            if(top[-1].tok == TOK)
                c_add_funcref(ctxt,top-1,func_name);
            n_stack = n_stack_in;
            break;
        case TOK:
            // possible struct deref
//             if((top[-1].tok == PTR_OP || top[-1].tok == '.') && top[-2].tok == TOK)
//             {
//             }
            
                
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
                        do{t++;}while(t < top && t->tok != ',');
                        continue;
                    }
                    else if(!type && IS_INTRINSIC_TYPE(t->tok))
                    {
                        intrinsic_decl = 1;
                        break; // don't bother with "int a;" decls
                    }
                    else if(!type && (t->tok == TOK))
                        type = t;
                    else if(type && (t[1].tok == ',' || t+1 == top))
                        c_add_structref(ctxt,t,type->l.str,func_name);
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
        case ':': // labeled statement
            if(top[-1].tok == TOK)
                var_decls_allowed = 0;
            break;
        }
    }
}

static void parse_struct_body(CParse *ctxt, StackElt *stack, int n_stack, char *struct_name)
{
    StackElt *tmp;
    StackElt *top = NULL;
    int n_stack_in = n_stack;

    for(;;)
    {
        if(n_stack == MAX_STACK)
        {
            parser_error(ctxt,top,"out of room on stack in %s. aborting.",__FUNCTION__);
            break;
        }
        
        NEXT_TOK();        
        if(top->tok == '}')
        {
            parse_to_tok(ctxt,stack,n_stack,';',0);
            return;
        }
        switch(top->tok)
        {
        case ';':
            for(tmp = stack + n_stack_in; tmp < top; ++tmp)
                if(tmp->tok == TOK || IS_INTRINSIC_TYPE(tmp->tok))
                    break;
            if(tmp < top  && top[-1].tok == TOK) 
                c_add_structmember(ctxt,top-1,tmp->l.str,struct_name);
            n_stack = n_stack_in;
            break;
        case '{':
            parse_to_tok(ctxt,stack,n_stack,'}','{');
            break;
        }
    }
}


int c_parse(CParse *ctxt)
{
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
            parser_error(ctxt,top,"out of room on stack. aborting.");
            res = -1;
            break;
        }

        NEXT_TOK();
        switch(top->tok)
        {
        case '{':
            if(PREV_TOKS3(TYPEDEF,STRUCT, TOK)) // struct decl
            {
                c_add_struct(ctxt,top[-1].l.str,top[-1].line);
                // todo: struct members
//                parse_body(ctxt,&ctxt->structrefs,stack,&n_stack,str);
                parse_struct_body(ctxt,stack,n_stack,top[-1].l.str);
                n_stack = 0;
            }
            else if(PREV_TOKS2(ENUM,TOK)) // struct decl
            {
                s = top[-1].l.str;
                c_add_enum_decl(ctxt,s,top[-1].line);
                parse_enum_body(ctxt,stack,n_stack,s);
                parse_to_tok(ctxt,stack,n_stack,';',0);
                n_stack = 0;
            }
            else if(PREV_TOK(FUNC_HEADER)) // function def
            {
                char *func_name = top[-1].l.str;
                c_add_funcdef(ctxt,top[-1].line,func_name,ctxt->last_line);
                // todo: refs in body
                parse_func_body(ctxt,stack,n_stack,func_name);
                
                // todo: cleanup
                n_stack = 0;
            }

            break;
        case '(':
            if(PREV_TOK(TOK)) // function decl/def
            {
                StackElt hdr = {0};
                // don't care about args yet
                //parse_arglist(ctxt,stack,n_stack);
                parse_to_tok(ctxt,stack,n_stack,')','(');

                hdr.tok = FUNC_HEADER;
                hdr.line = top->line;
                strcpy(hdr.l.str,top[-1].l.str);

                ZeroStruct(&stack[0]);
                stack[0] = hdr;
                n_stack = 1;
            }
            else
            {
                parse_to_tok(ctxt,stack,n_stack,')','(');
                n_stack = 0; // dunno what this is
            }            
            break;
        case ';':
            // todo: add func decls
//             if(stack[0].tok == FUNC_HEADER)
//                 c_add_funcdecl(ctxt,stack[0].l.str,ctxt->line);
            n_stack = 0;
            break;
        case '=':
            // todo: some kind of global var init
            parse_to_tok(ctxt,stack,n_stack,';',0);
            n_stack = 0;
            break;
        case POUND_DEFINE:
            c_add_define(ctxt,top->l.str, top->line);
            n_stack = 0;
            break;
        default:
            break;
        };
    }
    return res;
}
// todo: parse '==' properly
int c_lex(CParse *ctxt, StackElt *top)
{
    typedef struct KwTokPair { char *kw; int tok; } KwTokPair;
    typedef struct OpTokPair { char kw[4]; int tok;} OpTokPair;    
    static const KwTokPair kws[] = {
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
//        { "volatile", VOLATILE }, ignored
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
    };
     
    // NOTE: none of these work
    static const OpTokPair ops[] = {
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
    static char const *ignored_kws[] = {
        "ATH_ARG",
        "NN_PTR_GOOD",
        "const",
        "volatile"
    };
    int c;
    char tok[4096];
    char *i;
    int j;
    int newline;
    S64 timer_start = timer_get();
//    S64 timer_getc;
// timing each getch call has too much overhead
// #define GETC() ((timer_getc = timer_get()),(ctxt->line[(ctxt->i_line++)%DIMOF(ctxt->line)] = (char)(c = getc(ctxt->fp))),(ctxt->getc_timing += timer_diff(timer_getc)),c)

#define GETC() ((ctxt->line[(ctxt->i_line++)%DIMOF(ctxt->line)] = (char)(c = getc(ctxt->fp))),c)
#define UNGETC() (ctxt->i_line--,ungetc(c, ctxt->fp)) 
#define LEX_RET(VAL) { ctxt->lex_timing += timer_diff(timer_start); \
        ctxt->line[(ctxt->i_line)%DIMOF(ctxt->line)] = 0;           \
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
            ctxt->i_line %= DIMOF(ctxt->line);
            while(isspace(ctxt->line[--ctxt->i_line]))
                ; // empty
            ctxt->line[ctxt->i_line+1] = 0;
            ctxt->i_line = 0;
            strcpy(ctxt->last_line,ctxt->line);
            ctxt->parse_line++;
            newline = 1;
        }
    }
    top->line = ctxt->parse_line;

    if(newline && c == '#')
    {
        int last = 0;
        int found_pound_define = 0;
        c = c_lex(ctxt, top);

        if(!c)
            LEX_RET(0);
        
        if(c == TOK && 0 == strcmp(top->l.str,"define"))
        {
            c = c_lex(ctxt, top);
            if(c == TOK)
                found_pound_define = 1;
        }

        // eat preprocessor (could do in grammar, but what the hey)
        // if line ends with \, continue to eat.
        for(;;)
        {
            while((GETC()) != '\n' && c != EOF)
            {
                if(!isspace(c))
                    last = c;
            }
            ctxt->i_line--;
            if(last == '\\')
                ctxt->parse_line++;
            else
                break;
        }
        UNGETC();
        if(found_pound_define)
            LEX_RET(POUND_DEFINE);
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
                        ctxt->parse_line++;
                }
                    
                if((c=GETC()) == '/')
                    break;
                UNGETC();
            }
            goto yylex_start;
        }
        UNGETC();
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
                        ctxt->parse_line++;
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
    int i;
    DirScan dir_scan = {0};
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
    
    break_if_debugging();
    TEST(0==c_parse_file(&cp,"test/foo.c")); // todo: embed and write out if not existing.

    TEST(cp.structrefs.n_locs == 6);
    li = cp.structrefs.locs;
    TEST(0==strcmp(li->tag,"a"));
    TEST(0==strcmp(li->referrer,"int"));
    TEST(0==strbeginswith(li->context,"struct Foo"));
    TEST(li->line == 3);
    li++;
    TEST(0==strcmp(li->tag,"b"));
    TEST(0==strcmp(li->referrer,"char"));
    TEST(0==strbeginswith(li->context,"struct Foo"));
    TEST(li->line == 4);
    li++;
    TEST(0==strcmp(li->tag,"bar_a"));
    TEST(0==strcmp(li->referrer,"int"));
    TEST(0==strbeginswith(li->context,"struct Bar"));
    TEST(li->line == 9);
    li++;
    TEST(0==strcmp(li->tag,"baz_b"));
    TEST(0==strcmp(li->referrer,"char"));
    TEST(0==strbeginswith(li->context,"struct Bar"));
    TEST(li->line == 10);
    li++;
    TEST(0==strcmp(li->tag,"Foo"));
    TEST(0==strcmp(li->referrer,"test_func"));
    TEST(li->line == 16);
    li++;
    TEST(0==strcmp(li->tag,"Bar"));
    TEST(0==strcmp(li->referrer,"test_func"));
    TEST(li->line == 21);

    TEST(cp.structs.n_locs == 3);
    li = cp.structs.locs + 0;
    TEST(0==strcmp(li->tag,"Foo"));
    TEST(0==stricmp(li->file,"test/Foo.c"));
    TEST(0==strcmp(li->context,"struct Foo"));
    TEST(li->line == 1);
    
    li = cp.structs.locs + 1;
    TEST(0==strcmp(li->tag,"Bar"));
    TEST(0==stricmp(li->file,"test/Foo.c"));
    TEST(0==strcmp(li->context,"struct Bar"));
    TEST(li->line == 7);
    
    li = cp.structs.locs + 2;
    TEST(0==strcmp(li->tag,"Baz"));
    TEST(0==strcmp(li->context,"enum Baz"));
    TEST(li->line == 50);
    
    TEST(cp.funcs.n_locs == 3);
    li = cp.funcs.locs + 0;
    TEST(0==strcmp(li->tag,"test_func"));
    TEST(0==stricmp(li->file,"test/Foo.c"));
    TEST(0==strcmp(li->context,"void test_func( Entity *pEnt, char *RewardTableName, char *ChoiceName )"));
    TEST(li->line == 13);
    
    li = cp.funcs.locs + 1;
    TEST(0==strcmp(li->tag,"CommonAlgoTables_Load"));
    TEST(0==stricmp(li->file,"test/Foo.c"));
    TEST(0==strcmp(li->context,"void CommonAlgoTables_Load(void)"));
    TEST(li->line == 28);
    
    TEST(cp.defines.n_locs == 1);
    li = cp.defines.locs + 0;
    TEST(0==strcmp(li->tag,"FOO"));
    TEST(0==stricmp(li->file,"test/Foo.c"));
    TEST(0==strcmp(li->context,"#define FOO(X,Y,...) x = y + z;                 \\\r    line_2"));
    TEST(li->line == 47);
    
    TEST(cp.enums.n_locs == 3);
    li = cp.enums.locs;
    TEST(0==strcmp(li[0].tag,"Bar_A"));
    TEST(0==strcmp(li[0].referrer,"Baz"));
    TEST(0==strcmp(li[1].tag,"Bar_B"));    
    TEST(0==strcmp(li[1].referrer,"Baz"));
    TEST(0==strcmp(li[2].tag,"Bar_C"));    
    TEST(0==strcmp(li[2].referrer,"Baz"));
    TEST(li[0].line == 52);
    TEST(li[2].line == 55);   

    TEST(cp.funcrefs.n_locs == 2);
    li = cp.funcrefs.locs;
    TEST(0==strcmp(li->tag,"foo"));
    TEST(0==strcmp(li->referrer,"test_func2"));
    TEST(li->line == 60);
    li++;
    TEST(0==strcmp(li->tag,"strcmp"));
    TEST(0==strcmp(li->referrer,"test_func2"));
    TEST(li->line == 61);

    // ----------------------------------------
    // scan a bunch of files

    printf("scanning ./test...");
    scan_dir(&dir_scan,"./test",1,dirscan_accept_c_files,NULL);
    printf("done.\n");

    printf("found %i files:\n",dir_scan.n_files);
    TEST(dir_scan.n_files >= 3);
    for(i = 0; i < dir_scan.n_files;++i)
    {
        int r;
        printf("scanning %s",dir_scan.files[i]);
        r = c_parse_file(&cp,dir_scan.files[i]);
        TEST(r>=0);
    }

    for(i = 0; i < DIMOF(structs_to_find); ++i)
        TEST(0<c_findstructs(&cp,structs_to_find[i]));
    for(i = 0; i < DIMOF(structs_not_to_find); ++i)
        TEST(0==c_findstructs(&cp,structs_not_to_find[i]));
    return 0;
}
