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

int c_parse(CParse *ctxt);

#define OR(A,B) ((A)?(A):(B))

static void c_add_struct(CParse *ctxt, char *struct_name, int line)
{
    parse_add_locinfof(&ctxt->structs,ctxt->parse_file,line,struct_name,NULL,"struct %s", struct_name);
}

static void c_add_enum(CParse *ctxt, char *struct_name, int line)
{
    parse_add_locinfof(&ctxt->structs,ctxt->parse_file,line,struct_name,NULL,"enum %s", struct_name);
}


// static void c_add_structref(CParse *ctxt, int line, char *referent, char *referrer, char *varinfo)
// {
//     parse_add_locinfo(&ctxt->structrefs,ctxt->parse_file,line,referent,OR(referrer,""),OR(varinfo,""));
// }

static void c_add_funcdef(CParse *ctxt, int line, char *name, char *func_line)
{
    parse_add_locinfo(&ctxt->funcs,ctxt->parse_file,line,name,NULL,func_line);
}

// static void c_add_funcref(CParse *ctxt, int line, char *referent, char *referrer)
// {
//     parse_add_locinfo(&ctxt->funcrefs,ctxt->parse_file,line,referent,referent,referrer);
// }


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

    return res;
}

int c_load(CParse *cp)
{
    int res = 0;
    if(file_exists("c_structs.abs"))
        res += absfile_read_parse("c_structs.abs",&cp->structs);
    if(file_exists("c_funcs.abs"))
        res += absfile_read_parse("c_funcs.abs",&cp->funcs);
    if(file_exists("c_structrefs.abs"))
        res += absfile_read_parse("c_structrefs.abs",&cp->structrefs);
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

int c_query(CParse *cp, char *tag, int query_flags)
{
    int res = 0;
    if(query_flags & CQueryFlag_Structs)
        res += c_findstructs(cp,tag);
    if(query_flags & CQueryFlag_Structrefs)
        res += c_findstructrefs(cp,tag);
    if(query_flags & CQueryFlag_Funcs)
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
    
    // c keywords
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
} c_tokentype;
#define C_KWS_START TYPEDEF


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
        char str[32];
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

#define STACK(OFFSET) (stack+n_stack-1+OFFSET)
#define PREV_TOK(A) ((n_stack >= 2) && STACK(-1)->tok == A)
#define PREV_TOKS2(A,B) ((n_stack >= 3) && STACK(-2)->tok == A && STACK(-1)->tok == B)
#define PREV_TOKS3(A,B,C) ((n_stack >= 4) && STACK(-3)->tok == A && STACK(-2)->tok == B && STACK(-1)->tok == C)
//#define TOP (stack + n_stack - 1)

#define PUSH() ((top = (stack + n_stack++)),ZeroStruct(top))

#define NEXT_TOK()                                                   \
    PUSH();                                                          \
    top->tok=c_lex(ctxt,top);                                        \
    if(!top->tok)                                                    \
        break;

#define BP(I) (&stack[stack_start+(I)])    

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

// skip to give tok. open_tok is optional
static void parse_to_tok(CParse *ctxt, StackElt *stack, int n_stack, int tok, int open_tok) 
{
    StackElt *top = 0;
    int n_open = 1;

    if(!stack || !ctxt)
        return;
    
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


// static void parse_arglist(CParse *ctxt, StackElt **pstack, int *pn_stack)
// {
//     StackElt *t;
//     StackElt *stack = *pstack;
//     int n_stack = *pn_stack;
//     int stack_start = n_stack;
//     StackElt res = {0};
//     int done = 0;
//     res.tok = ARGLIST;
//     while(!done)
//     {
//         NEXT_TOK();
//         switch(TOP->tok)
//         {
//         case ')':
//             done = 1;
//             // fall through
//         case ',':
//             for(t = stack + stack_start; t < stack + n_stack-1; ++t)
//                 if(t->tok == TOK)
//                     break;
//             if(t->tok != TOK)
//                 TOK_ERROR(t,"unable to get var type");
//             else
//                 strs_find_add_str(&res.l.strs.s,&res.l.strs.n,t->l.str);

//             if(done)
//             {
//                 n_stack = stack_start;
//                 *TOP = res; // store result in entering slot on stack
//             }
//             break;
//         case '(':
//             parse_arglist(ctxt,&stack,&n_stack); // recurse
//             break;
//         case ';':
//             // some kind of error
//             done = 1;
//             strs_cleanup(res.l.strs.s,res.l.strs.n);
//             n_stack = stack_start - 1;
//             break;
//         default:
//             break;
//         };
//     }
//     *pn_stack = n_stack;
//     *pstack = stack;    
// }

// static void parse_body(CParse *ctxt, Parse *res, StackElt *stack, int *pn_stack, char *referrer )
// {
//     int n_stack = *pn_stack;
//     int stack_start = n_stack; // can't reduce past ths
//     int can_see_decls = 1;
//     int done = 0;
//     StackElt *top = NULL;
//     do
//     {
//         if(n_stack == MAX_STACK)
//         {
//             parser_error(ctxt,top,"out of room in stack");
//             return;
//         }
        
//         NEXT_TOK();
        
//         switch(top->tok)
//         {
//         case '(':
//             if(PREV_TOK(TOK))
//                 c_add_funcref(ctxt,top[-1].line,top[-1].l.str,referrer);
//             else
//                 parse_to_tok(ctxt,&stack,&n_stack,')');
//             n_stack = stack_start;
//             break;
//         case '{':
//             parse_body(ctxt,res,&stack,&n_stack,referrer ); // recurse
//             // parse_cleanup(&top->l.locs); todo: leak leak leak
//             break;
//         case '*':
//             break;
//         case CHAR_TOK:
//         case SHORT_TOK:
//         case INT_TOK:
//         case LONG_TOK:
//         case SIGNED:
//         case UNSIGNED:
//         case FLOAT_TOK:
//         case DOUBLE:
//         case VOID_TOK:
//             break;
//         case ';':
//             if(can_see_decls && BP(0)->tok == TOK)
//                 c_add_structref(ctxt,BP(0)->line,BP(0)->l.str,referrer, ctxt->line);
//             n_stack = stack_start;
//             break;
//         case '}':
//             done = 1;
//             break;
//         default:
//             if(top->tok >= C_KWS_START)
//                 can_see_decls = 0;
//         };
//     } while(!done);
//     *pn_stack = stack_start;
//     *pstack = stack;
// }

int c_parse(CParse *ctxt)
{
    StackElt stack[MAX_STACK] = {0}; 
    StackElt *top = stack;
    int n_stack = 0;
    int res = 0;

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
                c_add_struct(ctxt,top[-1].l.str,top->line);
                // todo: struct members
//                parse_body(ctxt,&ctxt->structrefs,stack,&n_stack,str);
                parse_to_tok(ctxt,stack,n_stack,'}','{');
                parse_to_tok(ctxt,stack,n_stack,';',0);
                n_stack = 0;
            }
            else if(PREV_TOKS3(TYPEDEF,ENUM,TOK)) // struct decl
            {
                c_add_enum(ctxt,top[-1].l.str,top->line);
                // todo: enum members
//                parse_body(ctxt,&ctxt->structrefs,stack,&n_stack,str);
                parse_to_tok(ctxt,stack,n_stack,'}','{');
                parse_to_tok(ctxt,stack,n_stack,';',0);
                n_stack = 0;
            }
            else if(PREV_TOK(FUNC_HEADER)) // function def
            {
                c_add_funcdef(ctxt,STACK(-1)->line,STACK(-1)->l.str,ctxt->last_line);
                // todo: refs in body
//                 parse_body(ctxt,&ctxt->funcrefs,&stack,&n_stack,str);
                // todo: cleanup
                parse_to_tok(ctxt,stack,n_stack,'}','{');
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
        default:
            break;
        };
    }
    return res;
}

int c_lex(CParse *ctxt, StackElt *top)
{
    static const struct { char *kw; int tok; } kws[] = {
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
        { "...", ELLIPSIS },
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
        { "->", PTR_OP },
        { "++", INC_OP },
        { "--", DEC_OP },
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
        { "+=", ADD_ASSIGN },
        { "-=", SUB_ASSIGN },
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
    int first_line;
    S64 timer_start = timer_get();

//    S64 diff_getc   = timer_get();
//#define GETC() (ctxt->getc_timing += timer_diff_reset(&diff_getc ), getc(ctxt->fp))
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
    first_line = top->line = ctxt->parse_line;
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

    if(newline && c == '#')
    {
        int last = 0;
        // eat preprocessor (could do in grammer, but what the hey)
        // if line ends with \, continue to eat.
        for(;;)
        {
            while((GETC()) != '\n' && c != EOF)
            {
                if(!isspace(c))
                    last = c;
            }
            if(last == '\\')
                ctxt->parse_line++;
            else
                break;
        }
        UNGETC();
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
        if(c_debug)
            fprintf(stderr,"character token(%c)\n",c);
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
