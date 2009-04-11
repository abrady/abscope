%union
 {
     char *str;
     int num;
}

%{
// PROLOGUE
#include "abscope.h"
#include <stdio.h>
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include "c_parse.h"
#define YYDEBUG 1
#define YYINCLUDED_STDLIB_H

#define __STDC__ // ab: do this last, only needed by output of this
    int yylex (struct Parse *ctxt);
    void yyerror (struct Parse *ctxt, char const *);
    void meta_init()
    {
        printf("meta_init\n");
    }
    void metatok_push(char *p) 
    {
        printf("meta param: %s\n",p);
    }
    void mata_done()
    {
        printf("meta_done\n");
    }
    char *tok_append(char*a,char *b);

static void print_token_value (FILE *, int, YYSTYPE);
#define YYPRINT(file, type, value) print_token_value (file, type, value)

#pragma warning(disable:4127) // conditional constant e.g. {}while(0);
#pragma warning(disable:4244) // cast to smaller type (int to short)
#pragma warning(disable:4702) // unreachable code
%}

// ========================================
// directives to the parser

%defines
%debug
%token-table
%name-prefix="c_"
%verbose
%locations
%parse-param {struct Parse *ctxt}
%lex-param   {struct Parse *ctxt}

// ========================================
// tokens

%token TYPEDEF EXTERN STATIC STRUCT AUTO_COMMAND
%token<num> CHAR_LITERAL
%token<str> TOK STR
%type<str>  type_decl

%destructor { free ($$); } TOK STR type_decl
// %destructor { free( $$)  } string


%start translation_unit

%%

translation_unit:
                external_declaration
        |       translation_unit external_declaration
                ;

external_declaration:
                function_definition
        |       struct_decl
        |       autocmd_decl
        |       ignored_stuff // catches errors too
                ;

unary_operator: 
                '&'
        | '*'
        | '+'
        | '-'
        | '~'
        | '!'
        | '='
                ;
ignored_stuff:
                '{'
        |       '}'
        |       ';'
        |       '['
        |       ']'
        |       ','
        |       '\\'
        |       '.'
        |       '<'
        |       '>'
        |       '/'
        |       '%'
        |       '^'
        |       '('
        |       ')'
        |       '|'
        |       '?'
        |       ':'
        |       unary_operator
        |       STR 
        |       EXTERN
        |       STATIC
        |       CHAR_LITERAL
        |       error 
        ;

function_definition:
                type_decl TOK '(' function_args_opt ')' '{' /*{ printf("function def. ret(%s) fn(%s)\n", $2, $3); }*/
                ;

function_arg:   
                type_decl TOK //{ printf("arg(%s,%s)", $1, $2); }
                ;

function_args_opt:
                /*empty*/ //{ printf("no args. "); }
        |       function_arg ',' function_args_opt //{ printf(","); }
        |       function_arg
                ;

type_decl:
                TOK { $$ = $1; }
        |       type_decl  '*' {$$ = tok_append($1,"*"); }
                ;


struct_decl:    TYPEDEF STRUCT TOK '{' { add_struct_decl(ctxt, $3, @3.first_line); }

        ;

autocmd_decl:   AUTO_COMMAND
        ;

%%  
struct Parse *ctxt;
int yylex (struct Parse *ctxt)
{
    static struct { char *kw; int tok; } kws[] = {
        { "typedef",           TYPEDEF },
        { "extern",            EXTERN },
        { "static",            STATIC },
        { "struct",            STRUCT },
        { "AUTO_COMMAND",      AUTO_COMMAND },
    };
    int c;
    char tok[4096];
    char *i;
    int j;
    int newline;
    int first_line;
// think of the gotos as a tail recursion, otherwise
// every comment, preprocessor, etc. would push unnecessary
// contxt onto the stack.
yylex_start: 
    i = tok;
    first_line = yylloc.first_line = ctxt->parse_line;
    newline = 0;
    while ((c = getc(ctxt->fp)) != EOF && isspace(c))
    {
        if(c == '\n')
        {
            ctxt->parse_line++;
            newline = 1;
        }
    }
    yylloc.last_line = ctxt->parse_line;

    if(newline && c == '#')
    {
        char last = 0;
        // eat preprocessor (could do in grammer, but what the hey)
        // if line ends with \, continue to eat.
        for(;;)
        {
            while((c = getc(ctxt->fp)) != '\n' && c != EOF)
            {
                if(!isspace(c))
                    last = c;
            }
            if(last == '\\')
                ctxt->parse_line++;
            else
                break;
        }
        ungetc (c, ctxt->fp);
        goto yylex_start;
    }
    else if(c == '/') // take care of comments
    {
        c=getc(ctxt->fp);
        if(c == '/')
        {
            while((c = getc(ctxt->fp)) != '\n' && c != EOF)
                ; // empty
            ungetc (c, ctxt->fp);
            goto yylex_start;
        }
        else if(c == '*')
        {
            for(;;)
            {
                while((c = getc(ctxt->fp)) != '*' && c != EOF)
                {
                    if(c=='\n')
                        ctxt->parse_line++;
                }
                    
                if((c=getc(ctxt->fp)) == '/')
                    break;
                ungetc(c, ctxt->fp);
            }
            goto yylex_start;
        }
        ungetc(c, ctxt->fp);
    }
    else if(c == '\'')     // 'a', '\'', '\\'
    {
        c = getc(ctxt->fp);
        if(c == '\\')
            c = getc(ctxt->fp);
        yylval.num = c;
        c = getc(ctxt->fp);
        if(c != '\'')
            ungetc(c, ctxt->fp); // malformed, oh-well
        return CHAR_LITERAL;
    }
    else if(c == '"')
    {
        while((c=getc(ctxt->fp)) != EOF)
        {
            if(c == '\\') // \n, \t, \\, \<newline> etc.
            {
                *i++ = c;
                c = getc(ctxt->fp);
                while(c != EOF && c == ' ' || c == '\t' || c == '\r' || c == '\n')
                {
                    if(c == '\n')
                        ctxt->parse_line++;
                    c = getc(ctxt->fp);
                }

                if(c == EOF)
                    break;
                else
                    *i++ = c;
            }
            else if(c == '"')
                break;
            *i++ = c;
        }
        *i = 0;
        yylval.str = _strdup(tok);
        if(c != '"')
            ungetc(c,ctxt->fp);
        return STR;
    }

    // parse the token

    ungetc (c, ctxt->fp);
    while(isalnum(c = getc(ctxt->fp)) || c == '_')
        *i++ = c;
    *i = 0;
    if(!*tok) // no characters grabbed. return
        return c == EOF?0:c;
    ungetc(c,ctxt->fp);
    //   @todo -AB: destructor :10/25/08 
    yylval.str = _strdup(tok);
    for(j = 0; j < sizeof(kws)/sizeof(*kws); ++j)
    {
        if(0 == strcmp(kws[j].kw,tok))
            return kws[j].tok;
    }
    return TOK;
}

void yyerror (struct Parse *ctxt, char const *s)
{
    _snprintf(ctxt->parse_error, DIMOF(ctxt->parse_error), "%s(%i): %s\n", ctxt->parse_file, ctxt->parse_line, s);
}

char *tok_append(char *a,char *b)
{
    char *res;
    int na = strlen(a);
    int nb = strlen(b);
    res = malloc(na+nb+1);
    sprintf(res,"%s%s",a,b);
    return res;
}

static void print_token_value (FILE *file, int type, YYSTYPE value)
{
//    if (type == VAR)
    if(type >= 258)
        fprintf (file, "%s", value.str);
    else
        fprintf(file,"'%c'",type);
//     else if (type == NUM)
//         fprintf (file, "%d", value.val);
}
