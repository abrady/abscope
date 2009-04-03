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
    int yylex (struct CParse *ctxt);
    void yyerror (struct CParse *ctxt, char const *);
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

%defines
%debug
%token-table


%parse-param {struct CParse *ctxt}
%lex-param   {struct CParse *ctxt}
%token<str> TYPEDEF EXTERN STATIC STRUCT AUTO_COMMAND
%token<str> META_PARAM
%token<str> TOK STR
%type<str>  type_decl str_literal
%type<num>  struct_decl_or_def

// %destructor { free( $$)  } string

%token TYPEDEF EXTERN STATIC 

%start translation_unit

%%

translation_unit:
                external_declaration
        |       external_declaration translation_unit
                ;

external_declaration:
                function_definition
        |       struct_decl
        |       autocmd_decl
        |       ignored_stuff
                ;

unary_operator: 
                '&'
        | '*'
        | '+'
        | '-'
        | '~'
        | '!'
                ;

assignment_operator:
                '='
        |  '*='
        |  '/='
        |  '%='
        |  '+='
        |  '-='
        |  '<<='
        |  '>>='
        |  '&='
        |  '^='
        |  '|='
                ;
ignored_stuff:
                '{'
        |       '}'
        |       ';'
        |       '['
        |       ']'
        |       '='
        |       '^'
        |       ','
        |       unary_operator
        |       str_literal
        |       TOK
        |       EXTERN
        |       STATIC
        |       assignment_operator
        ;

function_definition:
                type_decl TOK '(' function_args_opt ')' '{' /*{ printf("function def. ret(%s) fn(%s)\n", $2, $3); }*/
                ;

function_arg:   
                type_decl TOK { printf("arg(%s,%s)", $1, $2); }
                ;

function_args_opt:
                /*empty*/ { printf("no args. "); }
        |       function_arg ',' function_args_opt { printf(","); }
        |       function_arg
                ;

type_decl:
                TOK { $$ = $1; }
        |       type_decl  '*' {$$ = tok_append($1,"*"); }
                ;

struct_decl_or_def: 
                '{'     { $$ = 1; } // start of def
        |       TOK ';' { $$ = 0; } // just a forward decl
        ;

struct_decl:    TYPEDEF STRUCT TOK struct_decl_or_def { if($4) add_struct_decl(ctxt, $3); }

        ;

autocmd_decl:   AUTO_COMMAND
        ;

str_literal: 
                STR
        |       STR '\\' str_literal { char *tmp = realloc($1,strlen($1)+strlen($3)+1); $$ = strcat(tmp,$3); }
        |       STR str_literal { char *tmp = realloc($1,strlen($1)+strlen($2)+1); $$ = strcat(tmp,$2); }

%%  
struct CParse *ctxt;
int yylex (struct CParse *ctxt)
{
    static struct { char *kw; int tok; } kws[] = {
        { "typedef",           TYPEDEF },
        { "extern",            EXTERN },
        { "static",            STATIC },
        { "struct",            STRUCT },
        { "AUTO_COMMAND",      AUTO_COMMAND },
    };
    int c;
    char tok[128];
    char *i = tok;
    int j;
    int newline;

    newline = 0;
    while ((c = getc(ctxt->fp)) == ' ' || c == '\t' || c == '\n' || c == '\r')
    {
        if(c == '\n')
        {
            ctxt->parse_line++;
            newline = 1;
        }
        else
            newline = 0;
    }

    if(newline && c == '#')
    {
        // eat preprocessor (could do in grammer, but what the hey)
        while((c = getc(ctxt->fp)) != '\n' && c != EOF)
            ; // empty
        ungetc (c, ctxt->fp);
        return yylex(ctxt);
    }
    else if(c == '/') // take care of comments
    {
        c=getc(ctxt->fp);
        if(c == '/')
        {
            while((c = getc(ctxt->fp)) != '\n' && c != EOF)
                ; // empty
            ungetc (c, ctxt->fp);
            return yylex(ctxt);
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
            return yylex(ctxt);
        }
        ungetc(c, ctxt->fp);
    }
    else if(c == '"')
    {
        while((c=getc(ctxt->fp))!='"' && c != EOF)
            *i++ = c;
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

void yyerror (struct CParse *ctxt, char const *s)
{
    fprintf (stderr, "%s(%i): %s\n", ctxt->parse_file, ctxt->parse_line, s);
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
    if(type > 255)
        fprintf (file, "%s", value.str);
    else
        fprintf(file,"'%c'",type);
//     else if (type == NUM)
//         fprintf (file, "%d", value.val);
}
