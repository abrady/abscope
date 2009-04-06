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
// #define YYDEBUG 1
// #define YYINCLUDED_STDLIB_H

#define __STDC__ // ab: do this last, only needed by output of this
extern CParse *ctxt;
int yylex(YYSTYPE *lvalp, struct CParse *ctxt);
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
// yytoknum doesn't work with glr parsers for some reason
// #define YYPRINT(file, type, value) print_token_value (file, 0, value)

#pragma warning(push)         // push: hide all the gen code warnings
#pragma warning(disable:4127) // conditional constant e.g. {}while(0);
#pragma warning(disable:4244) // cast to smaller type (int to short)
#pragma warning(disable:4702) // unreachable code
#pragma warning(disable:4100) // unused parameter
#pragma warning(disable:4706) // assignment within conditional
#pragma warning(disable:4701) // potentially uninitialized var
#define inline __inline
%}

%debug
%pure-parser
%error-verbose
%verbose
%parse-param {CParse *ctxt}
%lex-param   {CParse *ctxt}
%defines                    // make a .h file
//                      %locations

%glr-parser

%token-table

// %destructor { free( $$)  } string

// Tokens
%token<str> IDENTIFIER CONSTANT STRING_LITERAL SIZEOF
%token PTR_OP INC_OP DEC_OP LEFT_OP RIGHT_OP LE_OP GE_OP EQ_OP NE_OP
%token AND_OP OR_OP MUL_ASSIGN DIV_ASSIGN MOD_ASSIGN ADD_ASSIGN
%token SUB_ASSIGN LEFT_ASSIGN RIGHT_ASSIGN AND_ASSIGN
%token XOR_ASSIGN OR_ASSIGN TYPE_NAME

%token TYPEDEF EXTERN STATIC AUTO REGISTER
%token CHAR_TOK SHORT_TOK INT_TOK LONG_TOK SIGNED_TOK UNSIGNED_TOK FLOAT_TOK DOUBLE_TOK CONST_DECL VOLATILE VOID_TOK
%token STRUCT UNION ENUM ELLIPSIS

%token CASE DEFAULT IF ELSE SWITCH WHILE DO FOR GOTO CONTINUE BREAK RETURN

%start translation_unit

%%

// *************************************************************************
 //  here we go
 // *************************************************************************

primary_expression
: IDENTIFIER
| CONSTANT
| STRING_LITERAL
| '(' expression ')'
;

postfix_expression
: primary_expression
| postfix_expression '[' expression ']'
| postfix_expression '(' ')'
| postfix_expression '(' argument_expression_list ')'
| postfix_expression '.' IDENTIFIER
| postfix_expression PTR_OP IDENTIFIER
| postfix_expression INC_OP
| postfix_expression DEC_OP
;

argument_expression_list
: assignment_expression
| argument_expression_list ',' assignment_expression
;

unary_expression
: postfix_expression
| INC_OP unary_expression
| DEC_OP unary_expression
| unary_operator cast_expression
| SIZEOF unary_expression
| SIZEOF '(' type_name ')'
;

unary_operator
: '&'
| '*'
| '+'
| '-'
| '~'
| '!'
;

cast_expression
: unary_expression
| '(' type_name ')' cast_expression
;

multiplicative_expression
: cast_expression
| multiplicative_expression '*' cast_expression
| multiplicative_expression '/' cast_expression
| multiplicative_expression '%' cast_expression
;

additive_expression
: multiplicative_expression
| additive_expression '+' multiplicative_expression
| additive_expression '-' multiplicative_expression
;

shift_expression
: additive_expression
| shift_expression LEFT_OP additive_expression
| shift_expression RIGHT_OP additive_expression
;

relational_expression
: shift_expression
| relational_expression '<' shift_expression
| relational_expression '>' shift_expression
| relational_expression LE_OP shift_expression
| relational_expression GE_OP shift_expression
;

equality_expression
: relational_expression
| equality_expression EQ_OP relational_expression
| equality_expression NE_OP relational_expression
;

and_expression
: equality_expression
| and_expression '&' equality_expression
;

exclusive_or_expression
: and_expression
| exclusive_or_expression '^' and_expression
;

inclusive_or_expression
: exclusive_or_expression
| inclusive_or_expression '|' exclusive_or_expression
;

logical_and_expression
: inclusive_or_expression
| logical_and_expression AND_OP inclusive_or_expression
;

logical_or_expression
: logical_and_expression
| logical_or_expression OR_OP logical_and_expression
;

conditional_expression
: logical_or_expression
| logical_or_expression '?' expression ':' conditional_expression
;

assignment_expression
: conditional_expression
| unary_expression assignment_operator assignment_expression
;

assignment_operator
: '='
| MUL_ASSIGN
| DIV_ASSIGN
| MOD_ASSIGN
| ADD_ASSIGN
| SUB_ASSIGN
| LEFT_ASSIGN
| RIGHT_ASSIGN
| AND_ASSIGN
| XOR_ASSIGN
| OR_ASSIGN
;

expression
: assignment_expression
| expression ',' assignment_expression
;

constant_expression
: conditional_expression
;

declaration
: declaration_specifiers ';'
| declaration_specifiers init_declarator_list ';'
;

declaration_specifiers
: storage_class_specifier
| storage_class_specifier declaration_specifiers
| type_specifier
| type_specifier declaration_specifiers
| type_qualifier
| type_qualifier declaration_specifiers
;

init_declarator_list
: init_declarator
| init_declarator_list ',' init_declarator
;

init_declarator
: declarator
| declarator '=' initializer
;

storage_class_specifier
: TYPEDEF
| EXTERN
| STATIC
| AUTO
| REGISTER
;

type_specifier
: VOID_TOK
| CHAR_TOK
| SHORT_TOK
| INT_TOK
| LONG_TOK
| FLOAT_TOK
| DOUBLE_TOK
| SIGNED_TOK
| UNSIGNED_TOK
| struct_or_union_specifier
| enum_specifier
| /*TYPE_NAME*/IDENTIFIER %dprec 2
;

struct_or_union_specifier
: struct_or_union IDENTIFIER '{' struct_declaration_list '}' { add_struct_decl(ctxt,$2); }
| struct_or_union '{' struct_declaration_list '}'
| struct_or_union IDENTIFIER
;

struct_or_union
: STRUCT
| UNION
;

struct_declaration_list
: struct_declaration
| struct_declaration_list struct_declaration
;

struct_declaration
: specifier_qualifier_list struct_declarator_list ';'
;

specifier_qualifier_list
: type_specifier specifier_qualifier_list
| type_specifier
| type_qualifier specifier_qualifier_list
| type_qualifier
;

struct_declarator_list
: struct_declarator
| struct_declarator_list ',' struct_declarator
;

struct_declarator
: declarator
| ':' constant_expression
| declarator ':' constant_expression
;

enum_specifier
: ENUM '{' enumerator_list '}'
| ENUM IDENTIFIER '{' enumerator_list '}'
| ENUM IDENTIFIER
;

enumerator_list
: enumerator
| enumerator_list ',' enumerator
;

enumerator
: IDENTIFIER
| IDENTIFIER '=' constant_expression
;

type_qualifier
: CONST_DECL
| VOLATILE
;

declarator 
: pointer direct_declarator
| direct_declarator         
;

direct_declarator
: IDENTIFIER                %dprec 1
| '(' declarator ')'
| direct_declarator '[' constant_expression ']'
| direct_declarator '[' ']'
| direct_declarator '(' parameter_type_list ')'
| direct_declarator '(' identifier_list ')'
| direct_declarator '(' ')'
;

pointer
: '*'
| '*' type_qualifier_list
| '*' pointer
| '*' type_qualifier_list pointer
;

type_qualifier_list
: type_qualifier
| type_qualifier_list type_qualifier
;

parameter_type_list
: parameter_list
| parameter_list ',' ELLIPSIS
;

parameter_list
: parameter_declaration
| parameter_list ',' parameter_declaration

parameter_declaration
: declaration_specifiers declarator
| declaration_specifiers abstract_declarator
| declaration_specifiers
;

identifier_list
: IDENTIFIER
| identifier_list ',' IDENTIFIER
;

type_name
: specifier_qualifier_list
| specifier_qualifier_list abstract_declarator
;

abstract_declarator
: pointer
| direct_abstract_declarator
| pointer direct_abstract_declarator
;

direct_abstract_declarator
: '(' abstract_declarator ')'
| '[' ']'
| '[' constant_expression ']'
| direct_abstract_declarator '[' ']'
| direct_abstract_declarator '[' constant_expression ']'
| '(' ')'
| '(' parameter_type_list ')'
| direct_abstract_declarator '(' ')'
| direct_abstract_declarator '(' parameter_type_list ')'
;

initializer
: assignment_expression
| '{' initializer_list '}'
| '{' initializer_list ',' '}'
;

initializer_list
: initializer
| initializer_list ',' initializer
;

statement
: labeled_statement
| compound_statement
| expression_statement
| selection_statement
| iteration_statement
| jump_statement
;

labeled_statement
: IDENTIFIER ':' statement
| CASE constant_expression ':' statement
| DEFAULT ':' statement
;

compound_statement
: '{' '}'
| '{' statement_list '}'
| '{' declaration_list '}'
| '{' declaration_list statement_list '}'
;

declaration_list
: declaration
| declaration_list declaration
;

statement_list
: statement
| statement_list statement
;

expression_statement
: ';'
| expression ';'
;

selection_statement
: IF '(' expression ')' statement
| IF '(' expression ')' statement ELSE statement
| SWITCH '(' expression ')' statement
;

iteration_statement
: WHILE '(' expression ')' statement
| DO statement WHILE '(' expression ')' ';'
| FOR '(' expression_statement expression_statement ')' statement
| FOR '(' expression_statement expression_statement expression ')' statement
;

jump_statement
: GOTO IDENTIFIER ';'
| CONTINUE ';'
| BREAK ';'
| RETURN ';'
| RETURN expression ';'
;

translation_unit
: external_declaration
| translation_unit external_declaration
;

external_declaration
: function_definition
| declaration
;

function_definition
: declaration_specifiers declarator declaration_list compound_statement
| declaration_specifiers declarator compound_statement
| declarator declaration_list compound_statement
| declarator compound_statement
;

%%  

#pragma warning(pop) // back to normal warnings state.

  // *************************************************************************
  // Start of Lexer  
  // *************************************************************************
struct CParse *ctxt;
int yylex (YYSTYPE /*out*/ *lvalp, struct CParse *ctxt)
{
    static struct { char *kw; int tok; } kws[] = {
//        { "identifier", IDENTIFIER },
//        { "constant", CONSTANT },
//        { "string_literal", STRING_LITERAL },
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
//        { "type_name", TYPE_NAME },
        
        { "typedef", TYPEDEF },
        { "extern", EXTERN },
        { "static", STATIC },
        { "auto", AUTO },
        { "register", REGISTER },
        
        { "char", CHAR_TOK },
        { "short", SHORT_TOK },
        { "int", INT_TOK },
        { "long", LONG_TOK },
        { "signed", SIGNED_TOK },
        { "unsigned", UNSIGNED_TOK },
        { "float", FLOAT_TOK },
        { "double", DOUBLE_TOK },
        { "const", CONST_DECL },
        { "volatile", VOLATILE },
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
    };
    int c;
    char tok[128];
    char *i = tok;
    int j;
    int newline;

    //@todo -AB: TYPE_NAME :04/02/09
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
        return yylex(lvalp,ctxt);
    }
    else if(c == '/') // take care of comments
    {
        c=getc(ctxt->fp);
        if(c == '/')
        {
            while((c = getc(ctxt->fp)) != '\n' && c != EOF)
                ; // empty
            ungetc (c, ctxt->fp);
            return yylex(lvalp,ctxt);
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
            return yylex(lvalp,ctxt);
        }
        ungetc(c, ctxt->fp);
    }
    else if(c == '"')
    {
        while((c=getc(ctxt->fp))!='"' && c != EOF)
            *i++ = (char)c;
        *i = 0;
        lvalp->str = _strdup(tok);
        if(c != '"')
            ungetc(c,ctxt->fp);
        return STRING_LITERAL;
    }

    // parse the token

    ungetc (c, ctxt->fp);
    while(isalnum(c = getc(ctxt->fp)) || c == '_')
        *i++ = (char)c;
    *i = 0;
    if(!*tok) // no characters grabbed. return
        return c == EOF?0:c;
    ungetc(c,ctxt->fp);
    //   @todo -AB: destructor :10/25/08 
    lvalp->str = _strdup(tok);
    for(j = 0; j < sizeof(kws)/sizeof(*kws); ++j)
    {
        if(0 == strcmp(kws[j].kw,tok))
            return kws[j].tok;
    }
    return IDENTIFIER;
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
