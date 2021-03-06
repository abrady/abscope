/***************************************************************************
 *     Copyright (c) 2009-2009, Aaron Brady
 *     All Rights Reserved
 *
 * Module Description:
 * todo:
 * - build a call tree
 * - logging
 * - parse 0xabcd as a numeric constant
 * failing:
 * - MasterControlProgram/MCP_ControllerTracker.c/(245):bool UpdateControllerTrackerConnection(ControllerTrackerConnectionStatusStruct *pStatus, char **ppResultEString)
 * -   bool operator==(const soap_multipart_iterator& iter) const { return content == iter.content; }
 *
 * performance: 
 * - writing out file top offender
 * -- fwrite_nolock 18%
 * -- parse_write 17%
 * -- fwrite 12%
 * - memcpy 7%
 * - RtlEnterCriticalSection/RtlLeaveCriticalSection ~30%
 ***************************************************************************/
#include "c_parse.h"
#include "abserialize.h"
#include "errno.h"
#include "direct.h"
#include "abhash.h"
#include "abscope.h"
#include "locinfo.h"
#include "abarray.h"
#include "abfile.h"

int c_parse(CParse *ctxt);
extern int g_verbose;

void c_parse_init(CParse *cp)
{
	int i;
	if(!cp)
		return;

	cp->pool =  STRUCT_ALLOC(*cp->pool);
	for(i = 0; i < DIMOF(cp->parses); ++i)
		cp->parses[i].pool = cp->pool;
}


int c_parse_files(CParse *cp, char **files, int n_files)
{
    int i;
    int res = 0;

	if(!DEREF(cp,pool))
		c_parse_init(cp);

    for(i = 0; i < n_files; ++i)
    {
        char *fn = files[i];
        if(g_verbose)
            printf("%s\n",fn);
        
        if(c_ext(fn))
            res += c_parse_file(cp,fn);
    }
    return res;
}

int c_parse_file(CParse *cp, char *fn)
{
	LocInfo *file = NULL;
    int parse_res = 0;

	if(!DEREF(cp,pool))
		c_parse_init(cp);

    cp->parse_file = fn;
    cp->parse_line = 1;
    cp->fp = abfopen(cp->parse_file,File_R,0);
    
    if(!cp->fp)
    {
        printf("couldn't open file %s\n",cp->parse_file);
        return -1;
    }
    if(0 != (parse_res = c_parse(cp)))
    {
        printf("failed to parse file %s, error(%i):%s\n",cp->parse_file,parse_res,cp->parse_error);
    }
    file = parse_add_locinfof(&cp->srcfiles,fn,1,fn,fname_nodir(fn),fn,"file %s",fn);
    abfclose(cp->fp);

	// for parsed files, track the modified time for dynamic re-parsing
	if(file)
	{
		struct stat st = {0};
		if(0==stat(fn,&st))
		{
			SrcInfo *si = file->p = STRUCT_ALLOC(SrcInfo);
			if(si)
				si->st_mtime = st.st_mtime; // get the modified time
		}
	}

    return parse_res;
}

static void c_remove_file_from_parse(Parse *p, char *fn)
{
	int i;
	int *rms = 0;

	if(!p || !fn)
		return;

	for(i = 0; i < p->n_locs; ++i)
	{
		LocInfo *li = p->locs + i;
		if(0 == stricmp(li->fname, fn))
			aint_push(&rms,i);
	}
	
	for(i = aint_size(&rms)-1; i >= 0; --i)
	{
		int i_rm = rms[i];
		int n_rmd = aint_size(&rms) - 1 - i;
		int n_mov = p->n_locs - i_rm - n_rmd - 1;
		CopyStructs(p->locs+i_rm,p->locs+i_rm+1,n_mov);
	}

	aint_destroy(&rms);
}

// remove all locinfos associated with a particular file
void c_remove_files(CParse *cp, char **fns, int n_fns)
{
	int i;
	int j;

	for(i = 0; i < DIMOF(cp->parses); ++i)
	{
		for(j = 0; j < n_fns; ++j)
			c_remove_file_from_parse(cp->parses+i,fns[j]);
	}
}


// 
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
		if(q->tag && *q->tag)
			hash_insert(&ht,q->tag,q);
    }    

    for(i = 0; i < c->n_locs; ++i)
    {
        LocInfo *l = c->locs + i;
        LocInfo *q = 0;
		if(l->referrer && *l->referrer)
			q = hash_find(&ht,l->referrer);
        if(!q)
            continue;
        l->ref = q;
    }
    hash_cleanup(&ht,NULL);
}

static void c_do_fixups(CParse *cp)
{
    fixup_refs(&cp->funcrefs,&cp->funcs);     // point all funcrefs at their funcs
    fixup_refs(&cp->structrefs,&cp->structs); // point all the refs at their struct
	fixup_refs(&cp->structmbrs,&cp->structs); // point all the mbrs at their struct
}



// helper for writing out useful tags to be used in emacs
// for auto-completing requests to abscope
static int c_write_tags(CParse *cp)
{
	FILE *fp = fopen("tags.el","w");
	if(!fp)
	{
		fprintf(stderr,"could't open tags.el for writing, err:%s\n",last_error());
		return -1;
	}	

	fprintf(fp,"'(\n");
	write_parse_tags(fp,"structs",&cp->structs);
//		write_parse_tags(fp,"structrefs",&cp->structrefs);
	write_parse_tags(fp,"structmbrs",&cp->structmbrs);
	write_parse_tags(fp,"funcs",&cp->funcs);
//		write_parse_tags(fp,"funcrefs",&cp->funcrefs);
	write_parse_tags(fp,"defines",&cp->defines);
	write_parse_tags(fp,"enums",&cp->enums);
//		write_parse_tags(fp,"vars",&cp->vars);
	write_parse_tags(fp,"cryptic",&cp->cryptic);
	write_parse_tags(fp,"srcfiles",&cp->srcfiles);
		
	fprintf(fp,"\n)\n");
	fclose(fp);
	return 0;
}

#define PROJFILE_VERSION 0x20100112
static void c_write_projfile(CParse *cp)
{
	FILE *fp = fopen(".abs/project.txt","wb");
	int i;

	if(!fp)
	{
		fprintf(stderr,"couldn't open project file for writing\n");
		return;
	}

	fprintf(fp,"VERSION %i\n",PROJFILE_VERSION);

	for(i = 0; i < cp->srcfiles.n_locs; ++i)
	{
		LocInfo *li = cp->srcfiles.locs + i;
		SrcInfo *fi = li->p;
		if(!fi)
			continue;
		fprintf(fp,"%s\t%i\n",li->fname,fi->st_mtime);
	}
	fclose(fp);	
}

static void c_load_projfile(CParse *cp)
{
	int ver;
	FILE *fp = fopen(".abs/project.txt","rb");
	char fn[1024];
	time_t mt = 0;

	if(!fp)
	{
		fprintf(stderr,"couldn't open project file for writing\n");
		return;
	}
	
	if(1 != fscanf(fp,"VERSION %i\n",&ver)
	   || ver != PROJFILE_VERSION)
	{
		fclose(fp);
		fprintf(stderr,"project version mismatch, %i!=%i",ver,PROJFILE_VERSION);
		return;
	}	

	// slurp up the src names with the saved modified times
	// so we can use it later.
	
	while(2==fscanf(fp,"%s\t%i\n",fn,&mt))
	{
		char *str;
		SrcInfo *si = hash_find(&cp->project.srcinfo_from_str,fn);
		if(si)
		{
			fprintf(stderr,"duplicate srcfile %s, skipping",fn);
			continue;
		}

		si = STRUCT_ALLOC(*si);
		if(!si)
			continue;
		
		str = strpool_add(cp->pool,fn);
		si->st_mtime = mt;
		hash_insert(&cp->project.srcinfo_from_str,str,si);
	}

	fclose(fp);	
}

int c_parse_write(CParse *cp)
{   
    int res = 0;

    c_do_fixups(cp);

	res = _mkdir(".abs");
	if(res && errno != EEXIST)
	{
		fprintf(stderr,"couldn't make .abs dir\n");
		return res;
	}
	res = 0;

	if(0!=strpool_write(".abs/strpool.abs",cp->pool))
	{
		fprintf(stderr,"strs failed to write.\n");
		return -1;
	}
	
#define WRITE_PARSE(P)  \
	{					\
		printf("wrote %i " #P, cp->P.n_locs);			\
		if(0 != parse_filewrite(".abs/c_" #P ".abs",&cp-> P))	\
		{														\
			printf("FAILED");									\
			res = -1;											\
		}														\
		printf("\n");											\
	}
	
	WRITE_PARSE(structs);
	WRITE_PARSE(structrefs);
	WRITE_PARSE(structmbrs);
	WRITE_PARSE(funcs);
	WRITE_PARSE(funcrefs);
	WRITE_PARSE(defines);
	WRITE_PARSE(enums);
	WRITE_PARSE(vars);
	WRITE_PARSE(srcfiles);
	WRITE_PARSE(cryptic);

	printf("writing project file\n");
	c_write_projfile(cp);

	c_write_tags(cp);
    return res;
}

int c_parse_load(CParse *cp)
{
	int i;
    int res = 0;
	SerializeCtxt ctxt = {0}; 
	StrPool *pool =  STRUCT_ALLOC(*pool);

	serializectxt_init(&ctxt);

	for(i = 0; i < DIMOF(cp->parses); ++i)
		cp->parses[i].pool = pool;

	// currently after loading everything else. the smart thing would be
	// to load this first so we
	c_load_projfile(cp);	

	if(0 != strpool_read(".abs/strpool.abs",pool,&ctxt))
		return -1; 


#define READ_PARSE(P)													\
		{																\
			if(g_verbose) printf("reading " #P);						\
			if(file_exists(".abs/c_" #P ".abs") && 0 != parse_fileread(".abs/c_" #P ".abs",&cp-> P, &ctxt))	\
			{															\
				printf(".abs/c_" #P ".abs read FAILED\n");				\
				res = -1;												\
			}															\
			if(g_verbose) printf(" %i locs\n",cp->P.n_locs);			\
	}

	READ_PARSE(structs);
	READ_PARSE(structrefs);
	READ_PARSE(structmbrs);
	READ_PARSE(funcs);
	READ_PARSE(funcrefs);
	READ_PARSE(defines);
	READ_PARSE(enums);
	READ_PARSE(vars);
	READ_PARSE(srcfiles);
	READ_PARSE(cryptic);

	for(i = 0; i < DIMOF(cp->parses); ++i)
		res += parse_fixup(cp->parses+i,&ctxt);

    c_do_fixups(cp); // intra Parse things like struct members to structs

	serializectxt_cleanup(&ctxt);

    return res;
}

int c_findstructs(CParse *cp, char *sn, LocInfoField flds)
{
    return parse_print_search(&cp->structs,sn,flds);
}

int c_findstructrefs(CParse *cp, char *sn, LocInfoField flds)
{
    return parse_print_search(&cp->structrefs,sn,flds);
}

int c_findstructmbrs(CParse *cp, char *sn, LocInfoField flds)
{
    return parse_print_search(&cp->structmbrs,sn,flds);
}

int c_findfuncs(CParse *cp, char *name, LocInfoField flds)
{
    return parse_print_search(&cp->funcs,name,flds);
}

int c_findfuncrefs(CParse *cp, char *sn, LocInfoField flds)
{
    return parse_print_search(&cp->funcrefs,sn,flds);
}

int c_finddefines(CParse *cp, char *sn, LocInfoField flds)
{
    return parse_print_search(&cp->defines,sn,flds);
}

int c_findenums(CParse *cp, char *sn, LocInfoField flds)
{
    return parse_print_search(&cp->enums,sn,flds);
}

int c_findsrcfiles(CParse *cp, char *sn, LocInfoField flds)
{
    return parse_print_search(&cp->srcfiles,sn,flds);
}

int c_findvars(CParse *cp, char *sn, LocInfoField flds)
{
    return parse_print_search(&cp->vars,sn,flds);
}

int c_findcryptic(CParse *cp, char *sn, LocInfoField flds)
{
    return parse_print_search(&cp->cryptic,sn,flds);
}

int c_query(CParse *cp, char *tag, int query_flags, LocInfoField flds)
{
    int res = 0;
    printf("'(\n");
    if(query_flags & CQueryFlag_Structs)
        res += c_findstructs(cp,tag,flds);
    if(query_flags & CQueryFlag_Enums)
        res += c_findenums(cp,tag,flds);
    if(query_flags & CQueryFlag_Funcs)
        res += c_findfuncs(cp,tag,flds);
    if(query_flags & CQueryFlag_Defines)
        res += c_finddefines(cp,tag,flds);
    if(query_flags & CQueryFlag_Structrefs)
        res += c_findstructrefs(cp,tag,flds);
    if(query_flags & CQueryFlag_Structmbrs)
        res += c_findstructmbrs(cp,tag,flds);
    if(query_flags & CQueryFlag_Funcrefs)
        res += c_findfuncrefs(cp,tag,flds);
    if(query_flags & CQueryFlag_Srcfile)
        res += c_findsrcfiles(cp,tag,flds);
    if(query_flags & CQueryFlag_Vars)
        res += c_findvars(cp,tag,flds);
    if(query_flags & CQueryFlag_Cryptic)
        res += c_findcryptic(cp,tag,flds);
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
    REDUCED_CRYPTIC_AUTO,
    AUTO_COMMAND,   // ACMD_NAME(foo)
    AUTO_COMMAND_REMOTE,
    AUTO_COMMAND_REMOTE_SLOW,
    AUTO_COMMAND_QUEUED,
    AUTO_CMD_INT,    // (var,cmd_name)
    AUTO_CMD_FLOAT,  // (var,cmd_name)
    AUTO_CMD_STRING, // (var,cmd_name)
    AUTO_CMD_SENTENCE,
    AUTO_EXPR_FUNC,  // (UIGen) ACMD_NAME(foo)
	AUTO_RUN,
	AUTO_RUN_ANON,
    AUTO_STARTUP,
    NOCONST_DECL,
    ACMD_NAME,       // (foo)
    AUTO_ENUM,

	// gluax.h
	GLUA_FUNC,  // GLUA_FUNC(foo) {
//	GLUA_END,   // } GLUA_END *ignored*

    EIGNORE,
} c_tokentype;
#define C_KWS_START TYPEDEF
#define IS_INTRINSIC_TYPE(T) INRANGE(T,CHAR_TOK,VOID_TOK+1)
#define INTRINSIC_TYPE CHAR_TOK: case SHORT_TOK: case INT_TOK:case LONG_TOK:case SIGNED:case UNSIGNED:case FLOAT_TOK:case DOUBLE:case VOID_TOK

#define PREV_TOK(A) ((p->n_stack >= 2) && top[-1].tok == A)
#define PREV_TOKS2(A,B) ((p->n_stack >= 3) && top[-2].tok == A && top[-1].tok == B)
#define PREV_TOKS3(A,B,C) ((p->n_stack >= 4) && top[-3].tok == A && top[-2].tok == B && top[-1].tok == C)

#define POP() pop_tok(p)
#define POP_TO(N) pop_tok_to(p,N)

#define NEXT_TOK()                                                      \
    if(p->n_stack >= MAX_STACK)                                         \
    {                                                                   \
        parser_error(p,top,"out of room on stack in %s. aborting.",__FUNCTION__); \
        break;                                                          \
    }                                                                   \
    top = get_tok(p);                                                   \
    if(!top->tok)                                                       \
        break;															\
	if(g_verbose)														\
		printf("TOK %s",top->tok_str);



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
	char tok_str[32]; // for debugging
    c_tokentype tok;
    int lineno;
} StackElt;


static LocInfo* c_add_struct(CParse *p, char *struct_name, int lineno)
{
    return parse_add_locinfof(&p->structs,p->parse_file,lineno,p->line,struct_name,NULL,"struct %s", struct_name);
}

static void c_add_structmbr(CParse *p, LocInfo *s, StackElt *mbr, StackElt *mbr_type)
{
    parse_add_locinfof(&p->structmbrs,p->parse_file,mbr->lineno,p->line,mbr->l.str,mbr_type->l.str,"struct %s", s->tag);
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

// usually for an AUTO_... function
static void c_add_cryptic(CParse *p, StackElt *elt, char *ref)
{
    char buf[sizeof(elt->l.str)];
    char buf2[sizeof(elt->l.str)];
    char *tmp;
    // a little hacky, but split the line and tag up.
    stracpy(buf,elt->l.str);
    tmp = strchr(buf,':');
    if(tmp)
    {
        *tmp++ = 0;
        stracpy(buf2,tmp);
    }
    if(!*buf)
        stracpy(buf,ref);
    parse_add_locinfo(&p->cryptic,p->parse_file,elt->lineno,p->line,buf,buf2,ref);
}


int c_debug;
int c_lex(CParse *p, StackElt *top);

ABINLINE StackElt* get_tok(CParse *p)
{
    StackElt *top;
    if(p->n_stack < p->m_stack)
        return p->stack + p->n_stack++;
    ++p->m_stack;
    top = p->stack + p->n_stack++;
    ZeroStruct(top);
    top->tok = c_lex(p,top);
    return top;
}

static void pop_tok(CParse *p)
{
    assert(p->n_stack > 0);
    assert(p->m_stack >= p->n_stack);
    if(p->m_stack > p->n_stack)
    {
        int n = p->m_stack - p->n_stack;
        StackElt *dst = p->stack + p->n_stack-1;
        StackElt *src = dst+1;
        CopyStructs(dst,src,n);
    }
     p->n_stack--;
    p->m_stack--;
}

static void pop_tok_to(CParse *p, int n)
{
    while(p->n_stack > n)
        pop_tok(p);
}

// saves existing elements above the one on the stack.
static StackElt* unget_tok(CParse *p)
{
    if(p->n_stack <= 0)
        return NULL;
    assert(p->m_stack >= p->n_stack);
    return p->stack + --p->n_stack;
}

// unget to a point on the stck
static StackElt* unget_tok_to(CParse *p, int to)
{
    StackElt*r = NULL;
    if(p->n_stack <= 0)
        return NULL;
    while(p->n_stack > to)
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
    StackElt tmp;
    if(top)
        tmp = *top;
    POP_TO(to+1);
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


// matches pairing tokens like '{' and '}'
// doesn't use stack for error cases
ABINLINE void parse_to_tok(CParse *p, int tok, int open_tok)
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

// extern void foo();
// {typedef | extern | static | auto | register
// | type (builtins, TOK)
// | struct name? { ... }
// | const | restrict | volatile}* ;
//static void parse_declaration(CParse *p)
//{
//}

static int parse_expr(CParse *p, char *ctxt, int terminating_tok, int terminating_tok2);

static void parse_arg_expr(CParse *p, char *ctxt)
{
    int t;
    do
    {
        t = parse_expr(p,ctxt,',',')'); // puts last tok on stack
    } while(t && t != ')');
}




// expressions: expr, expr
// assignment expression:
// cond_expr:
//   or_expr '?' expr ':' cond_expr
//   or_expr
// or_expr: rel_expr {==,!=,^,|,&&,||,<,>,<=,>=,<<,>>,+,-,*,/,%}?  rel_expr?
// rel_expr:
//  '(' TOK ')' unary_expr // cast
//  unary_expr
// unary_expr: 
//  postfix_expr
//  {++,--,&,*,+,-,~,!} unary_expr
//  SIZEOF unary_expr   // could include '(' ... ')'
// postfix_expr:
//  TOK
//  '(' expr ')' 
//  postfix_expr '[' expr ']'
//  postfix_expr '(' arg_expr_list ')'
//  postfix_expr '.' TOK
//  postfix_expr ->  TOK
//  postfix_expr '++'
//  postfix_expr '--'
static int parse_expr(CParse *p, char *ctxt, int terminating_tok, int terminating_tok2)  
{
    StackElt *top = NULL;
    int n_stack_in = p->n_stack;
    int res = 0;
    
    for(;;)
    {
        NEXT_TOK();

        if(top->tok==terminating_tok || top->tok == terminating_tok2)
        {
            res = top->tok;
            break;
        }
        
        switch(top->tok)
        {
        case TOK: // keep this around
        {
            char *ref = NULL;
            if((top[-1].tok == PTR_OP || top[-1].tok == '.') && top[-2].tok == TOK)
                ref = top[-2].l.str;
            c_add_var(p,top,ref,ctxt);
        }
        break;
        case '(': // funcall, typecast, parenthesized expr
            if(top[-1].tok == TOK)
                c_add_funcref(p,top-1,ctxt);
            parse_arg_expr(p,ctxt);
            POP_TO(n_stack_in);
            break;
        case '[':
            parse_expr(p,ctxt,']',0);
            POP_TO(n_stack_in);
            break;
        case PTR_OP:
        case '.':
            break;
        case EQ_OP:
        case NE_OP:
        case '^':
        case '|':
        case AND_OP:
        case OR_OP:
        case '<':
        case '>':
        case LE_OP:
        case GE_OP:            
        case LEFT_OP:
        case RIGHT_OP:
        case '/':
        case '%':
        case '&': // unary ops
        case '*':
        case '+':
        case '-':
        case '~':
        case '!':
        case SIZEOF:
        case DEC_OP:
        case INC_OP:
        case '?': // ternary op, just toss it
        case ':':
            POP_TO(n_stack_in);
            break;
        case ')':
            POP_TO(n_stack_in);
            break;
        case '}':
            POP_TO(n_stack_in);
            break;
        default:
            break;
        };
    }
    POP_TO(n_stack_in);
    return res;
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
        case TOK:
            c_add_enum(p,top->l.str,enum_typename,top->lineno);
            break;
        case '}': 
        case ',':
            if(top->tok == '}')
                goto done;
            POP_TO(n_stack_in);
            break;
        case '=':
            if('}' == parse_expr(p,enum_typename,',','}'))
                goto done;
            POP_TO(n_stack_in);
            break;
        }
    }
done:
    POP_TO(n_stack_in);
}

// 
//static void extract_vars(CParse *p, StackElt *s, StackElt *e)

static int parse_var_initializer(CParse *p, char *ctxt);


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
            top->tok = parse_var_initializer(p,ctxt);
            // fall thru
        case ',': // fall thru
        case ';': 
            if(last_tok == last_vartype)
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

// stmt:
// label : terminated by ':'
// compnd: { stmt* }
// expr  : terminated by ';'
// select: if, if else, switch: term by '}' or ';'
// iter  : while(), do-while();, for()
// jump  : goto TOK, continue, break, return ';'

// compound stmt:
// declaration
// stmt
static void parse_func_body(CParse *p, char *func_ctxt)
{
    StackElt *top = NULL;
    int n_stack_in = p->n_stack;
    StackElt *beg = p->stack + n_stack_in;

    parse_var_decls(p,func_ctxt);
    for(;;)
    {
        NEXT_TOK();

        if(top->tok == '}')
            break;

        switch(top->tok)
        {
        case ':':
            if(beg->tok == CASE)
            {
                unget_tok_to(p,n_stack_in+1);
                parse_expr(p,func_ctxt,':',0); // expressions are a superset of what is allowed, but should parse this just fine
            }
            POP_TO(n_stack_in);
            break;
        case '{':
            parse_func_body(p,func_ctxt);
            POP_TO(n_stack_in);
            break;
        case ';':
            unget_tok_to(p,n_stack_in);
            parse_expr(p,func_ctxt,';',0);
            POP_TO(n_stack_in);
            break;
        case DO:
            POP_TO(n_stack_in);
            break;
        case WHILE:
        case IF:
            top = get_tok(p);
            if(DEREF(top,tok) != '(')
            {
                unget_tok(p);
                break;
            }
            parse_expr(p,func_ctxt,')',0);
            POP_TO(n_stack_in);
            break;
        case ELSE: // followed by '{', expr, or IF. handled in-loop
            POP_TO(n_stack_in);
            break;
        case FOR:
            top = get_tok(p);
            if(DEREF(top,tok) != '(')
            {
                unget_tok(p);
                break;
            }
            // for(a;b;c)
            parse_expr(p,func_ctxt,';',0);
            parse_expr(p,func_ctxt,';',0);
            parse_expr(p,func_ctxt,')',0);
            POP_TO(n_stack_in);
            break;
        case GOTO:
            get_tok(p);
        case CONTINUE:
        case BREAK:
            POP_TO(n_stack_in);
            break;
        case RETURN:
            parse_expr(p,func_ctxt,';',0);
            POP_TO(n_stack_in);
            break;
//         case '(': // stand-alone parenthesized expr (valid?)
//             if(top[-1].tok == TOK)
//                 c_add_funcref(p,top-1,func_ctxt);
//             parse_expr(p,func_ctxt,')',0);
//             POP_TO(n_stack_in);
//             break;
//         case '=':
//             unget_tok_to(p,n_stack_in);    // move back and parse expr
//             parse_expr(p,func_ctxt,'=',0);
//             top = get_tok(p);
//             assert(top->tok == '=');
//             parse_expr(p,func_ctxt,';',0);
//             POP_TO(n_stack_in);
//             break;
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

static void parse_cryptic_auto_decl(CParse *p)
{
    StackElt *top = NULL;
    int n_stack_in = p->n_stack;
    char *res = NULL;
    
    for(;;)
    {
        NEXT_TOK();        
        if(top->tok == ';')
        {
            str_catf(&res,":%s",p->line);
            break;
        }
        
        switch(top->tok)
        {
        case ')':
            if(p->n_stack - n_stack_in < 3)
                break;
            if(top[-3].tok != ACMD_NAME || top[-2].tok != '(')
                break;
            if(top[-1].tok != STR && top[-1].tok != TOK)
                break; // ACMD_NAME can take a quoted or unquoted str
            str_cat(&res,top[-1].l.str);
            break;
        default:
        break;
        }
    }

    if(top)
    {
        top->tok = REDUCED_CRYPTIC_AUTO;
        if(res)
            stracpy(top->l.str,res);
        reduce_to(p,n_stack_in,top);
    }
    if(res)
        free(res);
}

// func return type, var type, etc.
// decl_spec:
//  stor_spec decl_spec?
//  type_spec decl_spec?
//  type_qual decl_spec?
//
// stor_spec: TYPEDEF, EXTERN, STATIC, AUTO
// type_spec: CHAR, SHORT, struct, enum, user-defined, ...
// type_qual: CONST, VOLATILE
static void parse_declspec(CParse *p, char *ctxt, int term_tok1, int term_tok2)
{
    StackElt *top = NULL;
    StackElt *type = NULL;
    int n_stack_in = p->n_stack;
    StackElt *beg = p->stack + n_stack_in;

    while(!type)
    {
        NEXT_TOK();

        if(top->tok == term_tok1 || top->tok == term_tok2)
            break;

        switch(top->tok)
        {
        case EXTERN:
        case TYPEDEF:
            // don't care about these types
            POP_TO(n_stack_in);
            beg->tok = 0;
            return;
        case STATIC:
        case AUTO:
//        case CONST:
//         case VOLATILE:
            break;
        case STRUCT:
        case UNION:
        {
            LocInfo *l = NULL;
            top = get_tok(p); // get optional ID
            if(top && top->tok != '{')
            {
                type = top;
                top = get_tok(p);
            }
            else
            {
                type = top-1;
                sprintf_s(SSTR(type->l.str),"_anonymous_%s",ctxt);
            }
            l = c_add_struct(p,type->l.str,type->lineno);
            parse_struct_body(p,l);
        }
        break;
        case TOK: // assume type name
            type = top;
            break;
        }
    }
    reduce_to(p,n_stack_in,type);
}

// initializer:  
//  expr 
//  '{' initializer (',' initializer)* ','? '}'
static int parse_var_initializer(CParse *p, char *ctxt)
{
    int res = 0;
    StackElt *top = NULL;
    int n_stack_in = p->n_stack;    
    int done = 0;
    do
    {
        NEXT_TOK();
        
        if(top->tok == ';' || top->tok == ',')
        {
            res = top->tok;
            break;
        }
        
        switch(top->tok)
        {
        case TOK:
            unget_tok(p);
            res = parse_expr(p,ctxt,';',',');
            done = 1;
            break;
        case '{':
            parse_expr(p,ctxt,'}',0);
            break;
        }
    } while(!done);
    POP_TO(n_stack_in);
    return res;
}




// init_decl_list:
//  init_decl (',' init_decl_list)*
//
// init_decl:
//  decl ('=' initializer)?
//
// decl:
//  ptr? direct_decl
//
// direct_decl:
//  TOK
//  '(' decl ')'
//  direct_decl '[' expr? ']'
//  direct_decl '(' parm_type_list? ')'
//  direct_decl '(' ident_list? ')'
//
// parm_type_list: parm_type (',' parm_type)*
// parm_type:
//  decl_spec decl,
//  decl_spec abstract_decl, (not important for tags)
//  decl_spec,
//
// ident_list: TOK (',' TOK)*
//
static void parse_initdecllist(CParse *p, char *ctxt, char *type)
{
    StackElt *top = NULL;
    int n_stack_in = p->n_stack;

    if(!type)
        type = "";

    for(;;)
    {
        NEXT_TOK();

        if(top->tok == ';')
            break;

        switch(top->tok)
        {
        case '=':
            parse_var_initializer(p,ctxt);
            POP_TO(n_stack_in);
            break;
        case TOK:
            c_add_structref(p,top,type,ctxt);
            POP_TO(n_stack_in);
            break;
        case '(':
            // some crazy funcptr or something. skip it
            parse_to_tok(p,')','(');
            POP_TO(n_stack_in);
            break;
        case '[':
            parse_expr(p,ctxt,']',0); 
            break;
        }
    }
}


// top: 
//   declaration
//   func_def
// declaration:
//  decl_spec {init}
//
// func_def:
//   decl_spec declr decl_list '{' ... '}'
//   decl_spec declr 
//
// declr:
//  ptr* declr
//  TOK
//  '(' declr ')'
//  declr '[' expr ']'
//  declr '(' parm_type_list ')'
//  declr '(' id_list ')'
//  declr '(' ')'
//
// parm_type_list:
//  parm_decl, ...?
//
// parm_decl:
//  decl_spec declr
//  decl_spec abstract_decl
//
// abstract_decl:
//  ptr
//  direct_abstract_declarator
//  ptr direct_abstract_declarator
//
// direct_abstract_declarator:
//  '(' abstract_decl ')'
//  '[' expr? ']'
//  direct_abstract_declarator '[' expr? ']'
//
// boy is this complicated :P. all I really care about are type
// references.
int c_parse(CParse *p)
{
    StackElt *type;
    char ctxt[128];
    char ctxt2[128];
    StackElt stack[MAX_STACK] = {0}; 
    StackElt *top = NULL;
    int res = 0;
    char *s;

	if(!DEREF(p,pool))
		c_parse_init(p);

    p->stack  = stack;
    p->m_stack = 0;
    p->n_stack = 0;
    
    // the top loop is really either:
    // declarations : extern foo();, struct Bar, int baz, enum trois
    // function definitions.
    for(;;)
    {
        NEXT_TOK();
        switch(top->tok)
        {
        case ';':
            unget_tok_to(p,0);
            parse_declspec(p,"global_var",';','=');
            type = get_tok(p);
            if(IS_INTRINSIC_TYPE(type->tok) || type->tok == TOK)
                parse_initdecllist(p,"global var",type->l.str);
            else
                parse_to_tok(p,';',0);
            POP_TO(0);
            break;
        case '{':
            s = top[-1].l.str;
            if(PREV_TOKS2(STRUCT, TOK)) // struct def
            {
                LocInfo *l;

                if(p->stack[0].tok==REDUCED_CRYPTIC_AUTO)
                    c_add_cryptic(p,p->stack+0,s);

                l = c_add_struct(p,s,top[-1].lineno);
                parse_struct_body(p,l);
                POP_TO(0);
            }
            else if(PREV_TOKS2(ENUM,TOK)) // enum def
            {
                if(p->stack[0].tok==REDUCED_CRYPTIC_AUTO)
                    c_add_cryptic(p,p->stack+0,s);
                c_add_enum_decl(p,s,top[-1].lineno);
                parse_enum_body(p,s);
                parse_to_tok(p,';',0);
                POP_TO(0);
            }
            else if(top[-1].tok == ')') // function def
            {
                int n_parens = 1;
				char *line = NULL;

                // guess to find function name
                for(type = top-2;type > p->stack && n_parens;--type)
                {
                    if(type->tok == ')')
                        n_parens++;
                    else if(type->tok == '(')
                        n_parens--;
                }

                if(type > p->stack)
                {
                    strcpy(ctxt,type->l.str);
                    s = ctxt;
                }
                
                if(p->stack[0].tok==REDUCED_CRYPTIC_AUTO)
                    c_add_cryptic(p,p->stack+0,s);

				// dodgy: detect snowplow braces vs. next line
				if(*p->last_line)
					line = p->last_line;
				else
					line = p->line;

				// special case section
				if(!*s)
				{
					if(top[-3].tok == GLUA_FUNC) // func declared: GLUA_FUNC(foo){}
						s = top[-1].l.str;
				}
				
                c_add_funcdef(p,top[-1].lineno,s,line);

                unget_tok_to(p,0);
                parse_declspec(p,s,'{',0);
                type = get_tok(p);
                
                if(!type)
                {
                    parser_error(p,top,"unable to parse this.");
                    parse_to_tok(p,'}','{');
                    break;
                }

                // get back to where we started, parse decl
                parse_to_tok(p,'(',0);
                parse_arglist(p,s);

                // parse body
                parse_to_tok(p,'{',0);
                sprintf(ctxt2,"func %s",s);
                parse_func_body(p,ctxt2);
                POP_TO(0);
            }
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
        case AUTO_COMMAND:
        case AUTO_COMMAND_REMOTE:
        case AUTO_COMMAND_REMOTE_SLOW:
        case AUTO_COMMAND_QUEUED:
        case AUTO_EXPR_FUNC:
        case AUTO_STARTUP:
        case AUTO_ENUM:
		case AUTO_RUN:
		case AUTO_CMD_SENTENCE:
		case AUTO_CMD_STRING:
		case AUTO_CMD_FLOAT:
		case AUTO_CMD_INT:
            parse_cryptic_auto_decl(p);
            top = get_tok(p);
            reduce_to(p,0,top);
            break;
		case AUTO_RUN_ANON:
			parse_to_tok(p,')','('); // parse paren expression
			break; // TODO: special case
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
    { "AUTO_COMMAND",             AUTO_COMMAND },
    { "NOCONST",            NOCONST_DECL },
//        { "const",              CONST_DECL },
    { "AUTO_COMMAND_REMOTE",      AUTO_COMMAND_REMOTE },
    { "AUTO_COMMAND_REMOTE_SLOW", AUTO_COMMAND_REMOTE_SLOW },
    { "AUTO_COMMAND_QUEUED",      AUTO_COMMAND_QUEUED },
    { "AUTO_CMD_INT",             AUTO_CMD_INT },
    { "AUTO_CMD_FLOAT",           AUTO_CMD_FLOAT },
    { "AUTO_CMD_STRING",          AUTO_CMD_STRING },
    { "AUTO_CMD_SENTENCE",        AUTO_CMD_SENTENCE },
    { "AUTO_EXPR_FUNC",           AUTO_EXPR_FUNC },
    { "AUTO_STARTUP",             AUTO_STARTUP },
    { "ACMD_NAME",                ACMD_NAME },
    { "AUTO_ENUM",                AUTO_ENUM },
    { "AUTO_RUN",                 AUTO_RUN },
    { "AUTO_RUN_ANON",            AUTO_RUN_ANON },

	{ "GLUA_FUNC",                GLUA_FUNC },

    { "typedef",                  TYPEDEF },
    { "extern",                   EXTERN },
    { "static",                   STATIC },
    { "auto",                     AUTO },
    { "register",                 REGISTER },
    { "char",                     CHAR_TOK },
    { "short",                    SHORT_TOK },
    { "int",                      INT_TOK },
    { "long",                     LONG_TOK },
    { "signed",                   SIGNED },
    { "unsigned",                 UNSIGNED },
    { "float",                    FLOAT_TOK },
    { "double",                   DOUBLE },
//        { "const",              CONST },
//        { "volatile",           VOLATILE }, ignored, see ignored_kws
    { "void",                     VOID_TOK },
    { "struct",                   STRUCT },
    { "union",                    UNION },
    { "enum",                     ENUM },
    { "case",                     CASE },
    { "default",                  DEFAULT },
    { "if",                       IF },
    { "else",                     ELSE },
    { "switch",                   SWITCH },
    { "while",                    WHILE },
    { "do",                       DO },
    { "for",                      FOR },
    { "goto",                     GOTO },
    { "continue",                 CONTINUE },
    { "break",                    BREAK },
    { "return",                   RETURN },
    { "sizeof",                   SIZEOF },
    { "AST",                      AST},
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
    "EIGNORE",
	"GLUA_END"
};


// todo: parse '==' properly
int c_lex(CParse *p, StackElt *top)
{
    static HashTable tok_from_kw = {0};
    U32 hash;
    int c;
    char tok[4096] = {0};
    char *i;
    int j;
    int newline;
    S64 timer_start = timer_get();
    
//    S64 timer_getc;
// timing each getch call has too much overhead
// #define GETC() ((timer_getc = timer_get()),(p->line[(p->i_line++)%DIMOF(p->line)] = (char)(c = getc(p->fp))),(p->getc_timing += timer_diff(timer_getc)),c)

#define GETC() ((p->line[(p->i_line++)%DIMOF(p->line)] = (char)(c = abgetc(p->fp))),c)
#define UNGETC() (p->i_line--,abungetc(c, p->fp)) 
#define LEX_RET(VAL) { p->lex_timing += timer_diff(timer_start);	\
	p->line[(p->i_line)%DIMOF(p->line)] = 0;						\ 
	stracpy(top->tok_str,tok);					\
	return VAL;									\
    }

    if(!tok_from_kw.elts)
    {
        for(j = 0; j < DIMOF(kws); ++j)
            hash_insert(&tok_from_kw,kws[j].kw,(void*)kws[j].tok);
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
			{
				static char bp_fn[256] = "./Core/MasterControlProgram/MCP_ControllerTracker.c";
				static int bp_ln = 241;
				if(0==strcmp(bp_fn,p->parse_file) && bp_ln == p->parse_line)
					break_if_debugging();
			}
			
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
parse_str:
        while((c=GETC()) != EOF)
        {
            if(c == '\\') // \n, \t, \\, \<newline> etc.
            {
                GETC();

				// special case: \<newline> means 
				// concat next line
				if(c == '\r')
					GETC();
				if(c == '\n')
				{
					p->parse_line++;
					goto parse_str;
				}

				*i++ = '\\';
				if(c == EOF)
					break;
				*i++ = (char)c;
            }
            else if(c == '"')
                break;
			else
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
    //UNGETC();
    // isalnum : funcall overhead too expensive
    while((c>='0'&&c<='9') 
          || (c>='A'&&c<='Z') 
          || (c>='a'&&c<='z') 
          || c == '_')
    {
        *i++ = (char)c;
        GETC();
    }
    
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

    hash = tok_from_kw.hashfp(tok,tok_from_kw.ctxt);
    
    for(j = 0; j < DIMOF(ignored_kws); ++j)
        if(0 == strcmp(tok,ignored_kws[j]))
            goto yylex_start;

    stracpy(top->l.str,tok);
    for(j = 0; j < sizeof(kws)/sizeof(*kws); ++j)
    {
        HashNode *n = hash_findnode_prehash(&tok_from_kw,tok,hash);
        if(n)
        {
            if(c_debug)
                fprintf(stderr,"keyword(%s)\n",tok);
            LEX_RET((int)n->p);
        }
    }
    if(c_debug)
        fprintf(stderr,"TOK(%s)\n",tok);
    LEX_RET(TOK);
}

#define TEST(COND) if(!(COND)) {fprintf(stderr,#COND ": failed line(%i)\n",__LINE__); break_if_debugging(); return -1;}

int c_parse_readwrite_test()
{
	CParse *cp = STRUCT_ALLOC(*cp);
	CParse *cp2 = STRUCT_ALLOC(*cp2);
	int i;
	
	c_parse_init(cp);
	TEST(0==_chdir("./test"));

	for(i = 0; i < DIMOF(cp->parses); ++i)
	{
		char tmp[32];
		sprintf(tmp,"%3d",i);
		parse_add_locinfo(cp->parses+i,tmp,i,tmp,tmp,tmp,tmp);
	}

	TEST(0==c_parse_write(cp));
	TEST(0==c_parse_load(cp2));

	for(i = 0; i < DIMOF(cp->parses); ++i)
	{
		Parse *p,*p2;
		char tmp[32];
		LocInfo *l, *l2;
		sprintf(tmp,"%3d",i);
		p = cp->parses+i;
		p2 = cp2->parses+i;
		TEST(p->n_locs == p2->n_locs);
		l = p->locs;
		l2 = p2->locs;
		TEST(l->tag != l2->tag && 0==strcmp(l->tag,l2->tag));
		TEST(l->referrer != l2->referrer && 0==strcmp(l->referrer,l2->referrer));
		TEST(l->context != l2->context && 0==strcmp(l->context,l2->context));
		TEST(l->context != l2->context && 0==strcmp(l->context,l2->context));
		TEST(l->fname != l2->fname && 0==strcmp(l->fname,l2->fname));
		TEST(l->lineno == l2->lineno);
		TEST(l->line != l2->line && 0==strcmp(l->line,l2->line));
	}

	c_parse_cleanup(cp);
	c_parse_cleanup(cp2);

	TEST(0==_chdir(".."));

	free(cp);
	free(cp2);
	
	return 0;
}

BOOL dirscan_accept_c_files(char *path, char **ctxt)
{
    ctxt; // ignored
    return path && (match_ext(path,"c") || match_ext(path,"h"));
}


int c_parse_test()
{
    int i;
    StackElt stack[MAX_STACK] = {0};
    CParse *p = NULL;
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

    // ----------
    // test stack ops
    p = &cp;
    p->stack  = stack;
    for(i = 0; i < 10; ++i)
        p->stack[i].tok = i+1;
    p->n_stack = 10;
    p->m_stack = 10;
    
    unget_tok_to(p,6);
    POP_TO(1);
    POP();
    for(i = 0; i < 4; ++i)
    {
        StackElt *top = get_tok(p);
        TEST(top->tok == i+7);
    }
    
    
    ZeroStruct(p);
        
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
    TEST(li->lineno == start_line + 17);
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
    li = cp.structs.locs + 0;
    TEST(0==strcmp(li->tag,"Foo"));
    TEST(li->referrer == NULL);
    TEST(0==strcmp(li->context,"struct Foo"));
    TEST(0==stricmp(li->fname,"test/Foo.c"));

    li = cp.structs.locs + 1;
    TEST(0==strcmp(li->tag,"Bar"));
    TEST(0==stricmp(li->fname,"test/Foo.c"));
    TEST(0==strcmp(li->context,"struct Bar"));

    li = cp.structs.locs + 2;
    TEST(0==strcmp(li->tag,"Baz"));
    TEST(0==strcmp(li->context,"enum Baz"));

    li++;
    TEST(0==strcmp(li->tag,"Foo2"));
    TEST(0==strcmp(li->context,"struct Foo2"));

    INIT_LI(cp.structmbrs);
    TEST_LI("a", "int", "struct Foo");
    TEST_LI("b", "char", "struct Foo");
    TEST_LI("bar_a", "int",  "struct Bar");
    TEST_LI("baz_b", "char", "struct Bar");
    TEST_LI("hNameMsg", "Message",  "struct Foo2");
    TEST_LI("iSortID", "U32", "struct Foo2");
    TEST_LI("bSearchable", "bool", "struct Foo2");
    TEST_LI("eType", "ItemType", "struct Foo2");
    TEST(li == li_end);

    // func decls
    TEST(cp.funcs.n_locs >= 3);
    li = cp.funcs.locs + 0;
    TEST(0==strcmp(li->tag,"test_func"));
    TEST(0==stricmp(li->fname,"test/Foo.c"));
    TEST(0==strcmp(li->context,"func test_func"))
    
    li = cp.funcs.locs + 1;
    TEST(0==strcmp(li->tag,"CommonAlgoTables_Load"));
    TEST(0==stricmp(li->fname,"test/Foo.c"));
    TEST(0==strcmp(li->context,"func CommonAlgoTables_Load"));
    
    TEST(cp.defines.n_locs == 1);
    li = cp.defines.locs + 0;
    TEST(0==strcmp(li->tag,"FOO"));
    TEST(0==stricmp(li->fname,"test/Foo.c"));
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

    INIT_LI(cp.cryptic);
    TEST_LI("test_func_expr",       "AUTO_EXPR_FUNC(UIGen) ACMD_NAME('test_fu", "test_func");
    TEST_LI("CommonAlgoTables_Load", "AUTO_STARTUP(AlgoTablesCommon);", "CommonAlgoTables_Load");
    TEST_LI("test_func2_command", "AUTO_COMMAND ACMD_NAME(test_func2_co", "test_func2");
    TEST_LI("Acmd",               "AUTO_EXPR_FUNC(UIGen) ACMD_NAME('Acmd');", "exprAcmd");
    TEST(li == li_end);    

    // ----------
    // queries

    n_lis = parse_locinfos_from_context(&cp.vars,"func test_func3",&lis);
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
    TEST(n_lis == 12);

    INIT_LI(cp.srcfiles);
    TEST_LI("foo.c", "test/foo.c", "file test/foo.c");
    TEST(li == li_end);

    // TODO: do a final lineno test

	TEST(0==c_parse_readwrite_test());

    return 0;
}

void c_parse_cleanup(CParse *p)
{
	parse_cleanup(&p->structs);
	parse_cleanup(&p->structrefs);
	parse_cleanup(&p->structmbrs);
	parse_cleanup(&p->funcs);
	parse_cleanup(&p->funcrefs);
	parse_cleanup(&p->defines);
	parse_cleanup(&p->enums);
	parse_cleanup(&p->vars);
	parse_cleanup(&p->srcfiles);
	parse_cleanup(&p->cryptic);
}











