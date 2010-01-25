/***************************************************************************
 *     Copyright (c) 2009-2009, Aaron Brady
 *     All Rights Reserved
 *
 * Module Description:
 *
 *
 ***************************************************************************/
#include "locinfo.h"
#include "pcre.h"
#include "abserialize.h"

static S64 locinfo_timer = 0;
static S64 locinfo_read_timer = 0;
static S64 locinfo_write_timer = 0;
static S64 locinfo_parse_find_add_str_timer = 0;
#define LOCINFO_TIMER_START TIMER_START
#define LOCINFO_TIMER_END() TIMER_END(locinfo_timer)

static void locinfo_cleanup(LocInfo *l)
{
    ZeroStruct(l);
}

#define ABSFILE_VERSION 0x20100124

int parse_filewrite(char *fn, Parse *p)
{
    File *fp;
	int r = 0;
	
    fp = absfile_open_write(fn);
    if(!fp)
    {
        fprintf(stderr,"failed to open %s for writing locinfos\n",fn);
        return -1;
    }
    
	r = int_binwrite(fp,ABSFILE_VERSION)
		&& ptr_binwrite(fp,p->locs)
		&& ali_binwrite(fp,&p->locs);

	abfclose(fp);

    return !r;
}

// first step of a deserialize
int parse_fileread(char *fn, Parse *p, SerializeCtxt *ctxt)
{
    int i;
	int ver = 0;
    File*fp = absfile_open_read(fn);
	LocInfo *oldp;
	
    if(!fp || !p)
    {
        fprintf(stderr,"couldn't open file %s to read locinfos\n",fn);
		abfclose(fp);
        return -1;
    }

	int_binread(fp,&ver);
	
	if(ver != ABSFILE_VERSION)
	{
        fprintf(stderr,"version mismatch %x doesn't equal file version %x",ABSFILE_VERSION,ver);
        return -1;
    }

	if(!ptr_binread(fp,&oldp) 
	   || !ali_binread(fp,&p->locs))
    {
        fprintf(stderr,"couldn't read locinfo from %s\n",fn);
		abfclose(fp);
        return -1;
    }
	p->n_locs = ali_size(&p->locs);

	// add the ptr fixup
	for(i = 0; i < p->n_locs; ++i)
	{
		intptr_t off = i*sizeof(*p->locs);
		serialize_fixup_add_ptr(ctxt,oldp+off,p->locs+i);
	}

	abfclose(fp);
    return 0;
}

// final step of a deserialize
int parse_fixup(Parse *p, SerializeCtxt *ctxt)
{
	int i;
	for(i = 0; i < p->n_locs; ++i)
	{
		LocInfo *li = p->locs + i;
		li->tag		 = serialize_fixup_ptr(ctxt,li->tag);
		li->referrer = serialize_fixup_ptr(ctxt,li->referrer);
		li->context	 = serialize_fixup_ptr(ctxt,li->context);
		li->fname	 = serialize_fixup_ptr(ctxt,li->fname);
		li->line	 = serialize_fixup_ptr(ctxt,li->line);
	}
	return 0;
}


void locinfo_print(LocInfo *li, char *in)
{
    char *referrer = li->referrer ? li->referrer : li->tag;
    char *ctxt = li->context?li->context:"";
    if(!in)
        in = "";
    
    printf("%s(LocInfo\n"
              "%s\t(File    . \"%s\")\n"
              "%s\t(Lineno  . \"%i\")\n"
              "%s\t(Line    . \"%s\")\n"
              "%s\t(Tag     . \"%s\")\n"  
              "%s\t(RefName . \"%s\")\n"
              "%s\t(Ctxt    . \"%s\")\n"
           ,in, in,li->fname, in,li->lineno, in,li->line, in,li->tag, in,referrer, in,ctxt);
//     if(li->ref)
//     {
//         char tmp[128];
//         printf("%s\t(Ref ",in);
//         sprintf_s(SSTR(tmp),"%s\t",in);
//         locinfo_print(li->ref,tmp);
//         printf("%s\t)\n", in);
//     }
    printf("%s)\n",in);
}


// int locinfo_vprintf(LocInfo *li,char *fmt,va_list args)
// {
//     locinfo_print(li);
//     return vprintf(fmt,args);
// }

// int locinfo_printf(LocInfo *li,char *fmt,...)
// {
//     int r;
//     va_list vl;
//     va_start(vl,fmt);
//     r = locinfo_vprintf(li,fmt,vl);
//     va_end(vl);
//     return r;
// }

static char* parse_find_add_str(Parse *p, char *s)
{
    char *r = NULL;
	assert(DEREF(p,pool));
    if(s)
    {
        r = strpool_add(p->pool,s);
        p;
        str_replacechar(r,'\n',' ');
        str_replacechar(r,'\"','\'');
    }
    return r;
}

LocInfo* parse_add_locinfo(Parse *p,char *filename, int lineno, char *line, char *tag, char *referrer, char *context)
{
    if(context)
        return parse_add_locinfof(p,filename,lineno,line,tag,referrer,"%s",context);
    return parse_add_locinfof(p,filename,lineno,line,tag,referrer,NULL);
}


LocInfo* parse_add_locinfof(Parse *p,char *filename, int lineno, char *line, char *tag, char *referrer, char *context, ...)
{
    LocInfo *r;
    va_list vl;
    va_start(vl,context);
    r = parse_add_locinfov(p,filename,lineno,line,tag,referrer,context,vl);
    va_end(vl);
    return r;
}

LocInfo* parse_add_locinfov(Parse *p,char *filename, int lineno, char *line, char *tag, char *referrer, char *context_in,va_list args)
{
    char buf[128];
    char *ctxt = context_in;
    LocInfo *l;
    TIMER_START();
    if(ctxt)
    {
        vsnprintf(buf,DIMOF(buf),ctxt,args);
        buf[DIMOF(buf)-1] = 0;
        ctxt = buf;
    }
    
    p->n_locs++;
    l           = ali_push(&p->locs);
    l->tag      = parse_find_add_str(p,tag);
    l->referrer = parse_find_add_str(p,referrer);
    l->context  = parse_find_add_str(p,ctxt);
    l->fname    = parse_find_add_str(p,filename);
    l->lineno   = lineno;
    l->line     = parse_find_add_str(p,line);

    TIMER_END(locinfo_timer);
    return l;
}


int parse_print_search(Parse *p,char *tag, LocInfoField flds)
{
    char *err_str = 0;
    int err_val = 0;
    pcre *re;
    int res = 0;
    int i;
    int matches[10];
    TIMER_START();

	if(!flds)
		flds = LocInfoField_tag;

    re = pcre_compile(tag, PCRE_CASELESS, &err_str, &err_val, NULL);
    if(!re)
    {
        if(err_str)
            pcre_free(err_str);
        return -1;
    }

    for(i = 0; i < p->n_locs; ++i)
    {
        LocInfo *li = p->locs+i; 
		// why didn't I integrate the bitfield check into the macro? :P
#define TAG_MATCH(T) (T && pcre_exec(re,NULL,T,strlen(T),0,0,matches,DIMOF(matches))>=0)
        if(((flds & LocInfoField_tag) && TAG_MATCH(li->tag))
			|| ((flds & LocInfoField_referrer) && TAG_MATCH(li->referrer))
			|| ((flds & LocInfoField_context) && TAG_MATCH(li->context))
			|| ((flds & LocInfoField_referrer) && TAG_MATCH(li->referrer))
			|| ((flds & LocInfoField_file) && TAG_MATCH(li->fname))
			|| ((flds & LocInfoField_line) && TAG_MATCH(li->line))
			)
        {
            res++;
            locinfo_print(li,NULL);
        }
#undef TAG_MATCH
    }
    TIMER_END(locinfo_timer);

    pcre_free(re);
    return res;
}

void locinfo_print_time()
{
    printf("locinfo total:\t\t\t%f\n"
           "\tfind_add_str:\t\t\t%f\n"
           "\tlocinfo_read:\t\t\t%f\n"
           "\tlocinfo_write:\t\t\t%f\n"
           ,timer_elapsed(locinfo_timer)
           ,timer_elapsed(locinfo_parse_find_add_str_timer)
           ,timer_elapsed(locinfo_read_timer)
           ,timer_elapsed(locinfo_write_timer)
        );
}

void parse_cleanup(Parse *p)
{
    int i;
	if(!p)
		return;
    for(i = 0; i<p->n_locs; ++i)
        locinfo_cleanup(p->locs+i);
	ali_destroy(&p->locs, NULL);
}


static int parse_locinfos_from_str_field(Parse *p, char *s, int off, LocInfo ***res)
{
    int n = 0;
    int i;
    if(!p || !res || !s || !INRANGE0(off,sizeof(LocInfo)))
        return 0;
    
    for(i = 0; i<p->n_locs; ++i)
    {
        LocInfo *li = p->locs + i;
        char *s_li = *(char**)OFFSET_PTR(li,off);
        if(s_li && 0 == stricmp(s_li,s))
        {
            (*res) = realloc((*res),++n*sizeof(*res));
            (*res)[n-1] = li;
        }
    }
    return n;
}

int parse_locinfos_from_ref(Parse *p, char *ref, LocInfo ***res)
{
    return parse_locinfos_from_str_field(p,ref,MBR_OFFSET(LocInfo,referrer),res);
}

int parse_locinfos_from_context(Parse *p, char *ref, LocInfo ***res)
{
    return parse_locinfos_from_str_field(p,ref,MBR_OFFSET(LocInfo,context),res);
}

int locinfo_fields_from_str(char *s)
{
    char *a = s;
    int res = 0;
    if(!a)
        return 0;
    while(*a && !isspace(*a))
    {
        
        switch(*a)
        {
        case 't':
            res |= LocInfoField_tag;
            break;
        case 'r':
            res |= LocInfoField_referrer;
            break;
        case 'c':
            res |= LocInfoField_context;
            break;
        case 'f':
            res |= LocInfoField_file;
            break;
        case 'l':
            res |= LocInfoField_line;
            break;
        default:
            fprintf(stderr, "unknown locinfo field %c in %s\n",*a, s);
            return 0;
        };
        a++;
    }
    return res;
}

#define TEST(COND) if(!(COND)) {printf("%s(%d):"#COND ": failed\n",__FILE__,__LINE__); break_if_debugging(); return -1;}

int test_locinfo(void)
{
    Parse p = {0}; 
    Parse p2 = {0}; 
	SerializeCtxt ctxt = {0};
	StrPool pool1 = {0};
	StrPool pool2 = {0};

	p.pool  = &pool1;
	p2.pool = &pool2;
	
    printf("testing locinfo...");
    parse_add_locinfo(&p,"filename",0xaabbccdd,"line0","foo","bar","baz");
    parse_add_locinfo(&p,"fn2",0xaabbccee,"line1","alpha","beta","delta");
	serializectxt_init(&ctxt);
	TEST(0==strpool_write("test.strpool",p.pool));
    TEST(0==parse_filewrite("test.absfile",&p));

	TEST(0==strpool_read("test.strpool",p2.pool,&ctxt));
    TEST(0==parse_fileread("test.absfile",&p2,&ctxt));
    TEST(0==parse_fixup(&p2,&ctxt));
    TEST(p2.n_locs == 2);

    TEST(0==strcmp(p2.locs[0].tag,"foo"));
    TEST(0==strcmp(p2.locs[0].referrer,"bar"));
    TEST(0==strcmp(p2.locs[0].context,"baz"));
    TEST(0==strcmp(p2.locs[0].fname,"filename"));
    TEST(p2.locs[0].lineno == 0xaabbccdd);
    TEST(0==strcmp(p2.locs[0].line,"line0"));

    TEST(0==strcmp(p2.locs[1].tag,"alpha"));
    TEST(0==strcmp(p2.locs[1].referrer,"beta"));
    TEST(0==strcmp(p2.locs[1].context,"delta"));
    TEST(0==strcmp(p2.locs[1].fname,"fn2"));
    TEST(p2.locs[1].lineno == 0xaabbccee);
    TEST(0==strcmp(p2.locs[1].line,"line1"));

    printf("done.\n");
    parse_cleanup(&p);
    parse_cleanup(&p2);
    return 0;
}




#define TYPE_T LocInfo
#define TYPE_FUNC_PREFIX ali
#define ABARRAY_SERIALIZE
#include "abarrayx.c"
#undef TYPE_T
#undef TYPE_FUNC_PREFIX
#undef ABARRAY_SERIALIZE
