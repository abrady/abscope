typedef struct Foo
{
    int a;
    char *b;
} Foo;

typedef struct Bar
{
    int bar_a;
    char baz_b;
} Foo;

void test_func( Entity *pEnt, char *RewardTableName, char *ChoiceName )
{
    int a = pEnt ? pEnt->n : 0;
    Foo b = {0};
    do
    {
        if(0)
        {
            Bar c;
        }
    }while(0);
    test_foo();
}

AUTO_STARTUP(AlgoTablesCommon);
void CommonAlgoTables_Load(void)
{
/*
	loadstart_printf("Loading CommonAlgoTables...");

	StructInit(parse_CommonAlgoTables, &g_CommonAlgoTables);

	ParserLoadFiles( NULL, "defs/rewards/algotables_common.data", "algotables_common.bin", 0, parse_CommonAlgoTables, &g_CommonAlgoTables);	

	loadend_printf(" done." );

	if (isDevelopmentMode())
	{
		// Have reload take effect immediately
		FolderCacheSetCallback(FOLDER_CACHE_CALLBACK_UPDATE, "defs/rewards/algotables_common.data", CommonAlgoTables_ReloadCallback);
	}
*/
}

#define FOO(X,Y,...) x = y + z;                 \
    line_2

typedef enum Baz
{
    Bar_A = 1,
    Bar_B,
    // last
    Bar_C
} Bar;

void test_func2( Entity *pEnt, char *RewardTableName, char *ChoiceName )
{
    foo(pEnt,a,b);
    if(0==strcmp("foo","bar"))
        return;
}

Foo **bar2;    

typedef struct Foo2
{
    REF_TO(Message) hNameMsg; AST(NAME(NameMsg))
	U32 iSortID; AST(NO_TEXT_SAVE) // automatically calculated
    
	bool bSearchable; // shows up in marketplace search
    
	ItemType eType;
};
    
void test_func3()
{
    Bar *pBar = pFoo ? GET_REF(pFoo->hFoo) : NULL, *pBaz;
    U32 foo;
    if (pDef->eContents != Store_All && pDef->bSellEnabled)
        return;
    foo = eBar;
}

AUTO_EXPR_FUNC(UIGen) ACMD_NAME("Acmd");
void exprAcmd()
{
}

/*
typedef struct Bar
{
    void *p;
    float q;
    char *c;
} Bar;


void GrantMissionRewards( Entity *pEnt, ACMD_NAMELIST("RewardTable", REFDICTIONARY) char *RewardTableName, ACMD_NAMELIST("ItemDef", REFDICTIONARY) char *ChoiceName )
{
}

int atexit(void(*)(void)) throw();

void inv_trh_AddSlotToBagWithDef(ATR_ARGS, ATH_ARG NOCONST(Entity)* pEnt, ATH_ARG NOCONST(InventoryBag)* pBag, InventorySlotIDDef *pDef)
{
}


const char* RankNames[NUM_CRITTER_RANKS] = {"henchman", "villain", "mastervillain", "supervillain", "legendaryvillain", "cosmicvillain" };

*/
