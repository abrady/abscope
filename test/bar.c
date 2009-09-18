AUTO_EXPR_FUNC(util) ACMD_NAME(TimeSinceSS2000);
U32 exprFuncTimeSinceSS2000(U32 iInTime)
{
	return timeSecondsSince2000() - iInTime;
}

AUTO_ENUM AEN_EXTEND_WITH_DYNLIST(g_ExtraCharClassTypeIDs);
typedef enum CharClassTypes
{
	CharClassTypes_None = 0
	//None is assumed to be the last fixed bag ID by the dynamic enum loading code.
	//Add any new modes after it

} CharClassTypes;

void if_testing_suite()
{
    if(a)
        foo = 0;
    else
        foo = 1;
    if(b)
    {
        foo = 2;
    }
    else if(c)
    {
        foo = 3;
    }
}

void foo_why_isn_time_to_linger_getting_added()
{    
    if (entCheckFlag(pent, ENTITYFLAG_DESTROY) || pent->pCritter->timeToLinger < 0.0)
    {
    }
}


// item/inventoryCommon.c/(704): this isn't showing up as a function
AUTO_TRANS_HELPER;
int inv_ent_trh_AddBag(ATR_ARGS, ATH_ARG NOCONST(Entity)* pEnt, bool bSilent, NOCONST(InventoryBag)* pBag)
{
    if ( ISNULL(pEnt))
        return false;
    
    if (inv_trh_GetBag(ATR_PASS_ARGS,pEnt,bSilent,pBag->BagID))
        return false;
	eaIndexedAdd(&pEnt->pInventory->ppInventoryBags, pBag);
	return true;
}


AUTO_EXPR_FUNC(UIGen) ACMD_NAME("InitMenus");
void lots_of_args()
{
	for( i=0; i < sky_data->visible_sky->skyDomeCount; i++ )
	{
		estrPrintf(&sky_text, "\n%s:\n    Alpha(%g)\n    Sort Order(%g)\n    High Detail(%d)\n    Draw Percent(%g)\n    Scale(%g)\n    Angle(%g)\n    Ambient(%.2f, %.2f, %.2f)\n    Tint(%.2f, %.2f, %.2f)\n    Position(%.2f, %.2f, %.2f)\n", 
			sky_data->visible_sky->skyDomes[i]->dome->name, 
			sky_data->visible_sky->skyDomes[i]->alpha,
			sky_data->visible_sky->skyDomes[i]->sort_order,
			sky_data->visible_sky->skyDomes[i]->high_detail, 
			sky_data->visible_sky->skyDomes[i]->group_percent,
			sky_data->visible_sky->skyDomes[i]->dome->current_dome_values.scale,
			sky_data->visible_sky->skyDomes[i]->dome->current_dome_values.angle,
			sky_data->visible_sky->skyDomes[i]->dome->current_dome_values.ambientHSV[0],
			sky_data->visible_sky->skyDomes[i]->dome->current_dome_values.ambientHSV[1],
			sky_data->visible_sky->skyDomes[i]->dome->current_dome_values.ambientHSV[2],
			sky_data->visible_sky->skyDomes[i]->dome->current_dome_values.tintHSV[0],
			sky_data->visible_sky->skyDomes[i]->dome->current_dome_values.tintHSV[1],
			sky_data->visible_sky->skyDomes[i]->dome->current_dome_values.tintHSV[2],
			sky_data->visible_sky->skyDomes[i]->dome->current_dome_values.pos[0],
			sky_data->visible_sky->skyDomes[i]->dome->current_dome_values.pos[1],
			sky_data->visible_sky->skyDomes[i]->dome->current_dome_values.pos[2]);
		estrConcatString(&sky_debug_text, sky_text, estrLength(&sky_text));
	}
}


void wleMenuInitMenus(void)
{
	EMMenuItemDef wleMenuItems[] =
	{
		{"em_newcurrented", "New ZoneMap...", wleCheckCmdNewZoneMap, NULL, "Editor.NewGrid"},
		{"em_opencurrented", "Open ZoneMap...", wleCheckCmdOpenZoneMap, NULL, "Editor.OpenGrid"},
		{"em_close", "Close", wleMenuDisableCheck},
		{"em_cut", "Cut", wleCheckCmdCutCopyPaste, NULL, "EM.Cut"},
		{"em_copy", "Copy", wleCheckCmdCutCopyPaste, NULL, "EM.Copy"},
		{"em_paste", "Paste", wleCheckCmdCutCopyPaste, NULL, "EM.Paste"},
		{"focuscamera", "Focus camera", NULL, NULL, "Editor.FocusCamera"},
		{"uptree", "Select parent", wleCheckCmdUpTree, NULL, "Editor.UpTree"},
		{"downtree", "Select child", wleCheckCmdDownTree, NULL, "Editor.DownTree"},
		{"deselect", "Deselect", wleCheckCmdDeselect, NULL, "Editor.Deselect"},
		{"invselect", "Invert", wleCheckCmdInvertSelection, NULL, "Editor.Invert"},
		{"lockselection", "Toggle selection lock", wleCheckCmdLockSelection, NULL, "Editor.Lock"},
		{"hide", "Hide/Unhide", wleCheckCmdHideSelection, NULL, "Editor.Hide"},
		{"unhide", "Unhide all", wleCheckCmdUnhideAll, NULL, "Editor.Unhide"},
		{"hidevols", "Hide all volumes"},
		{"hideoccvols", "Hide occlusion volumes", wleUIVolumeTypeHideCheck},
		{"hideaudiovols", "Hide audio volumes", wleUIVolumeTypeHideCheck},
		{"hideskyvols", "Hide skyfade volumes", wleUIVolumeTypeHideCheck},
		{"hidehoodvols", "Hide neighborhood volumes", wleUIVolumeTypeHideCheck},
		{"hideoptactvols", "Hide optional action volumes", wleUIVolumeTypeHideCheck},
		{"hidelandmarkvols", "Hide landmark volumes", wleUIVolumeTypeHideCheck},
		{"hidepowervols", "Hide power volumes", wleUIVolumeTypeHideCheck},
		{"hidewarpvols", "Hide warp volumes", wleUIVolumeTypeHideCheck},
		{"hidegenesisvols", "Hide genesis volumes", wleUIVolumeTypeHideCheck},
		{"disablevolcoll", "Disable volume collision"},
		{"hidevolsubmenu", "Hide volumes"},
		{"hidepatrolpoints", "Hide patrol points"},
		{"hideencounteractors", "Hide encounter actors"},
		{"placevolume", "Place volume", wleCheckCmdPlaceObject, NULL, "Editor.PlaceVolume"},
		{"placecurve", "Place curve", wleCheckCmdPlaceObject, NULL, "Editor.PlaceCurve"},
		{"placeencounter", "Place encounter", wleCheckCmdPlaceObject, NULL, "Editor.PlaceEncounter"},
		{"placespawnpoint", "Place spawn point", wleCheckCmdPlaceObject, NULL, "Editor.PlaceSpawnPoint"},
		{"placepatrolroute", "Place patrol route", wleCheckCmdPlaceObject, NULL, "Editor.PlacePatrolRoute"},
		{"placenamedpoint", "Place named point", wleCheckCmdPlaceObject, NULL, "Editor.PlaceNamedPoint"},
		{"placeobjsubmenu", "Place"},
		{"freeze", "Freeze", wleCheckCmdFreezeSelection, NULL, "Editor.Freeze"},
		{"unfreeze", "Unfreeze all", wleCheckCmdUnfreeze, NULL, "Editor.Unfreeze"},
		{"cyclewidget", "Cycle widget", wleCheckCmdCycleGizmo, NULL, "Editor.TransRot"},
		{"snapnormal", "Snap to normal", wleCheckCmdSnapNormal},
		{"snapclamping", "Snap clamping", wleCheckCmdSnapClamp},
		{"cycletranssnap", "Cycle translate snap", wleCheckCmdCycleTransSnap, NULL, "Editor.CycleTransSnap"},
		{"snapgrid", "Snap to grid", wleCheckCmdSnapNormal},
		{"snapvertex", "Snap to vertex", wleCheckCmdSnapNormal},
		{"snapmidpoint", "Snap to midpoint", wleCheckCmdSnapNormal},
		{"snapedge", "Snap to edge", wleCheckCmdSnapNormal},
		{"snapface", "Snap to face", wleCheckCmdSnapNormal},
		{"snapterrain", "Snap to terrain", wleCheckCmdSnapNormal},
		{"snapsmart", "Auto snap", wleCheckCmdSnapNormal},
		{"snapnone", "No snap", wleCheckCmdSnapNormal},
		{"snapsubmenu", "Snap to", wleCheckCmdSnapNormal},
		{"dectranssnap", "Decrease translation snap", NULL, NULL, "Editor.DecTransSnap"},
		{"inctranssnap", "Increase translation snap", NULL, NULL, "Editor.IncTransSnap"},
		{"cycletransaxes", "Cycle translation axes", wleCheckCmdCycleTransAxes, NULL, "Editor.CycleTransAxes"},
		{"toggletransx", "Toggle x-axis translation", wleCheckCmdToggleTransX, NULL, "Editor.ToggleTransX"},
		{"toggletransy", "Toggle y-axis translation", wleCheckCmdToggleTransY, NULL, "Editor.ToggleTransY"},
		{"toggletransz", "Toggle z-axis translation", wleCheckCmdToggleTransZ, NULL, "Editor.ToggleTransZ"},
		{"togglerotsnap", "Toggle rotation snap", wleCheckCmdToggleRotSnap, NULL, "Editor.ToggleRotSnap"},
		{"decrotsnap", "Decrease rotation snap", NULL, NULL, "Editor.DecRotSnap"},
		{"incrotsnap", "Increase rotation snap", NULL, NULL, "Editor.IncRotSnap"},
		{"worldpivot", "World pivot", wleCheckCmdWorldPivot, NULL, "Editor.WorldPivot"},
		{"resetrot", "Reset rotation", wleCheckCmdResetRot, NULL, "Editor.ResetRot"},
		{"lockfiles", "Lock object library files", wleCheckCmdLockFiles, NULL, "Editor.LockFiles"},
		{"findandreplace", "Find and replace...", wleCheckCmdFindAndReplace, NULL, "Editor.FindAndReplace"},
		{"instance", "Instance", wleCheckCmdInstantiate, NULL, "Editor.Instance"},
		{"duplicate", "Duplicate", wleCheckCmdDuplicate, NULL, "Editor.SelectDo duplicate"},
		{"reseed", "Reseed", wleCheckDefault, NULL, "Editor.ReseedAll"},
		{"group", "Group", wleCheckCmdGroup, NULL, "Editor.Group"},
		{"rename", "Rename", wleCheckCmdRename, NULL, "Editor.Rename"},
		{"select_children", "Select children", wleCheckDefault, NULL, "Editor.SelectChildren"},
		{"addtogroup", "Add to group...", wleCheckCmdAddToGroup, NULL, "Editor.AddToGroup"},
		{"ungroup", "Ungroup", wleCheckCmdUngroup, NULL, "Editor.Ungroup"},
		{"delete", "Delete", wleCheckCmdDelete, NULL, "Editor.SelectDo delete"},
		{"editinstances", "Edit instances", wleCheckCmdEditOrig, NULL, "Editor.EditOrig"},
		{"newlayer", "New layer...", wleCheckCmdNewLayer, NULL, "Editor.NewLayer"},
		{"importlayer", "Import layer...", wleCheckCmdImportLayer, NULL, "Editor.ImportLayer"},
		{"reloadfromsource", "Reload from source", wleCheckCmdReloadFromSource, NULL, "Editor.ReloadFromSource"},
		{"savetolib", "Save to library...", wleCheckCmdSaveToLib, NULL, "Editor.SaveToLib"},
		{"edit_subobjects", "Edit Subobjects", NULL, NULL, "Editor.SelectDo edit_subobjects"},
		{"copynames", "Copy selection names", NULL, NULL, "Editor.CopySelectionNames"},

		// keep these?
		//	{"locklayer", "Edit layer", NULL, NULL, "Editor.SelectDo lock_layer"},
		//	{"revertlayer", "Close layer", NULL, NULL, "Edit.SelectDo revert_layer"},
		//	{"savelayer", "Save layer", NULL, NULL, "Editor.SelectDo save_layer"},
		//	{"saveandcloselayer", "Save and close layer", NULL, NULL, "Editor.SelectDo save_and_close_layer"},
	};

	PERFINFO_AUTO_START_FUNC();

	// create items
	emMenuItemCreateFromTable(&worldEditor, wleMenuItems, ARRAY_SIZE(wleMenuItems));

	// customize non-command menu items
	emMenuItemSet(&worldEditor, "snapgrid", ui_MenuItemCreate("Snap to grid", UIMenuCheckButton, wleUIEditTranslateSnap, (void*) EditSnapGrid, (void*) false));
	emMenuItemSet(&worldEditor, "snapvertex", ui_MenuItemCreate("Snap to vertex", UIMenuCheckButton, wleUIEditTranslateSnap, (void*) EditSnapVertex, (void*) false));
	emMenuItemSet(&worldEditor, "snapmidpoint", ui_MenuItemCreate("Snap to midpoint", UIMenuCheckButton, wleUIEditTranslateSnap, (void*) EditSnapMidpoint, (void*) false));
	emMenuItemSet(&worldEditor, "snapedge", ui_MenuItemCreate("Snap to edge", UIMenuCheckButton, wleUIEditTranslateSnap, (void*) EditSnapEdge, (void*) false));
	emMenuItemSet(&worldEditor, "snapface", ui_MenuItemCreate("Snap to face", UIMenuCheckButton, wleUIEditTranslateSnap, (void*) EditSnapFace, (void*) false));
	emMenuItemSet(&worldEditor, "snapterrain", ui_MenuItemCreate("Snap to terrain", UIMenuCheckButton, wleUIEditTranslateSnap, (void*) EditSnapTerrain, (void*) false));
	emMenuItemSet(&worldEditor, "snapsmart", ui_MenuItemCreate("Auto snap", UIMenuCheckButton, wleUIEditTranslateSnap, (void*) EditSnapSmart, (void*) false));
	emMenuItemSet(&worldEditor, "snapnone", ui_MenuItemCreate("No snap", UIMenuCheckButton, wleUIEditTranslateSnap, (void*) EditSnapNone, (void*) false));
	emMenuItemSet(&worldEditor, "snapnormal", ui_MenuItemCreate("Snap to normal", UIMenuCheckButton, wleUIEditTranslateSnapNormal, NULL, (void*) false));
	emMenuItemSet(&worldEditor, "snapclamping", ui_MenuItemCreate("Snap clamping", UIMenuCheckButton, wleUIEditTranslateSnapClamping, NULL, (void*) false));
	emMenuItemSet(&worldEditor, "hidevols", ui_MenuItemCreate("Hide all volumes", UIMenuCheckRefButton, wleUIViewHideAllVols, NULL, &visSettings->hide_all_volumes));
	emMenuItemSet(&worldEditor, "hideoccvols", ui_MenuItemCreate("Hide occlusion volumes", UIMenuCheckRefButton, wleUIViewHideOccVols, NULL, &visSettings->hide_occlusion_volumes));
	emMenuItemSet(&worldEditor, "hideaudiovols", ui_MenuItemCreate("Hide audio volumes", UIMenuCheckRefButton, wleUIViewHideAudioVols, NULL, &visSettings->hide_audio_volumes));
	emMenuItemSet(&worldEditor, "hideskyvols", ui_MenuItemCreate("Hide skyfade volumes", UIMenuCheckRefButton, wleUIViewHideSkyVols, NULL, &visSettings->hide_skyfade_volumes));
	emMenuItemSet(&worldEditor, "hidehoodvols", ui_MenuItemCreate("Hide neighborhood volumes", UIMenuCheckRefButton, wleUIViewHideNeighborhoodVols, NULL, &visSettings->hide_neighborhood_volumes));
	emMenuItemSet(&worldEditor, "hideoptactvols", ui_MenuItemCreate("Hide optional action volumes", UIMenuCheckRefButton, wleUIViewHideOptionalActionVols, NULL, &visSettings->hide_optionalaction_volumes));
	emMenuItemSet(&worldEditor, "hidelandmarkvols", ui_MenuItemCreate("Hide landmark volumes", UIMenuCheckRefButton, wleUIViewHideLandmarkVols, NULL, &visSettings->hide_landmark_volumes));
	emMenuItemSet(&worldEditor, "hidepowervols", ui_MenuItemCreate("Hide power volumes", UIMenuCheckRefButton, wleUIViewHidePowerVols, NULL, &visSettings->hide_power_volumes));
	emMenuItemSet(&worldEditor, "hidewarpvols", ui_MenuItemCreate("Hide warp volumes", UIMenuCheckRefButton, wleUIViewHideWarpVols, NULL, &visSettings->hide_warp_volumes));
	emMenuItemSet(&worldEditor, "hidegenesisvols", ui_MenuItemCreate("Hide genesis volumes", UIMenuCheckRefButton, wleUIViewHideGenesisVols, NULL, &visSettings->hide_genesis_volumes));
	emMenuItemSet(&worldEditor, "hidepatrolpoints", ui_MenuItemCreate("Hide patrol points", UIMenuCheckButton, wleUIViewHidePatrolPoints, NULL, (void*) (intptr_t) EditorPrefGetInt(WLE_PREF_EDITOR_NAME, WLE_PREF_CAT_UI, "HidePatrolPoints", 0)));
	emMenuItemSet(&worldEditor, "hideencounteractors", ui_MenuItemCreate("Hide encounter actors", UIMenuCheckButton, wleUIViewHideEncounterActors, NULL, (void*) (intptr_t) EditorPrefGetInt(WLE_PREF_EDITOR_NAME, WLE_PREF_CAT_UI, "HideEncounterActors", 0)));
	emMenuItemSet(&worldEditor, "disablevolcoll", ui_MenuItemCreate("Disable volume collision", UIMenuCheckRefButton, wleUIViewDisableVolColl, NULL, &editorUIState->disableVolColl));

	editorUIState->snapModeMenu = emMenuCreate(&worldEditor, "",
		"snapgrid",
		"snapvertex",
		"snapmidpoint",
		"snapedge",
		"snapface",
		"snapterrain",
		"snapsmart",
		"snapnone",
		NULL);
	emMenuItemSet(&worldEditor, "snapsubmenu", ui_MenuItemCreate("Snap to", UIMenuSubmenu, NULL, NULL, editorUIState->snapModeMenu));

	submenu = emMenuCreate(&worldEditor, "",
		"hidevols",
		"em_separator",
		"hideoccvols",
		"hideaudiovols",
		"hideskyvols",
		"hidehoodvols",
		"hideoptactvols",
		"hidelandmarkvols",
		"hidepowervols",
		"hidewarpvols",
		"hidegenesisvols",
		"em_separator",
		"disablevolcoll",
		NULL);
	emMenuItemSet(&worldEditor, "hidevolsubmenu", ui_MenuItemCreate("Hide volumes", UIMenuSubmenu, NULL, NULL, submenu));

	if (gShowNewWorldEditorFeatures)
	{
		submenu = emMenuCreate(&worldEditor, "",
			"placevolume",
			"placecurve",
			"placespawnpoint",
			"placeencounter",
			"placepatrolroute",
			"placenamedpoint",
			NULL);
	}
	else
	{
		submenu = emMenuCreate(&worldEditor, "",
			"placevolume",
			"placecurve",
			"placespawnpoint",
			"placenamedpoint",
			NULL);
	}

	emMenuItemSet(&worldEditor, "placeobjsubmenu", ui_MenuItemCreate("Place", UIMenuSubmenu, NULL, NULL, submenu));

	// menu bar
	emMenuRegister(&worldEditor, emMenuCreate(&worldEditor, "File",
		"newlayer",
		"importlayer",
		"setscene",
		"setpublicname",
		"setmaptype",
		"em_separator",
		"save",
		"saveas",
		"reloadfromsource",
		NULL));
	emMenuRegister(&worldEditor, emMenuCreate(&worldEditor, "Edit",
		"resetrot",
		"duplicate",
		"copy",
		"cut",
		"instance",
		"delete",
		"reseed",
		"findandreplace",
		"em_separator",
		"placeobjsubmenu",
		"em_separator",
		"snapsubmenu",
		"snapnormal",
		"snapclamping",
		"em_separator",
		"lockfiles",
		"savetolib",
		NULL));
	emMenuRegister(&worldEditor, emMenuCreate(&worldEditor, "View",
		"focuscamera",
		"em_separator",
		"hide",
		"unhide",
		"em_separator",
		"hidevolsubmenu",
		"hidepatrolpoints",
		"hideencounteractors",
		NULL));
	emMenuRegister(&worldEditor, emMenuCreate(&worldEditor, "Select",
		//"invselect",
		"deselect",
		"lockselection",
		"em_separator",
		"freeze",
		"unfreeze",
		"lock",
		NULL));
	emMenuRegister(&worldEditor, emMenuCreate(&worldEditor, "Group",
		"group",
		"addtogroup",
		"ungroup",
		NULL));

	// custom menu
	if (!wleMenuRightClick)
	{
		wleMenuRightClick = StructCreate(parse_EdObjCustomMenu);
		wleMenuRightClick->editor = &worldEditor;
	}
	EditorPrefGetStruct(WLE_PREF_EDITOR_NAME, WLE_PREF_CAT_OPTIONS, "CustomMenu", parse_EdObjCustomMenu, wleMenuRightClick);

	PERFINFO_AUTO_STOP();
}

AUTO_EXPR_FUNC(UIGen) ACMD_NAME("InitMenus");
void lots_of_args()
{
	for( i=0; i < sky_data->visible_sky->skyDomeCount; i++ )
	{
		estrPrintf(&sky_text, "\n%s:\n    Alpha(%g)\n    Sort Order(%g)\n    High Detail(%d)\n    Draw Percent(%g)\n    Scale(%g)\n    Angle(%g)\n    Ambient(%.2f, %.2f, %.2f)\n    Tint(%.2f, %.2f, %.2f)\n    Position(%.2f, %.2f, %.2f)\n", 
			sky_data->visible_sky->skyDomes[i]->dome->name, 
			sky_data->visible_sky->skyDomes[i]->alpha,
			sky_data->visible_sky->skyDomes[i]->sort_order,
			sky_data->visible_sky->skyDomes[i]->high_detail, 
			sky_data->visible_sky->skyDomes[i]->group_percent,
			sky_data->visible_sky->skyDomes[i]->dome->current_dome_values.scale,
			sky_data->visible_sky->skyDomes[i]->dome->current_dome_values.angle,
			sky_data->visible_sky->skyDomes[i]->dome->current_dome_values.ambientHSV[0],
			sky_data->visible_sky->skyDomes[i]->dome->current_dome_values.ambientHSV[1],
			sky_data->visible_sky->skyDomes[i]->dome->current_dome_values.ambientHSV[2],
			sky_data->visible_sky->skyDomes[i]->dome->current_dome_values.tintHSV[0],
			sky_data->visible_sky->skyDomes[i]->dome->current_dome_values.tintHSV[1],
			sky_data->visible_sky->skyDomes[i]->dome->current_dome_values.tintHSV[2],
			sky_data->visible_sky->skyDomes[i]->dome->current_dome_values.pos[0],
			sky_data->visible_sky->skyDomes[i]->dome->current_dome_values.pos[1],
			sky_data->visible_sky->skyDomes[i]->dome->current_dome_values.pos[2]);
		estrConcatString(&sky_debug_text, sky_text, estrLength(&sky_text));
	}
}
typedef enum PCLayerArea {
	kPCLayerArea_Main,
	kPCLayerArea_Child
} PCLayerArea;

void tf1()
{
    (*unlock)(lock->accountID, lock->key);
}
