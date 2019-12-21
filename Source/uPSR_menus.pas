Unit uPSR_menus;
{$I PascalScript.inc}
Interface
Uses uPSRuntime;

procedure RIRegister_Menus_Routines(S: TPSExec);
{$IFNDEF FPC}
procedure RIRegisterTMENUITEMSTACK(Cl: TPSRuntimeClassImporter);
procedure RIRegisterTPOPUPLIST(Cl: TPSRuntimeClassImporter);
{$ENDIF}
procedure RIRegisterTPOPUPMENU(Cl: TPSRuntimeClassImporter);
procedure RIRegisterTMAINMENU(Cl: TPSRuntimeClassImporter);
procedure RIRegisterTMENU(Cl: TPSRuntimeClassImporter);
procedure RIRegisterTMENUITEM(Cl: TPSRuntimeClassImporter);
procedure RIRegister_Menus(CL: TPSRuntimeClassImporter);

implementation
{$IFDEF LINUX}
{$IFNDEF FPC}
Uses
  Libc, SysUtils, Classes, QControls, QMenus, QGraphics;
{$ELSE}
Uses
  SysUtils, Classes, Controls, Menus, Graphics, LCLType, ImgList{+}{$IFDEF DELPHI16UP},UITypes{$ENDIF}{+.};
{$ENDIF}
{$ELSE}
Uses {$IFNDEF FPC}WINDOWS,{$ELSE} LCLType,{$ENDIF} SYSUTILS, CLASSES, CONTNRS, MESSAGES, GRAPHICS, IMGLIST{+}{$IFDEF DELPHI16UP},UITypes{$ENDIF}{+.}, ACTNLIST, Menus;
{$ENDIF}

{$IFNDEF FPC}
procedure TPOPUPLISTWINDOW_R(Self: TPopupList; var T: HWND);
begin T := Self.WINDOW; end;
{$ENDIF}

procedure TPOPUPMENUONPOPUP_W(Self: TPopupMenu; const T: TNotifyEvent);
begin Self.OnPopup := T; end;

procedure TPOPUPMENUONPOPUP_R(Self: TPopupMenu; var T: TNotifyEvent);
begin T := Self.OnPopup; end;

{$IFNDEF FPC}
procedure TPOPUPMENUTRACKBUTTON_W(Self: TPopupMenu; const T: TTrackButton);
begin Self.TrackButton := T; end;

procedure TPOPUPMENUTRACKBUTTON_R(Self: TPopupMenu; var T: TTrackButton);
begin T := Self.TrackButton; end;

procedure TPOPUPMENUMENUANIMATION_W(Self: TPopupMenu; const T: TMenuAnimation);
begin Self.MenuAnimation := T; end;

procedure TPOPUPMENUMENUANIMATION_R(Self: TPopupMenu; var T: TMenuAnimation);
begin T := Self.MenuAnimation; end;

procedure TPOPUPMENUHELPCONTEXT_W(Self: TPopupMenu; const T: THelpContext);
begin Self.HelpContext := T; end;

procedure TPOPUPMENUHELPCONTEXT_R(Self: TPopupMenu; var T: THelpContext);
begin T := Self.HelpContext; end;
{$ENDIF}

procedure TPOPUPMENUAUTOPOPUP_W(Self: TPopupMenu; const T: Boolean);
begin Self.AutoPopup := T; end;

procedure TPOPUPMENUAUTOPOPUP_R(Self: TPopupMenu; var T: Boolean);
begin T := Self.AutoPopup; end;

{$IFNDEF FPC}
procedure TPOPUPMENUALIGNMENT_W(Self: TPopupMenu; const T: TPopupAlignment);
begin Self.Alignment := T; end;

procedure TPOPUPMENUALIGNMENT_R(Self: TPopupMenu; var T: TPopupAlignment);
begin T := Self.Alignment; end;
{$ENDIF}

procedure TPOPUPMENUPOPUPCOMPONENT_W(Self: TPopupMenu; const T: TComponent);
begin Self.PopupComponent := T; end;

procedure TPOPUPMENUPOPUPCOMPONENT_R(Self: TPopupMenu; var T: TComponent);
begin T := Self.PopupComponent; end;

{$IFNDEF FPC}
procedure TMAINMENUAUTOMERGE_W(Self: TMainMenu; const T: Boolean);
begin Self.AutoMerge := T; end;

procedure TMAINMENUAUTOMERGE_R(Self: TMainMenu; var T: Boolean);
begin T := Self.AutoMerge; end;
{$ENDIF}

procedure TMENUITEMS_R(Self: TMenu; var T: TMenuItem);
begin T := Self.Items; end;

{$IFNDEF FPC}
procedure TMENUWINDOWHANDLE_W(Self: TMenu; const T: HWND);
begin Self.WindowHandle := T; end;

procedure TMENUWINDOWHANDLE_R(Self: TMenu; var T: HWND);
begin T := Self.WindowHandle; end;

procedure TMENUPARENTBIDIMODE_W(Self: TMenu; const T: Boolean);
begin Self.ParentBiDiMode := T; end;

procedure TMENUPARENTBIDIMODE_R(Self: TMenu; var T: Boolean);
begin T := Self.ParentBiDiMode; end;

procedure TMENUOWNERDRAW_W(Self: TMenu; const T: Boolean);
begin Self.OwnerDraw := T; end;

procedure TMENUOWNERDRAW_R(Self: TMenu; var T: Boolean);
begin T := Self.OwnerDraw; end;

procedure TMENUBIDIMODE_W(Self: TMenu; const T: TBiDiMode);
begin Self.BiDiMode := T; end;

procedure TMENUBIDIMODE_R(Self: TMenu; var T: TBiDiMode);
begin T := Self.BiDiMode; end;

procedure TMENUAUTOLINEREDUCTION_W(Self: TMenu; const T: TMenuAutoFlag);
begin Self.AutoLineReduction := T; end;

procedure TMENUAUTOLINEREDUCTION_R(Self: TMenu; var T: TMenuAutoFlag);
begin T := Self.AutoLineReduction; end;

procedure TMENUAUTOHOTKEYS_W(Self: TMenu; const T: TMenuAutoFlag);
begin Self.AutoHotkeys := T; end;

procedure TMENUAUTOHOTKEYS_R(Self: TMenu; var T: TMenuAutoFlag);
begin T := Self.AutoHotkeys; end;

{$ENDIF}

procedure TMENUHANDLE_R(Self: TMenu; var T: HMENU);
begin T := Self.Handle; end;

procedure TMENUIMAGES_W(Self: TMenu; const T: TCustomImageList);
begin Self.Images := T; end;

procedure TMENUIMAGES_R(Self: TMenu; var T: TCustomImageList);
begin T := Self.IMAGES; end;

{$IFNDEF FPC}
procedure TMENUITEMONMEASUREITEM_W(Self: TMenuItem; const T: TMenuMeasureItemEvent);
begin Self.OnMeasureItem := T; end;

procedure TMENUITEMONMEASUREITEM_R(Self: TMenuItem; var T: TMenuMeasureItemEvent);
begin T := Self.OnMeasureItem; end;

procedure TMENUITEMONADVANCEDDRAWITEM_W(Self: TMenuItem; const T: TAdvancedMenuDrawItemEvent);
begin Self.OnAdvancedDrawItem := T; end;

procedure TMENUITEMONADVANCEDDRAWITEM_R(Self: TMenuItem; var T: TAdvancedMenuDrawItemEvent);
begin T := Self.OnAdvancedDrawItem; end;

procedure TMENUITEMONDRAWITEM_W(Self: TMenuItem; const T: TMenuDrawItemEvent);
begin Self.OnDrawItem := T; end;

procedure TMENUITEMONDRAWITEM_R(Self: TMenuItem; var T: TMenuDrawItemEvent);
begin T := Self.OnDrawItem; end;
{$ENDIF}

procedure TMENUITEMONCLICK_W(Self: TMenuItem; const T: TNotifyEvent);
begin Self.OnClick := T; end;

procedure TMENUITEMONCLICK_R(Self: TMenuItem; var T: TNotifyEvent);
begin T := Self.OnClick; end;

procedure TMENUITEMVISIBLE_W(Self: TMenuItem; const T: Boolean);
begin Self.Visible := T; end;

procedure TMENUITEMVISIBLE_R(Self: TMenuItem; var T: Boolean);
begin T := Self.Visible; end;

procedure TMENUITEMSHORTCUT_W(Self: TMenuItem; const T: TShortcut);
begin Self.Shortcut := T; end;

procedure TMENUITEMSHORTCUT_R(Self: TMenuItem; var T: TShortcut);
begin T := Self.Shortcut; end;

procedure TMENUITEMRADIOITEM_W(Self: TMenuItem; const T: Boolean);
begin Self.RadioItem := T; end;

procedure TMENUITEMRADIOITEM_R(Self: TMenuItem; var T: Boolean);
begin T := Self.RadioItem; end;

procedure TMENUITEMIMAGEINDEX_W(Self: TMenuItem; const T: TImageIndex);
begin Self.ImageIndex := T; end;

procedure TMENUITEMIMAGEINDEX_R(Self: TMenuItem; var T: TImageIndex);
begin T := Self.ImageIndex; end;

procedure TMENUITEMHINT_W(Self: TMenuItem; const T: string);
begin Self.Hint := T; end;

procedure TMENUITEMHINT_R(Self: TMenuItem; var T: string);
begin T := Self.hint; end;

procedure TMENUITEMHELPCONTEXT_W(Self: TMenuItem; const T: THelpContext);
begin Self.HelpContext := T; end;

procedure TMENUITEMHELPCONTEXT_R(Self: TMenuItem; var T: THelpContext);
begin T := Self.HelpContext; end;

procedure TMENUITEMGROUPINDEX_W(Self: TMenuItem; const T: Byte);
begin Self.GroupIndex := T; end;

procedure TMENUITEMGROUPINDEX_R(Self: TMenuItem; var T: Byte);
begin T := Self.GroupIndex; end;

procedure TMENUITEMENABLED_W(Self: TMenuItem; const T: Boolean);
begin Self.Enabled := T; end;

procedure TMENUITEMENABLED_R(Self: TMenuItem; var T: Boolean);
begin T := Self.Enabled; end;

procedure TMENUITEMDEFAULT_W(Self: TMenuItem; const T: Boolean);
begin Self.Default := T; end;

procedure TMENUITEMDEFAULT_R(Self: TMenuItem; var T: Boolean);
begin T := Self.Default; end;

procedure TMENUITEMSUBMENUIMAGES_W(Self: TMenuItem; const T: TCustomImageList);
begin Self.SubMenuImages := T; end;

procedure TMENUITEMSUBMENUIMAGES_R(Self: TMenuItem; var T: TCustomImageList);
begin T := Self.SubMenuImages; end;

procedure TMENUITEMCHECKED_W(Self: TMenuItem; const T: Boolean);
begin Self.Checked := T; end;

procedure TMENUITEMCHECKED_R(Self: TMenuItem; var T: Boolean);
begin T := Self.Checked; end;

procedure TMENUITEMCAPTION_W(Self: TMenuItem; const T: string);
begin Self.Caption := T; end;

procedure TMENUITEMCAPTION_R(Self: TMenuItem; var T: string);
begin T := Self.Caption; end;

procedure TMENUITEMBITMAP_W(Self: TMenuItem; const T: TBitmap);
begin Self.Bitmap := T; end;

procedure TMENUITEMBITMAP_R(Self: TMenuItem; var T: TBitmap);
begin T := Self.Bitmap; end;

{$IFNDEF FPC}
procedure TMENUITEMAUTOLINEREDUCTION_W(Self: TMenuItem; const T: TMenuItemAutoFlag);
begin Self.AutoLineReduction := T; end;

procedure TMENUITEMAUTOLINEREDUCTION_R(Self: TMenuItem; var T: TMenuItemAutoFlag);
begin T := Self.AutoLineReduction; end;

procedure TMENUITEMAUTOHOTKEYS_W(Self: TMenuItem; const T: TMenuItemAutoFlag);
begin Self.AutoHotkeys := T; end;

procedure TMENUITEMAUTOHOTKEYS_R(Self: TMenuItem; var T: TMenuItemAutoFlag);
begin T := Self.AutoHotkeys; end;
{$ENDIF}

procedure TMENUITEMACTION_W(Self: TMenuItem; const T: TBasicAction);
begin Self.Action := T; end;

procedure TMENUITEMACTION_R(Self: TMenuItem; var T: TBasicAction);
begin T := Self.Action; end;

procedure TMENUITEMPARENT_R(Self: TMenuItem; var T: TMenuItem);
begin T := Self.Parent; end;

procedure TMENUITEMMENUINDEX_W(Self: TMenuItem; const T: Integer);
begin Self.MenuIndex := T; end;

procedure TMENUITEMMENUINDEX_R(Self: TMenuItem; var T: Integer);
begin T := Self.MenuIndex; end;

procedure TMENUITEMITEMS_R(Self: TMenuItem; var T: TMenuItem; const t1: Integer);
begin T := Self.Items[t1]; end;

procedure TMENUITEMCOUNT_R(Self: TMenuItem; var T: Integer);
begin T := Self.Count; end;

procedure TMENUITEMHANDLE_R(Self: TMenuItem; var T: HMENU);
begin T := Self.Handle; end;

procedure TMENUITEMCOMMAND_R(Self: TMenuItem; var T: WORD);
begin T := Self.Command; end;

procedure RIRegister_Menus_Routines(S: TPSExec);
begin
  S.RegisterDelphiFunction(@SHORTCUT, 'ShortCut', cdRegister);
  S.RegisterDelphiFunction(@SHORTCUTTOKEY, 'ShortCutToKey', cdRegister);
{$IFNDEF FPC}
  S.RegisterDelphiFunction(@SHORTCUTTOTEXT, 'ShortCutToText', cdRegister);
  S.RegisterDelphiFunction(@TEXTTOSHORTCUT, 'TextToShortCut', cdRegister);
  S.RegisterDelphiFunction(@NEWMENU, 'NewMenu', cdRegister);
  S.RegisterDelphiFunction(@NEWPOPUPMENU, 'NewPopupMenu', cdRegister);
  S.RegisterDelphiFunction(@NEWSUBMENU, 'NewSubMenu', cdRegister);
  S.RegisterDelphiFunction(@NEWITEM, 'NewItem', cdRegister);
  S.RegisterDelphiFunction(@NEWLINE, 'NewLine', cdRegister);
  S.RegisterDelphiFunction(@DRAWMENUITEM, 'DrawMenuItem', cdRegister);
{$ENDIF}
end;

{$IFNDEF FPC}
procedure RIRegisterTMENUITEMSTACK(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TMenuItemStack) do
  begin
    RegisterMethod(@TMenuItemStack.CLEARITEM, 'ClearItem');
  end;
end;

procedure RIRegisterTPOPUPLIST(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TPopupList) do
  begin
    RegisterPropertyHelper(@TPOPUPLISTWINDOW_R,nil,'Window');
    RegisterMethod(@TPopupList.Add, 'Add');
    RegisterMethod(@TPopupList.Remove, 'Remove');
  end;
end;
{$ENDIF}

procedure RIRegisterTPOPUPMENU(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TPopupMenu) do
  begin
    RegisterConstructor(@TPopupMenu.CREATE, 'Create');
    RegisterVirtualMethod(@TPopupMenu.POPUP, 'Popup');
    RegisterPropertyHelper(@TPOPUPMENUPOPUPCOMPONENT_R,@TPOPUPMENUPOPUPCOMPONENT_W,'PopupComponent');
    RegisterEventPropertyHelper(@TPOPUPMENUONPOPUP_R,@TPOPUPMENUONPOPUP_W,'OnPopup');
{$IFNDEF FPC}
    RegisterPropertyHelper(@TPOPUPMENUALIGNMENT_R,@TPOPUPMENUALIGNMENT_W,'Alignment');
    RegisterPropertyHelper(@TPOPUPMENUAUTOPOPUP_R,@TPOPUPMENUAUTOPOPUP_W,'AutoPopup');
    RegisterPropertyHelper(@TPOPUPMENUHELPCONTEXT_R,@TPOPUPMENUHELPCONTEXT_W,'HelpContext');
    RegisterPropertyHelper(@TPOPUPMENUMENUANIMATION_R,@TPOPUPMENUMENUANIMATION_W,'MenuAnimation');
    RegisterPropertyHelper(@TPOPUPMENUTRACKBUTTON_R,@TPOPUPMENUTRACKBUTTON_W,'TrackButton');
{$ENDIF}
  end;
end;

procedure RIRegisterTMAINMENU(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TMainMenu) do
  begin
{$IFNDEF FPC}
    RegisterMethod(@TMainMenu.Merge, 'Merge');
    RegisterMethod(@TMainMenu.UnMerge, 'Unmerge');
    RegisterMethod(@TMainMenu.PopulateOle2Menu, 'PopulateOle2Menu');
    RegisterMethod(@TMainMenu.GetOle2AcceleratorTable, 'GetOle2AcceleratorTable');
    RegisterMethod(@TMainMenu.SetOle2MenuHandle, 'SetOle2MenuHandle');
    RegisterPropertyHelper(@TMAINMENUAUTOMERGE_R,@TMAINMENUAUTOMERGE_W,'AutoMerge');
{$ENDIF}
  end;
end;

procedure RIRegisterTMENU(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TMenu) do
  begin
    RegisterConstructor(@TMenu.CREATE, 'Create');
    RegisterMethod(@TMenu.DISPATCHCOMMAND, 'DispatchCommand');
    RegisterMethod(@TMenu.FINDITEM, 'FindItem');
    RegisterPropertyHelper(@TMENUIMAGES_R,@TMENUIMAGES_W,'Images');
    RegisterMethod(@TMenu.ISRIGHTTOLEFT, 'IsRightToLeft');
    RegisterPropertyHelper(@TMENUHANDLE_R,nil,'Handle');
    RegisterPropertyHelper(@TMENUITEMS_R,nil,'Items');
{$IFNDEF FPC}
    RegisterMethod(@TMenu.DISPATCHPOPUP, 'DispatchPopup');
    RegisterMethod(@TMenu.PARENTBIDIMODECHANGED, 'ParentBiDiModeChanged');
    RegisterMethod(@TMenu.PROCESSMENUCHAR, 'ProcessMenuChar');
    RegisterPropertyHelper(@TMENUAUTOHOTKEYS_R,@TMENUAUTOHOTKEYS_W,'AutoHotkeys');
    RegisterPropertyHelper(@TMENUAUTOLINEREDUCTION_R,@TMENUAUTOLINEREDUCTION_W,'AutoLineReduction');
    RegisterPropertyHelper(@TMENUBIDIMODE_R,@TMENUBIDIMODE_W,'BiDiMode');
    RegisterMethod(@TMenu.GETHELPCONTEXT, 'GetHelpContext');
    RegisterPropertyHelper(@TMENUOWNERDRAW_R,@TMENUOWNERDRAW_W,'OwnerDraw');
    RegisterPropertyHelper(@TMENUPARENTBIDIMODE_R,@TMENUPARENTBIDIMODE_W,'ParentBiDiMode');
    RegisterPropertyHelper(@TMENUWINDOWHANDLE_R,@TMENUWINDOWHANDLE_W,'WindowHandle');
{$ENDIF}
  end;
end;

procedure RIRegisterTMENUITEM(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TMenuItem) do
  begin
    RegisterConstructor(@TMenuItem.Create, 'Create');
    RegisterVirtualMethod(@TMenuItem.InitiateAction, 'InitiateAction');
    RegisterMethod(@TMenuItem.Insert, 'Insert');
    RegisterMethod(@TMenuItem.Delete, 'Delete');
    RegisterMethod(@TMenuItem.Clear, 'Clear');
    RegisterVirtualMethod(@TMenuItem.Click, 'Click');
{$IFNDEF FPC}
    RegisterMethod(@TMenuItem.Find, 'Find');
    RegisterMethod(@TMenuItem.NewTopLine, 'NewTopLine');
    RegisterMethod(@TMenuItem.NewBottomLine, 'NewBottomLine');
    RegisterMethod(@TMenuItem.InsertNewLineBefore, 'InsertNewLineBefore');
    RegisterMethod(@TMenuItem.InsertNewLineAfter, 'InsertNewLineAfter');
    RegisterMethod(@TMenuItem.RethinkHotkeys, 'RethinkHotkeys');
    RegisterMethod(@TMenuItem.RethinkLines, 'RethinkLines');
    RegisterMethod(@TMenuItem.IsLine, 'IsLine');
{$ENDIF}
    RegisterMethod(@TMenuItem.IndexOf, 'IndexOf');
    RegisterMethod(@TMenuItem.GetImageList, 'GetImageList');
    RegisterMethod(@TMenuItem.GetParentComponent, 'GetParentComponent');
    RegisterMethod(@TMenuItem.GetParentMenu, 'GetParentMenu');
    RegisterMethod(@TMenuItem.HasParent, 'HasParent');
    RegisterMethod(@TMenuItem.Add, 'Add');
    RegisterMethod(@TMenuItem.Remove, 'Remove');
{$IFNDEF FPC}
    RegisterPropertyHelper(@TMENUITEMAUTOHOTKEYS_R,@TMENUITEMAUTOHOTKEYS_W,'AutoHotkeys');
    RegisterPropertyHelper(@TMENUITEMAUTOLINEREDUCTION_R,@TMENUITEMAUTOLINEREDUCTION_W,'AutoLineReduction');
    RegisterEventPropertyHelper(@TMENUITEMONDRAWITEM_R,@TMENUITEMONDRAWITEM_W,'OnDrawItem');
    RegisterEventPropertyHelper(@TMENUITEMONADVANCEDDRAWITEM_R,@TMENUITEMONADVANCEDDRAWITEM_W,'OnAdvancedDrawItem');
    RegisterEventPropertyHelper(@TMENUITEMONMEASUREITEM_R,@TMENUITEMONMEASUREITEM_W,'OnMeasureItem');
{$ENDIF}
    RegisterPropertyHelper(@TMENUITEMCOMMAND_R,nil,'Command');
    RegisterPropertyHelper(@TMENUITEMHANDLE_R,nil,'Handle');
    RegisterPropertyHelper(@TMENUITEMCOUNT_R,nil,'Count');
    RegisterPropertyHelper(@TMENUITEMITEMS_R,nil,'Items');
    RegisterPropertyHelper(@TMENUITEMMENUINDEX_R,@TMENUITEMMENUINDEX_W,'MenuIndex');
    RegisterPropertyHelper(@TMENUITEMPARENT_R,nil,'Parent');
    RegisterPropertyHelper(@TMENUITEMACTION_R,@TMENUITEMACTION_W,'Action');
    RegisterPropertyHelper(@TMENUITEMBITMAP_R,@TMENUITEMBITMAP_W,'Bitmap');
    RegisterPropertyHelper(@TMENUITEMCAPTION_R,@TMENUITEMCAPTION_W,'Caption');
    RegisterPropertyHelper(@TMENUITEMCHECKED_R,@TMENUITEMCHECKED_W,'Checked');
    RegisterPropertyHelper(@TMENUITEMSUBMENUIMAGES_R,@TMENUITEMSUBMENUIMAGES_W,'SubMenuImages');
    RegisterPropertyHelper(@TMENUITEMDEFAULT_R,@TMENUITEMDEFAULT_W,'Default');
    RegisterPropertyHelper(@TMENUITEMENABLED_R,@TMENUITEMENABLED_W,'Enabled');
    RegisterPropertyHelper(@TMENUITEMGROUPINDEX_R,@TMENUITEMGROUPINDEX_W,'GroupIndex');
    RegisterPropertyHelper(@TMENUITEMHELPCONTEXT_R,@TMENUITEMHELPCONTEXT_W,'HelpContext');
    RegisterPropertyHelper(@TMENUITEMHINT_R,@TMENUITEMHINT_W,'Hint');
    RegisterPropertyHelper(@TMENUITEMIMAGEINDEX_R,@TMENUITEMIMAGEINDEX_W,'ImageIndex');
    RegisterPropertyHelper(@TMENUITEMRADIOITEM_R,@TMENUITEMRADIOITEM_W,'RadioItem');
    RegisterPropertyHelper(@TMENUITEMSHORTCUT_R,@TMENUITEMSHORTCUT_W,'ShortCut');
    RegisterPropertyHelper(@TMENUITEMVISIBLE_R,@TMENUITEMVISIBLE_W,'Visible');
    RegisterEventPropertyHelper(@TMENUITEMONCLICK_R,@TMENUITEMONCLICK_W,'OnClick');
  end;
end;

procedure RIRegister_Menus(CL: TPSRuntimeClassImporter);
begin
  RIRegisterTMENUITEM(Cl);
  RIRegisterTMENU(Cl);
  RIRegisterTPOPUPMENU(Cl);
  RIRegisterTMAINMENU(Cl);
  {$IFNDEF FPC}
  RIRegisterTPOPUPLIST(Cl);
  RIRegisterTMENUITEMSTACK(Cl);
  {$ENDIF}
end;

end.
