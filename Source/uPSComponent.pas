{ uPSComponent.pas } // version: 2020.1010.1010
{----------------------------------------------------------------------------}
{ RemObjects Pascal Script                                                   }
{----------------------------------------------------------------------------}
unit uPSComponent;
{$I PascalScript.inc}

interface

uses
  {$IFDEF DELPHI7UP}Types,{$ENDIF}
  //{$IFDEF DELPHI17UP}System.UITypes,{$ENDIF}
  SysUtils, Classes, uPSRuntime, uPSDebugger, uPSUtils,
  uPSCompiler,
  {$IF DEFINED (MSWINDOWS) OR Defined (UNIX) OR Defined (fpc)} uPSC_dll, uPSR_dll,{$IFEND}
  uPSPreProcessor;

const
  cdRegister = uPSRuntime.cdRegister; {alias to @link(ifps3.cdRegister)}
  cdPascal = uPSRuntime.cdPascal; {alias to @link(ifps3.cdPascal)}
  CdCdecl = uPSRuntime.CdCdecl;
  CdStdCall = uPSRuntime.CdStdCall;

type
  TPSScript = class;

  TDelphiCallingConvention = uPSRuntime.TPSCallingConvention;
  {Alias to @link(ifps3.TPSRuntimeClassImporter)}
  TPSRuntimeClassImporter = uPSRuntime.TPSRuntimeClassImporter;

  TPSPlugin = class(TComponent)
  public
    procedure CompOnUses({%H-}CompExec: TPSScript); virtual;
    procedure ExecOnUses({%H-}CompExec: TPSScript); virtual;
    procedure CompileImport1({%H-}CompExec: TPSScript); virtual;
    procedure CompileImport2({%H-}CompExec: TPSScript); virtual;
    procedure ExecImport1({%H-}CompExec: TPSScript; const {%H-}ri: TPSRuntimeClassImporter); virtual;
    procedure ExecImport2({%H-}CompExec: TPSScript; const {%H-}ri: TPSRuntimeClassImporter); virtual;
  end;

  TIFPS3Plugin = class(TPSPlugin);

  TPSDllPlugin = class(TPSPlugin)
  public
    procedure CompOnUses(CompExec: TPSScript); override;
    procedure ExecOnUses(CompExec: TPSScript); override;
  end;

  TIFPS3DllPlugin = class(TPSDllPlugin);

  TPSPluginItem = class(TCollectionItem)
  private
    FPlugin: TPSPlugin;
    procedure SetPlugin(const Value: TPSPlugin);
  protected
    function GetDisplayName: string; override;
  public
    procedure Assign(Source: TPersistent); override; //Birb
  published
    property Plugin: TPSPlugin read FPlugin write SetPlugin;
  end;

  TIFPS3CEPluginItem = class(TPSPluginItem);

  TPSPlugins = class(TCollection)
  private
    FCompExec: TPSScript;
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(CE: TPSScript);
  end;

  TIFPS3CEPlugins = class(TPSPlugins);

  TPSOnGetNotVariant = function (Sender: TPSScript; const Name: TbtString): Variant of object;
  TPSOnSetNotVariant = procedure (Sender: TPSScript; const Name: TbtString; V: Variant) of object;
  TPSCompOptions = set of (icAllowNoBegin, icAllowUnit, icAllowNoEnd, icBooleanShortCircuit);

  TPSVerifyProc = procedure (Sender: TPSScript; Proc: TPSInternalProcedure; const Decl: TbtString; var Error: Boolean) of object;

  TPSEvent = procedure (Sender: TPSScript) of object;

  TPSOnCompImportEvent = procedure (Sender: TObject; x: TPSPascalCompiler) of object;

  TPSOnExecImportEvent = procedure (Sender: TObject; se: TPSExec; x: TPSRuntimeClassImporter) of object;
  {Script engine event function}
  TPSOnNeedFile = function (Sender: TObject; const OrginFileName: TbtString; var FileName, Output: TbtString): Boolean of object;

  TPSOnProcessDirective = procedure (
                            Sender: TPSPreProcessor;
                            Parser: TPSPascalPreProcessorParser;
                            const Active: Boolean;
                            const DirectiveName, DirectiveParam: TbtString;
                            Var Continue: Boolean) of Object;  // jgv

  {+}
  TLoadDebugInfoState = (ldisNotLoaded, ldisLoaded, ldisFailed);
  {+.}

  TPSScript = class(TComponent)
  private
    FOnGetNotificationVariant: TPSOnGetNotVariant;
    FOnSetNotificationVariant: TPSOnSetNotVariant;
    FCanAdd: Boolean;
    FComp: TPSPascalCompiler;
    FCompOptions: TPSCompOptions;
    FExec: TPSDebugExec;
    FSuppressLoadData: Boolean;
    FScript: TStrings;
    FOnLine: TNotifyEvent;
    FUseDebugInfo: Boolean;
    {+}
    fLoadDebugInfoState: TLoadDebugInfoState;
    {+.}
    FOnAfterExecute, FOnCompile, FOnExecute: TPSEvent;
    FOnCompImport: TPSOnCompImportEvent;
    FOnExecImport: TPSOnExecImportEvent;
    RI: TPSRuntimeClassImporter;
    FPlugins: TPSPlugins;
    FPP: TPSPreProcessor;
    FMainFileName: TbtString;
    FOnNeedFile: TPSOnNeedFile;
    FUsePreProcessor: Boolean;
    FDefines: TStrings;
    FOnVerifyProc: TPSVerifyProc;
    FOnProcessDirective: TPSOnProcessDirective;
    FOnProcessUnknowDirective: TPSOnProcessDirective;
    FOnFindUnknownFile: TPSOnNeedFile;
    function GetRunning: Boolean;
    procedure SetScript(const Value: TStrings);
    function GetCompMsg(i: Integer): TPSPascalCompilerMessage;
    function GetCompMsgCount: Longint;
    function GetAbout: TbtString;
    function ScriptUses(Sender: TPSPascalCompiler; const Name: TbtString): Boolean;
    function GetExecErrorByteCodePosition: Cardinal;
    function GetExecErrorCode: TIFError;
    function GetExecErrorParam: TbtString;
    function GetExecErrorProcNo: Cardinal;
    function GetExecErrorString: TbtString;
    function GetExecErrorPosition: Cardinal;
    function GetExecErrorCol: Cardinal;
    function GetExecErrorRow: Cardinal;
    function GetExecErrorFileName: TbtString;
    procedure SetDefines(const Value: TStrings);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  protected
    //jgv move where private before - not very usefull
    procedure OnLineEvent; virtual;
    procedure SetMainFileName(const Value: TbtString); virtual;

    //--jgv new
    function  DoOnNeedFile (Sender: TObject; const OrginFileName: TbtString; var FileName, Output: TbtString): Boolean; virtual;
    function  DoOnUnknowUses (Sender: TPSPascalCompiler; const Name: TbtString): Boolean; virtual; // return true if processed
    procedure DoOnCompImport; virtual;
    procedure DoOnCompile; virtual;
    function  DoVerifyProc (Sender: TPSScript; Proc: TPSInternalProcedure; const Decl: TbtString): Boolean; virtual;

    procedure DoOnExecImport (RunTimeImporter: TPSRuntimeClassImporter); virtual;
    procedure DoOnExecute ({%H-}RunTimeImporter: TPSRuntimeClassImporter); virtual;
    procedure DoAfterExecute; virtual;
    function  DoOnGetNotificationVariant (const Name: TbtString): Variant; virtual;
    procedure DoOnSetNotificationVariant (const Name: TbtString; V: Variant); virtual;

    procedure DoOnProcessDirective (Sender: TPSPreProcessor;
                Parser: TPSPascalPreProcessorParser;
                const Active: Boolean;
                const DirectiveName, DirectiveParam: TbtString;
                Var Continue: Boolean); virtual;
    procedure DoOnProcessUnknowDirective (Sender: TPSPreProcessor;
                Parser: TPSPascalPreProcessorParser;
                const Active: Boolean;
                const DirectiveName, DirectiveParam: TbtString;
                Var Continue: Boolean); virtual;
    {+}
    procedure DoOnLoadDebugInfo(Sender: TPSExec; var OK: Boolean);
    {+.}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property RuntimeImporter: TPSRuntimeClassImporter read RI;

    function FindNamedType(const Name: TbtString): TPSTypeRec;
    function FindBaseType(Bt: TPSBaseType): TPSTypeRec;

    property SuppressLoadData: Boolean read FSuppressLoadData write FSuppressLoadData;

    {+}
    function LoadDebugInfo(): Boolean; // Allows downloading of debugging information dynamically as needed!
    {+.}
    function LoadExec: Boolean;

    procedure Stop; virtual;
    function Compile: Boolean; virtual;
    function Execute: Boolean; virtual;
    property Running: Boolean read GetRunning;

    procedure GetCompiled(var data: TbtString);
    procedure SetCompiled(const Data: TbtString);

    property Comp: TPSPascalCompiler read FComp;
    property Exec: TPSDebugExec read FExec;
    property CompilerMessageCount: Longint read GetCompMsgCount;
    property CompilerMessages[i: Longint]: TPSPascalCompilerMessage read GetCompMsg;
    function CompilerErrorToStr(I: Longint): TbtString;

    {+}
    property LoadDebugInfoState: TLoadDebugInfoState read fLoadDebugInfoState;
    {+.}
    property ExecErrorCode: TIFError read GetExecErrorCode;
    property ExecErrorParam: TbtString read GetExecErrorParam;
    property ExecErrorToString: TbtString read GetExecErrorString;
    property ExecErrorProcNo: Cardinal read GetExecErrorProcNo;
    property ExecErrorByteCodePosition: Cardinal read GetExecErrorByteCodePosition;
    property ExecErrorPosition: Cardinal read GetExecErrorPosition;
    property ExecErrorRow: Cardinal read GetExecErrorRow;
    property ExecErrorCol: Cardinal read GetExecErrorCol;
    property ExecErrorFileName: TbtString read GetExecErrorFileName;

    function AddFunctionEx(Ptr: Pointer; const Decl: TbtString; CallingConv: TDelphiCallingConvention): Boolean;
    function AddFunction(Ptr: Pointer; const Decl: TbtString): Boolean;
    function AddMethodEx(Slf, Ptr: Pointer; const Decl: TbtString; CallingConv: TDelphiCallingConvention): Boolean;
    function AddMethod(Slf, Ptr: Pointer; const Decl: TbtString): Boolean;
    function AddRegisteredVariable(const VarName, VarType: TbtString): Boolean;
    function AddNotificationVariant(const VarName: TbtString): Boolean;
    function AddRegisteredPTRVariable(const VarName, VarType: TbtString): Boolean;

    function GetVariable(const Name: TbtString): PIFVariant;

    function SetVarToInstance(const VarName: TbtString; cl: TObject): Boolean;
    procedure SetPointerToData(const VarName: TbtString; Data: Pointer; aType: TIFTypeRec);

    function TranslatePositionPos({%H-}Proc, {%H-}Position: Cardinal; var Pos: Cardinal; var fn: TbtString): Boolean; //{+} overload;
    //function TranslatePositionPos(Proc, Position: Cardinal; var Pos: Cardinal): Boolean; overload; {+.}
    function TranslatePositionRC(Proc, Position: Cardinal; var Row, Col: Cardinal; var fn: TbtString): Boolean; {+} overload;
    function TranslatePositionRC(Proc, Position: Cardinal; var Row, Col: Cardinal ): Boolean; overload; {+.}

    function GetProcMethod(const ProcName: TbtString): TMethod;
    function ExecuteFunction(const Params: array of Variant; const ProcName: TbtString): Variant;
  published
    property About: TbtString read GetAbout stored false;
    property Script: TStrings read FScript write SetScript;
    property CompilerOptions: TPSCompOptions read FCompOptions write FCompOptions;
    property OnLine: TNotifyEvent read FOnLine write FOnLine;
    property OnCompile: TPSEvent read FOnCompile write FOnCompile;
    property OnExecute: TPSEvent read FOnExecute write FOnExecute;
    property OnAfterExecute: TPSEvent read FOnAfterExecute write FOnAfterExecute;
    property OnCompImport: TPSOnCompImportEvent read FOnCompImport write FOnCompImport;
    property OnExecImport: TPSOnExecImportEvent read FOnExecImport write FOnExecImport;
    property UseDebugInfo: Boolean read FUseDebugInfo write FUseDebugInfo default True;
    property Plugins: TPSPlugins read FPlugins write FPlugins;
    property MainFileName: TbtString read FMainFileName write SetMainFileName;
    property UsePreProcessor: Boolean read FUsePreProcessor write FUsePreProcessor;
    property OnNeedFile: TPSOnNeedFile read FOnNeedFile write FOnNeedFile;
    property Defines: TStrings read FDefines write SetDefines;
    property OnVerifyProc: TPSVerifyProc read FOnVerifyProc write FOnVerifyProc;
    property OnGetNotificationVariant: TPSOnGetNotVariant read FOnGetNotificationVariant write FOnGetNotificationVariant;
    property OnSetNotificationVariant: TPSOnSetNotVariant read FOnSetNotificationVariant write FOnSetNotificationVariant;
    property OnFindUnknownFile: TPSOnNeedFile read FOnFindUnknownFile write FOnFindUnknownFile;
  published //-- jgv
    property OnProcessDirective: TPSOnProcessDirective read FOnProcessDirective write FOnProcessDirective;
    property OnProcessUnknowDirective: TPSOnProcessDirective read FOnProcessUnknowDirective write FOnProcessUnknowDirective;
  end;

  TIFPS3CompExec = class(TPSScript);

  TPSBreakPointInfo = class
  private
    FLine: Longint;
    FFileNameHash: Longint;
    FFileName: TbtString;
    procedure SetFileName(const Value: TbtString);
  public
    property FileName: TbtString read FFileName write SetFileName;
    property FileNameHash: Longint read FFileNameHash;
    property Line: Longint read FLine write FLine;
  end;

  TPSOnLineInfo = procedure (Sender: TObject; const FileName: TbtString; Position, Row, Col: Cardinal) of object;

  TPSScriptDebugger = class(TPSScript)
  private
    FOnIdle: TNotifyEvent;
    FBreakPoints: TIFList;
    FOnLineInfo: TPSOnLineInfo;
    FLastRow: Cardinal;
    FOnBreakpoint: TPSOnLineInfo;
    function GetBreakPoint(I: Integer): TPSBreakPointInfo;
    function GetBreakPointCount: Longint;
  protected
    procedure SetMainFileName(const Value: TbtString); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Pause; virtual;
    procedure Resume; virtual;
    procedure StepInto; virtual;
    procedure StepOver; virtual;
    procedure SetBreakPoint(const Fn: TbtString; Line: Longint);
    procedure ClearBreakPoint(const Fn: TbtString; Line: Longint);
    property BreakPointCount: Longint read GetBreakPointCount;
    property BreakPoint[I: Longint]: TPSBreakPointInfo read GetBreakPoint;
    function HasBreakPoint(const Fn: TbtString; Line: Longint): Boolean;
    procedure ClearBreakPoints;
    function GetVarContents(const Name: TbtString): TbtString;
    function GetVarValue(const Name: TbtString): Pointer;
  published
    property OnIdle: TNotifyEvent read FOnIdle write FOnIdle;
    property OnLineInfo: TPSOnLineInfo read FOnLineInfo write FOnLineInfo;
    property OnBreakpoint: TPSOnLineInfo read FOnBreakpoint write FOnBreakpoint;
  end;

  TIFPS3DebugCompExec = class(TPSScriptDebugger);

  TPSCustomPlugin = class(TPSPlugin)
  private
    FOnCompileImport2: TPSEvent;
    FOnExecOnUses: TPSEvent;
    FOnCompOnUses: TPSEvent;
    FOnCompileImport1: TPSEvent;
    FOnExecImport1: TPSOnExecImportEvent;
    FOnExecImport2: TPSOnExecImportEvent;
  public
    procedure CompOnUses(CompExec: TPSScript); override;
    procedure ExecOnUses(CompExec: TPSScript); override;
    procedure CompileImport1(CompExec: TPSScript); override;
    procedure CompileImport2(CompExec: TPSScript); override;
    procedure ExecImport1(CompExec: TPSScript; const ri: TPSRuntimeClassImporter); override;
    procedure ExecImport2(CompExec: TPSScript; const ri: TPSRuntimeClassImporter); override;
  published
    property OnCompOnUses : TPSEvent read FOnCompOnUses write FOnCompOnUses;
    property OnExecOnUses: TPSEvent read FOnExecOnUses write FOnExecOnUses;
    property OnCompileImport1: TPSEvent read FOnCompileImport1 write FOnCompileImport1;
    property OnCompileImport2: TPSEvent read FOnCompileImport2 write FOnCompileImport2;
    property OnExecImport1: TPSOnExecImportEvent read FOnExecImport1 write FOnExecImport1;
    property OnExecImport2: TPSOnExecImportEvent read FOnExecImport2 write FOnExecImport2;
  end;

implementation

{+}
{$IFDEF DELPHI12UP}
uses
  AnsiStrings;
{$ENDIF}

type
  EPSScriptError = class(EPSError);
  Exception = EPSScriptError;
{+.}

{$if (defined(DELPHI3UP) or defined(FPC))}
resourcestring
{$else}
const
{$ifend}
  RPS_UnableToReadVariant = 'Unable to read variant';
  RPS_UnableToWriteVariant = 'Unable to write variant';
  RPS_ScripEngineAlreadyRunning = 'Script engine already running';
  RPS_ScriptNotCompiled = 'Script is not compiled';
  RPS_NotRunning = 'Not running';
  RPS_UnableToFindVariable = 'Unable to find variable';
  RPS_UnknownIdentifier = 'Unknown Identifier';
  RPS_NoScript = 'No script';

function MyGetVariant(Sender: TPSExec; const Name: TbtString): Variant;
begin
  Result := TPSScript (Sender.Id).DoOnGetNotificationVariant(Name);
end;

procedure MySetVariant(Sender: TPSExec; const Name: TbtString; V: Variant);
begin
  TPSScript (Sender.Id).DoOnSetNotificationVariant(Name, V);
end;

function CompScriptUses(Sender: TPSPascalCompiler; const Name: TbtString): Boolean;
begin
  Result := TPSScript(Sender.ID).ScriptUses(Sender, Name);
end;

procedure ExecOnLine(Sender: TPSExec);
begin
  if assigned(TPSScript(Sender.ID).FOnLine) then
  begin
    TPSScript(Sender.ID).OnLineEvent;
  end;
end;

function CompExportCheck(Sender: TPSPascalCompiler; Proc: TPSInternalProcedure; const ProcDecl: TbtString): Boolean;
begin
  Result := TPSScript(Sender.ID).DoVerifyProc (Sender.ID, Proc, ProcDecl);
end;

procedure callObjectOnProcessDirective (
  Sender: TPSPreProcessor;
  Parser: TPSPascalPreProcessorParser;
  const Active: Boolean;
  const DirectiveName, DirectiveParam: TbtString;
  Var Continue: Boolean);
begin
  TPSScript (Sender.ID).DoOnProcessDirective(Sender, Parser, Active, DirectiveName, DirectiveParam, Continue);
end;

procedure callObjectOnProcessUnknowDirective (
  Sender: TPSPreProcessor;
  Parser: TPSPascalPreProcessorParser;
  const Active: Boolean;
  const DirectiveName, DirectiveParam: TbtString;
  Var Continue: Boolean);
begin
  TPSScript (Sender.ID).DoOnProcessUnknowDirective(Sender, Parser, Active, DirectiveName, DirectiveParam, Continue);
end;

{ TPSPlugin }
procedure TPSPlugin.CompileImport1(CompExec: TPSScript);
begin
  // do nothing
end;

procedure TPSPlugin.CompileImport2(CompExec: TPSScript);
begin
  // do nothing
end;

procedure TPSPlugin.CompOnUses(CompExec: TPSScript);
begin
 // do nothing
end;

procedure TPSPlugin.ExecImport1(CompExec: TPSScript; const ri: TPSRuntimeClassImporter);
begin
  // do nothing
end;

procedure TPSPlugin.ExecImport2(CompExec: TPSScript; const ri: TPSRuntimeClassImporter);
begin
  // do nothing
end;

procedure TPSPlugin.ExecOnUses(CompExec: TPSScript);
begin
 // do nothing
end;

{ TPSScript }

function TPSScript.AddFunction(Ptr: Pointer;
  const Decl: TbtString): Boolean;
begin
  Result := AddFunctionEx(Ptr, Decl, cdRegister);
end;

function TPSScript.AddFunctionEx(Ptr: Pointer; const Decl: TbtString;
  CallingConv: TDelphiCallingConvention): Boolean;
var
  P: TPSRegProc;
begin
  if not FCanAdd then begin Result := False; exit; end;
  p := Comp.AddDelphiFunction(Decl);
  if p <> nil then
  begin
    Exec.RegisterDelphiFunction(Ptr, p.Name, CallingConv);
    Result := True;
  end else Result := False;
end;

function TPSScript.AddRegisteredVariable(const VarName,
  VarType: TbtString): Boolean;
var
  FVar: TPSVar;
begin
  if not FCanAdd then begin Result := False; exit; end;
  FVar := FComp.AddUsedVariableN(varname, vartype);
  if fvar = nil then
    result := False
  else begin
    fvar.exportname := fvar.Name;
    Result := True;
  end;
end;

function CENeedFile(Sender: TPSPreProcessor; const callingfilename: TbtString; var FileName, Output: TbtString): Boolean;
begin
  Result := TPSScript (Sender.ID).DoOnNeedFile(Sender.ID, CallingFileName, FileName, Output);
end;

procedure CompTranslateLineInfo(Sender: TPSPascalCompiler; var Pos, Row, Col: Cardinal; var Name: TbtString);
var res: TPSLineInfoResults;
begin
  if TPSScript(Sender.ID).FPP.CurrentLineInfo.GetLineInfo(Name, Pos, {%H-}Res) then begin
    Pos := Res.Pos;
    Row := Res.Row;
    Col := Res.Col;
    Name := Res.Name;
  end;
end;

function TPSScript.Compile: Boolean;
var
  i: Longint;
  dta: TbtString;
begin
  FExec.Clear;
  FExec.CMD_Err(erNoError);
  FExec.ClearspecialProcImports;
  FExec.ClearFunctionList;
  if ri <> nil then
  begin
    RI.Free;
    RI := nil;
  end;
  RI := TPSRuntimeClassImporter.Create;
  for i := 0 to FPlugins.Count -1 do
  begin
    if (TPSPluginItem(FPlugins.Items[i]) <> nil) and (TPSPluginItem(FPlugins.Items[i]).Plugin <> nil) then
      TPSPluginItem(FPlugins.Items[i]).Plugin.ExecImport1(Self, ri);
  end;

  DoOnExecImport (RI);

  for i := 0 to FPlugins.Count -1 do
  begin
    if (TPSPluginItem(FPlugins.Items[i]) <> nil)and (TPSPluginItem(FPlugins.Items[i]).Plugin <> nil) then
      TPSPluginItem(FPlugins.Items[i]).Plugin.ExecImport2(Self, ri);
  end;
  RegisterClassLibraryRuntime(Exec, RI);
  for i := 0 to FPlugins.Count -1 do
  begin
    if (TPSPluginItem(FPlugins.Items[i]) <> nil)and (TPSPluginItem(FPlugins.Items[i]).Plugin <> nil) then
      TPSPluginItem(FPlugins.Items[i]).Plugin.ExecOnUses(Self);
  end;
  FCanAdd := True;
  FComp.BooleanShortCircuit := icBooleanShortCircuit in FCompOptions;
  FComp.AllowNoBegin := icAllowNoBegin in FCompOptions;
  FComp.AllowUnit := icAllowUnit in FCompOptions;
  FComp.AllowNoEnd := icAllowNoEnd in FCompOptions;
  if FUsePreProcessor then
  begin
    FPP.Clear;
    FPP.Defines.Assign(FDefines);
    FComp.OnTranslateLineInfo := CompTranslateLineInfo;
    FPP.OnProcessDirective := callObjectOnProcessDirective;
    FPP.OnProcessUnknowDirective := callObjectOnProcessUnknowDirective;
    FPP.MainFile := {+}TbtString(FScript.Text){+.};
    FPP.MainFileName := FMainFileName;
    Fpp.PreProcess(FMainFileName, {%H-}dta);
    if FComp.Compile(dta) then
    begin
      FCanAdd := False;
      if (not SuppressLoadData) and (not LoadExec) then begin
        {+}
        FComp.MakeError('', ecInternalError, TbtString('Failed Define/Load Exec'));
        {+.}
        Result := False;
      end else Result := True;
    end else Result := False;
    FPP.AdjustMessages(Comp);
  end else
  begin
    FComp.OnTranslateLineInfo := nil;
    if FComp.Compile({+}TbtString(FScript.Text){+.}) then
    begin
      FCanAdd := False;
      if not LoadExec then
      begin
        {+}
        FComp.MakeError('', ecInternalError, TbtString('Failed Define/Load Exec'));
        {+.}
        Result := False;
      end else
        Result := True;
    end else Result := False;
  end;
end;

function TPSScript.CompilerErrorToStr(I: Integer): TbtString;
begin
  Result := CompilerMessages[i].MessageToString;
end;

constructor TPSScript.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  {+}
  FCompOptions := [icAllowNoBegin, icAllowUnit, icAllowNoEnd, icBooleanShortCircuit{!!!==$B-}];
  {+.}
  FComp := TPSPascalCompiler.Create;
  FExec := TPSDebugExec.Create;
  {+}
  FExec.OnLoadDebugInfo := DoOnLoadDebugInfo;
  {+.}
  FScript := TStringList.Create;
  FPlugins := TPSPlugins.Create(self);

  FComp.ID := Self;
  FComp.OnUses := CompScriptUses;
  FComp.OnExportCheck := CompExportCheck;
  FExec.Id := Self;
  FExec.OnRunLine:= ExecOnLine;
  FExec.OnGetNVariant := MyGetVariant;
  FExec.OnSetNVariant := MySetVariant;

  FUseDebugInfo := True; // !!! NB: Needed "TRUE" for detect line when errors !!!

  FPP := TPSPreProcessor.Create;
  FPP.Id := Self;
  FPP.OnNeedFile := CENeedFile;

  FDefines := TStringList.Create;
end;

destructor TPSScript.Destroy;
begin
  FreeAndNil(FDefines);

  FreeAndNil(FPP);
  FreeAndNil(RI);
  FreeAndNil(FPlugins);
  FreeAndNil(FScript);
  FreeAndNil(FExec);
  FreeAndNil(FComp);
  inherited;
end;

function TPSScript.Execute: Boolean;
begin
  if Running then
    raise Exception.Create(RPS_ScripEngineAlreadyRunning);
  if SuppressLoadData then
    LoadExec;

  DoOnExecute (RI);

  FExec.DebugEnabled := FUseDebugInfo;
  Result := FExec.RunScript and (FExec.ExceptionCode = erNoError) ;

  DoAfterExecute;
end;

function TPSScript.GetAbout: TbtString;
begin
  Result := TPSExec.About;
end;

procedure TPSScript.GetCompiled(var data: TbtString);
begin
  if not FComp.GetOutput(Data) then
    raise Exception.Create(RPS_ScriptNotCompiled);
end;

function TPSScript.GetCompMsg(i: Integer): TPSPascalCompilerMessage;
begin
  Result := FComp.Msg[i];
end;

function TPSScript.GetCompMsgCount: Longint;
begin
  Result := FComp.MsgCount;
end;

function TPSScript.GetExecErrorByteCodePosition: Cardinal;
begin
  Result := Exec.ExceptionPos;
end;

function TPSScript.GetExecErrorCode: TIFError;
begin
  Result := Exec.ExceptionCode;
end;

function TPSScript.GetExecErrorParam: TbtString;
begin
  Result := Exec.ExceptionString;
end;

function TPSScript.GetExecErrorPosition: Cardinal;
begin
  Result := FExec.TranslatePosition(Exec.ExceptionProcNo, Exec.ExceptionPos);
end;

function TPSScript.GetExecErrorProcNo: Cardinal;
begin
  Result := Exec.ExceptionProcNo;
end;

function TPSScript.GetExecErrorString: TbtString;
begin
  Result := TIFErrorToString(Exec.ExceptionCode, Exec.ExceptionString);
end;

function TPSScript.GetVariable(const Name: TbtString): PIFVariant;
begin
  Result := FExec.GetVar2(name);
end;

function TPSScript.LoadDebugInfo(): Boolean;
// Allows downloading of debugging information dynamically as needed!
var S: TbtString;
begin
  if fLoadDebugInfoState <> ldisNotLoaded then
    Result := fLoadDebugInfoState = ldisLoaded
  else
  begin
    Result := FComp.GetDebugOutput({%H-}S);
    if Result then
      Result := FExec.LoadDebugData(S) > 0; // @dbg: FExec.FStatus = isNotLoaded
    if Result then
      fLoadDebugInfoState := ldisLoaded
    else
      fLoadDebugInfoState := ldisFailed;
  end;
end;

procedure TPSScript.DoOnLoadDebugInfo(Sender: TPSExec; var OK: Boolean);
begin
  OK := LoadDebugInfo();
end;

function TPSScript.LoadExec: Boolean;
var OK: Boolean; S: TbtString;
begin
  {+}
  fLoadDebugInfoState := ldisNotLoaded;
  OK := FComp.GetOutput({%H-}S);
  if OK then
    OK := FExec.LoadData(S);
  if not OK then
  begin
    Result := False;
    exit;
  end;
  if FUseDebugInfo then
  begin
    OK := LoadDebugInfo(); // @dbg: FExec.FStatus = isNotLoaded
    if OK then
    //else
    //  FUseDebugInfo := False // failed load debug info
    ;
  end;
  Result := True;
  {+.}
end;

function TPSScript.ScriptUses(Sender: TPSPascalCompiler; const Name: TbtString): Boolean;
var
  i: Longint;
  {+}
  aPluginItem: TPSPluginItem;
  aMsg: TPSPascalCompilerMessage;
  {+.}
begin
  if Name = 'SYSTEM' then begin
    for i := 0 to FPlugins.Count -1 do begin
    {+}
      aPluginItem := TPSPluginItem(FPlugins.Items[i]);
      if (aPluginItem <> nil)and (aPluginItem.Plugin <> nil) then
        aPluginItem.Plugin.CompOnUses(Self); // @dbg: aPluginItem.Plugin.ClassType
    {+.}
    end;

    for i := 0 to FPlugins.Count -1 do begin
    {+}
      aPluginItem := TPSPluginItem(FPlugins.Items[i]);
      if (aPluginItem <> nil)and (aPluginItem.Plugin <> nil) then
        aPluginItem.Plugin.CompileImport1(Self); // @dbg: aPluginItem.Plugin.ClassType
    {+.}
    end;

    DoOnCompImport;

    for i := 0 to FPlugins.Count -1 do begin
    {+}
      aPluginItem := TPSPluginItem(FPlugins.Items[i]);
      if (aPluginItem <> nil)and (aPluginItem.Plugin <> nil) then
        aPluginItem.Plugin.CompileImport2(Self); // @dbg: aPluginItem.Plugin.ClassType
    {+.}
    end;

    DoOnCompile;

    Result := True;
    for i := 0 to Sender.MsgCount -1 do begin
    {+}
      aMsg := Sender.Msg[i];
      //if aMsg is TPSPascalCompilerError then // not any inheritance : class(TPSPascalCompilerError)
      if (aMsg <> nil) and (aMsg.ClassType = TPSPascalCompilerError) then // best perfomance
      begin
        Result := false;
        Break; //!!!
      end;
    {+.}
    end;
  end else begin
    Result := DoOnUnknowUses(Sender, Name);
    {If Not Result then
       Sender.MakeError('', ecUnknownIdentifier, Name);}
  end;
end;

procedure TPSScript.SetCompiled(const Data: TbtString);
var
  i: Integer;
begin
  FExec.Clear;
  FExec.ClearspecialProcImports;
  FExec.ClearFunctionList;
  if ri <> nil then
  begin
    RI.Free;
    RI := nil;
  end;
  RI := TPSRuntimeClassImporter.Create;
  for i := 0 to FPlugins.Count -1 do
  begin
    if (TPSPluginItem(FPlugins.Items[i]) <> nil)and (TPSPluginItem(FPlugins.Items[i]).Plugin <> nil) then
        TPSPluginItem(FPlugins.Items[i]).Plugin.ExecImport1(Self, ri);
  end;

  DoOnExecImport(RI);

  for i := 0 to FPlugins.Count -1 do
  begin
    if (TPSPluginItem(FPlugins.Items[i]) <> nil)and (TPSPluginItem(FPlugins.Items[i]).Plugin <> nil) then
      TPSPluginItem(FPlugins.Items[i]).Plugin.ExecImport2(Self, ri);
  end;
  RegisterClassLibraryRuntime(Exec, RI);
  for i := 0 to FPlugins.Count -1 do
  begin
    if (TPSPluginItem(FPlugins.Items[i]) <> nil)and (TPSPluginItem(FPlugins.Items[i]).Plugin <> nil) then
      TPSPluginItem(FPlugins.Items[i]).Plugin.ExecOnUses(Self);
  end;
  if not FExec.LoadData(Data) then
    raise Exception.Create({+}string(GetExecErrorString){+.});
end;

function TPSScript.SetVarToInstance(const VarName: TbtString; cl: TObject): Boolean;
var
  p: PIFVariant;
begin
  p := GetVariable(VarName);
  if p <> nil then
  begin
    SetVariantToClass(p, cl);
    result := true;
  end else result := false;
end;

procedure TPSScript.SetScript(const Value: TStrings);
begin
  FScript.Assign(Value);
end;

function TPSScript.AddMethod(Slf, Ptr: Pointer;
  const Decl: TbtString): Boolean;
begin
  Result := AddMethodEx(Slf, Ptr, Decl, cdRegister);
end;

function TPSScript.AddMethodEx(Slf, Ptr: Pointer; const Decl: TbtString;
  CallingConv: TDelphiCallingConvention): Boolean;
var
  P: TPSRegProc;
begin
  if not FCanAdd then begin Result := False; exit; end;
  p := Comp.AddDelphiFunction(Decl);
  if p <> nil then
  begin
    Exec.RegisterDelphiMethod(Slf, Ptr, p.Name, CallingConv);
    Result := True;
  end else Result := False;
end;

procedure TPSScript.OnLineEvent;
begin
  if @FOnLine <> nil then FOnLine(Self);
end;

function TPSScript.GetRunning: Boolean;
begin
  Result := FExec.Status = isRunning;
end;

function TPSScript.GetExecErrorCol: Cardinal;
var S: TbtString; D1: Cardinal;
begin
  {$IFDEF FPC}{$push} // FPC: https://wiki.freepascal.org/Turn_warnings_and_hints_on_or_off
    {$warn 5057 off}  // FPC: Hint: Local variable "$1" does not seem to be initialized
    {$warn 5060 off}  // FPC: Hint: Function result variable does not seem to be initialized
    {$warn 5091 off}  // FPC: Local variable "$1" of a managed type does not seem to be initialized
  {$ENDIF}
  if not TranslatePositionRC(Exec.ExceptionProcNo, Exec.ExceptionPos, D1, Result, S) then
    Result := 0;
end;{$IFDEF FPC}{$pop}{$ENDIF}

function TPSScript.TranslatePositionPos(Proc, Position: Cardinal;
  var Pos: Cardinal; var fn: TbtString): Boolean;
var D1, D2: Cardinal;
begin
  Result := Exec.TranslatePositionEx(Exec.ExceptionProcNo, Exec.ExceptionPos, Pos, {%H-}D1, {%H-}D2, fn);
end;

{+}
//function TPSScript.TranslatePositionPos(Proc, Position: Cardinal; var Pos: Cardinal): Boolean;
//var
//  D1, D2: Cardinal; fn: TbtString;
//begin
//  Result := Exec.TranslatePositionEx(Exec.ExceptionProcNo, Exec.ExceptionPos, Pos, D1, D2, fn);
//end;
{+.}

function TPSScript.TranslatePositionRC(Proc, Position: Cardinal;
  var Row, Col: Cardinal; var fn: TbtString): Boolean;
var D1: Cardinal;
begin
  Result := Exec.TranslatePositionEx(Proc, Position, {%H-}D1, Row, Col, fn);
end;

{+}
function TPSScript.TranslatePositionRC(Proc, Position: Cardinal; var Row, Col: Cardinal ): Boolean;
var D1: Cardinal; fn: TbtString;
begin
  Result := Exec.TranslatePositionEx(Proc, Position, {%H-}D1, Row, Col, {%H-}fn);
end;
{+.}

function TPSScript.GetExecErrorRow: Cardinal;
var D1: Cardinal; S: TbtString;
begin
  {$IFDEF FPC}{$push}
    {$warn 5057 off}  // FPC: Hint: Local variable "$1" does not seem to be initialized
    {$warn 5060 off}  // FPC: Hint: Function result variable does not seem to be initialized
    {$warn 5091 off}  // FPC: Local variable "$1" of a managed type does not seem to be initialized
  {$ENDIF}
  if not TranslatePositionRC(Exec.ExceptionProcNo, Exec.ExceptionPos, Result, D1, S) then
    Result := 0;
end;{$IFDEF FPC}{$pop}{$ENDIF}

procedure TPSScript.Stop;
begin
  if (FExec.Status = isRunning) or (Fexec.Status = isPaused) then
    FExec.Stop
  else
    raise Exception.Create(RPS_NotRunning);
end;

function TPSScript.GetProcMethod(const ProcName: TbtString): TMethod;
begin
  Result := FExec.GetProcAsMethodN(ProcName)
end;

procedure TPSScript.SetMainFileName(const Value: TbtString);
begin
  FMainFileName := Value;
end;

function TPSScript.GetExecErrorFileName: TbtString;
var
  D1, D2: Cardinal;
begin
  {$IFDEF FPC}{$push}
    {$warn 5057 off}  // FPC: Hint: Local variable "$1" does not seem to be initialized
    {$warn 5091 off}  // FPC: Hint: Local variable "$1" of a managed type does not seem to be initialized
    {$warn 5092 off}  // FPC: Hint: Variable "$1" of a managed type does not seem to be initialized
    {$warn 5093 off}  // FPC: Warning: Function result variable of a managed type does not seem to be initialized
  {$ENDIF}
  if not TranslatePositionRC(Exec.ExceptionProcNo, Exec.ExceptionPos, D1, D2, Result) then
    Result := '';
end;{$IFDEF FPC}{$pop}{$ENDIF}

procedure TPSScript.SetPointerToData(const VarName: TbtString;
  Data: Pointer; aType: TIFTypeRec);
var
  v: PIFVariant;
  t: TPSVariantIFC;
begin
  v := GetVariable(VarName);
  if (Atype = nil) or (v = nil) then
    raise Exception.Create(RPS_UnableToFindVariable);
  t.Dta := @PPSVariantData(v).Data;
  t.aType := v.FType;
  t.VarParam := false;
  VNSetPointerTo(t, Data, aType);
end;

function TPSScript.AddRegisteredPTRVariable(const VarName,
  VarType: TbtString): Boolean;
var
  FVar: TPSVar;
begin
  if not FCanAdd then begin Result := False; exit; end;
  FVar := FComp.AddUsedVariableN(varname, vartype);
  if fvar = nil then
    result := False
  else begin
    fvar.exportname := fvar.Name;
    fvar.SaveAsPointer := true;
    Result := True;
  end;
end;

procedure TPSScript.SetDefines(const Value: TStrings);
begin
  FDefines.Assign(Value);
end;

function TPSScript.ExecuteFunction(const Params: array of Variant;
  const ProcName: TbtString): Variant;
begin
  if SuppressLoadData then
    LoadExec;

  DoOnExecute (RI);

  FExec.DebugEnabled := FUseDebugInfo;

  Result := Exec.RunProcPN(Params, ProcName);

  DoAfterExecute;
end;

function TPSScript.FindBaseType(Bt: TPSBaseType): TPSTypeRec;
begin
  Result := Exec.FindType2(Bt);
end;

function TPSScript.FindNamedType(const Name: TbtString): TPSTypeRec;
begin
  Result := Exec.GetTypeNo(Exec.GetType(Name));
end;

procedure TPSScript.Notification(AComponent: TComponent;
  Operation: TOperation);
var
  i: Longint;
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (aComponent is TPSPlugin) then
  begin
    if Plugins <> nil then
    for i := Plugins.Count -1 downto 0 do
    begin
      if (Plugins.Items[i] as TPSPluginItem).Plugin = aComponent then
        {$IFDEF FPC_COL_NODELETE}
        TCollectionItem(Plugins.Items[i]).Free;
        {$ELSE}
        Plugins.Delete(i);
        {$ENDIF}
    end;
  end;
end;

function TPSScript.AddNotificationVariant(const VarName: TbtString): Boolean;
begin
  Result := AddRegisteredVariable(VarName, '!NOTIFICATIONVARIANT');
end;

procedure TPSScript.DoOnProcessDirective(Sender: TPSPreProcessor;
  Parser: TPSPascalPreProcessorParser; const Active: Boolean;
  const DirectiveName, DirectiveParam: TbtString; var Continue: Boolean);
begin
  If Assigned (OnProcessDirective) then
    OnProcessDirective (Sender, Parser, Active, DirectiveName, DirectiveParam, Continue);
end;

procedure TPSScript.DoOnProcessUnknowDirective(Sender: TPSPreProcessor;
  Parser: TPSPascalPreProcessorParser; const Active: Boolean;
  const DirectiveName, DirectiveParam: TbtString; var Continue: Boolean);
begin
  If Assigned (OnProcessUnknowDirective) then
    OnProcessUnknowDirective (Sender, Parser, Active, DirectiveName, DirectiveParam, Continue);
end;

function TPSScript.DoOnNeedFile(Sender: TObject;
  const OrginFileName: TbtString; var FileName, Output: TbtString): Boolean;
begin
  If Assigned (OnNeedFile) then
    Result := OnNeedFile(Sender, OrginFileName, FileName, Output)
  else
    Result := False;
end;

function TPSScript.DoOnUnknowUses(Sender: TPSPascalCompiler;
  const Name: TbtString): Boolean;
var
  lPrevAllowUnit: Boolean;
  lData, lName: TbtString;
begin
  if Assigned(FOnFindUnknownFile) then begin
    lName := Name;
    if FOnFindUnknownFile(self, '', lName, {%H-}lData) then begin
      lPrevAllowUnit := FComp.AllowUnit;
      FComp.AllowUnit := true;
      if FUsePreProcessor then
      begin
        FPP.Defines.Assign(FDefines);
        Fpp.MainFile := lData;
        Fpp.MainFileName := lName;
        Fpp.PreProcess(lName, lData);
        Result := FComp.Compile(lData);
        Fpp.AdjustMessages(FComp);
      end else
      begin
        FComp.OnTranslateLineInfo := nil;
        Result := FComp.Compile(lData);
      end;
      FComp.AllowUnit := lPrevAllowUnit;
    end else begin
      FComp.MakeError(FComp.UnitName, ecUnknownIdentifier, lName);
      Result := false;
    end;
  end else begin
    FComp.MakeError(FComp.UnitName, ecUnknownIdentifier, {%H-}lName);
    Result := False;
  end;
end;

procedure TPSScript.DoOnCompImport;
begin
  if Assigned(OnCompImport) then
    OnCompImport(Self, Comp);
end;

procedure TPSScript.DoOnCompile;
begin
  if Assigned(OnCompile) then
    OnCompile(Self);
end;

procedure TPSScript.DoOnExecute;
begin
  if Assigned(OnExecute) then
    OnExecute(Self);
end;

procedure TPSScript.DoAfterExecute;
begin
  if Assigned(OnAfterExecute) then
    OnAfterExecute(Self);
end;

function TPSScript.DoVerifyProc(Sender: TPSScript;
  Proc: TPSInternalProcedure; const Decl: TbtString): Boolean;
begin
  if Assigned(OnVerifyProc) then begin
    Result := false;
    OnVerifyProc(Sender, Proc, Decl, Result);
    Result := not Result;
  end
  else
    Result := True;
end;

procedure TPSScript.DoOnExecImport(
  RunTimeImporter: TPSRuntimeClassImporter);
begin
  if Assigned(OnExecImport) then
    OnExecImport(Self, FExec, RunTimeImporter);
end;

function TPSScript.DoOnGetNotificationVariant(const Name: TbtString): Variant;
begin
  if not Assigned(OnGetNotificationVariant) then
    raise Exception.Create(RPS_UnableToReadVariant);
  Result := OnGetNotificationVariant(Self, Name);
end;

procedure TPSScript.DoOnSetNotificationVariant(const Name: TbtString;
  V: Variant);
begin
  if not Assigned(OnSetNotificationVariant) then
    raise Exception.Create(RPS_UnableToWriteVariant);
  OnSetNotificationVariant(Self, Name, v);
end;

{ TPSDllPlugin }

procedure TPSDllPlugin.CompOnUses;
begin
  CompExec.Comp.OnExternalProc := nil;
  {$IF DEFINED (MSWINDOWS) OR Defined (UNIX) OR Defined (fpc)}
  CompExec.Comp.OnExternalProc := DllExternalProc;
  {$IFEND}
end;

procedure TPSDllPlugin.ExecOnUses;
begin
  {$IF DEFINED (MSWINDOWS) OR Defined (UNIX) OR Defined (fpc)}
  RegisterDLLRuntime(CompExec.Exec);
  {$IFEND}
end;

{ TPS3DebugCompExec }

procedure LineInfo(Sender: TPSDebugExec; const FileName: TbtString; Position, Row, Col: Cardinal);
var
  Dc: TPSScriptDebugger;
  h, i: Longint;
  bi: TPSBreakPointInfo;
  lFileName: TbtString;
begin
  Dc := Sender.Id;
  if FileName = '' then
    lFileName := dc.MainFileName
  else
    lFileName := FileName;

  if @dc.FOnLineInfo <> nil then dc.FOnLineInfo(dc, lFileName, Position, Row, Col);
  if row = dc.FLastRow then exit;
  dc.FLastRow := row;
  h := MakeHash(lFileName);
  bi := nil;
  for i := DC.FBreakPoints.Count -1 downto 0 do
  begin
    bi := Dc.FBreakpoints[i];
    if (h = bi.FileNameHash) and (lFileName = bi.FileName) and (Cardinal(bi.Line) = Row) then
    begin
      Break;
    end;
    Bi := nil;
  end;
  if bi <> nil then
  begin
    if @dc.FOnBreakpoint <> nil then dc.FOnBreakpoint(dc, lFileName, Position, Row, Col);
    dc.Pause;
  end;
end;

procedure IdleCall(Sender: TPSDebugExec);
var
  Dc: TPSScriptDebugger;
begin
  Dc := Sender.Id;
  if @dc.FOnIdle <> nil then
    dc.FOnIdle(DC)
  else
    dc.Exec.Run;
end;

procedure TPSScriptDebugger.ClearBreakPoint(const Fn: TbtString; Line: Integer);
var
  h, i: Longint;
  bi: TPSBreakPointInfo;
begin
  h := MakeHash(Fn);
  for i := FBreakPoints.Count -1 downto 0 do
  begin
    bi := FBreakpoints[i];
    if (h = bi.FileNameHash) and (Fn = bi.FileName) and (bi.Line = Line) then
    begin
      FBreakPoints.Delete(i);
      bi.Free;
      Break;
    end;
  end;
end;

procedure TPSScriptDebugger.ClearBreakPoints;
var
  i: Longint;
begin
  for i := FBreakPoints.Count -1 downto 0 do
    TPSBreakPointInfo(FBreakPoints[i]).Free;
  FBreakPoints.Clear;;
end;

constructor TPSScriptDebugger.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBreakPoints := TIFList.Create;
  FExec.OnSourceLine := LineInfo;
  FExec.OnIdleCall := IdleCall;
end;

destructor TPSScriptDebugger.Destroy;
var
  i: Longint;
begin
  if Assigned(FBreakPoints) then begin
    for i := FBreakPoints.Count-1 downto 0 do begin
      TPSBreakPointInfo(FBreakPoints[i]).Free;
      FBreakPoints[i] := nil;
    end;
    FreeAndNil(FBreakPoints);
  end;
  inherited;
end;

function TPSScriptDebugger.GetBreakPoint(I: Integer): TPSBreakPointInfo;
begin
  Result := FBreakPoints[i];
end;

function TPSScriptDebugger.GetBreakPointCount: Longint;
begin
  Result := FBreakPoints.Count;
end;

function TPSScriptDebugger.GetVarContents(const Name: TbtString): TbtString;
var
  i: Longint;
  pv: PIFVariant;
  s1, s: {+}string; //TbtString;{+.}
begin
  s := Uppercase({+}string(Name){+.});
  if pos('.', s) > 0 then
  begin
    s1 := copy(s,1,pos('.', s) -1);
    delete(s,1,pos('.', {+}string(Name){+.}));
  end else begin
    s1 := s;
    s := '';
  end;
  pv := nil;
  for i := 0 to Exec.CurrentProcVars.Count -1 do
  begin
    if Uppercase({+}string(Exec.CurrentProcVars[i]){+.}) =  s1 then
    begin
      pv := Exec.GetProcVar(i);
      break;
    end;
  end;
  if pv = nil then
  begin
    for i := 0 to Exec.CurrentProcParams.Count -1 do
    begin
      if Uppercase({+}string(Exec.CurrentProcParams[i]){+.}) =  s1 then
      begin
        pv := Exec.GetProcParam(i);
        break;
      end;
    end;
  end;
  if pv = nil then
  begin
    for i := 0 to Exec.GlobalVarNames.Count -1 do
    begin
      if Uppercase({+}string(Exec.GlobalVarNames[i]){+.}) =  s1 then
      begin
        pv := Exec.GetGlobalVar(i);
        break;
      end;
    end;
  end;
  if pv = nil then
    Result := {+}TbtString(RPS_UnknownIdentifier){+.}
  else
    Result := PSVariantToString(NewTPSVariantIFC(pv, False), {+}TbtString(s){+.});
end;

function TPSScriptDebugger.GetVarValue(const Name: TbtString): Pointer;
var
  i: LongInt;
  pv: PIFVariant;
  s1, s: TbtString;
begin
  s := UpperCase(Name);
  if Pos({+.}tbtChar('.'){+.}, s) > 0 then
  begin
    s1 := Copy(s,1,Pos({+.}tbtChar('.'){+.}, s) -1);
    Delete(s,1,Pos({+.}tbtchar('.'){+.}, Name));
  end else begin
    s1 := s;
    s := '';
  end;
  pv := nil;
  for i := 0 to Exec.CurrentProcVars.Count -1 do
  begin
    if UpperCase(Exec.CurrentProcVars[i]) =  s1 then
    begin
      pv := Exec.GetProcVar(i);
      break;
    end;
  end;
  if pv = nil then
  begin
    for i := 0 to Exec.CurrentProcParams.Count -1 do
    begin
      if UpperCase(Exec.CurrentProcParams[i]) =  s1 then
      begin
        pv := Exec.GetProcParam(i);
        break;
      end;
    end;
  end;
  if pv = nil then
  begin
    for i := 0 to Exec.GlobalVarNames.Count -1 do
    begin
      if UpperCase(Exec.GlobalVarNames[i]) =  s1 then
      begin
        pv := Exec.GetGlobalVar(i);
        break;
      end;
    end;
  end;
  if pv = nil then
    Result := nil
  else
    Result := NewTPSVariantIFC(pv, False).Dta;
end;

function TPSScriptDebugger.HasBreakPoint(const Fn: TbtString; Line: Integer): Boolean;
var
  h, i: Longint;
  bi: TPSBreakPointInfo;
begin
  h := MakeHash(Fn);
  for i := FBreakPoints.Count -1 downto 0 do
  begin
    bi := FBreakpoints[i];
    if (h = bi.FileNameHash) and (Fn = bi.FileName) and (bi.Line = Line) then
    begin
      Result := true;
      exit;
    end;
  end;
  Result := False;
end;

procedure TPSScriptDebugger.Pause;
begin
  if FExec.Status = isRunning then
    FExec.Pause
  else
    raise Exception.Create(RPS_NotRunning);
end;

procedure TPSScriptDebugger.Resume;
begin
  if FExec.Status = isRunning then
    FExec.Run
  else
    raise Exception.Create(RPS_NotRunning);
end;

procedure TPSScriptDebugger.SetBreakPoint(const fn: TbtString; Line: Integer);
var
  i, h: Longint;
  BI: TPSBreakPointInfo;
begin
  h := MakeHash(fn);
  for i := FBreakPoints.Count -1 downto 0 do
  begin
    bi := FBreakpoints[i];
    if (h = bi.FileNameHash) and (fn = bi.FileName) and (bi.Line = Line) then
      exit;
  end;
  bi := TPSBreakPointInfo.Create;
  FBreakPoints.Add(bi);
  bi.FileName := fn;
  bi.Line := Line;
end;

procedure TPSScriptDebugger.SetMainFileName(const Value: TbtString);
var
  OldFn: TbtString;
  h1, h2,i: Longint;
  bi: TPSBreakPointInfo;
begin
  OldFn := FMainFileName;
  inherited SetMainFileName(Value);
  h1 := MakeHash(OldFn);
  h2 := MakeHash(Value);
  if OldFn <> Value then
  begin
    for i := FBreakPoints.Count -1 downto 0 do
    begin
      bi := FBreakPoints[i];
      if (bi.FileNameHash = h1) and (bi.FileName = OldFn) then
      begin
        bi.FFileNameHash := h2;
        bi.FFileName := Value;
      end else if (bi.FileNameHash = h2) and (bi.FileName = Value) then
      begin
        // It's already the new filename, that can't be right, so remove all the breakpoints there
        FBreakPoints.Delete(i);
        bi.Free;
      end;
    end;
  end;
end;

procedure TPSScriptDebugger.StepInto;
begin
  if (FExec.Status = isRunning) or (FExec.Status = isLoaded) then
    FExec.StepInto
  else
    raise Exception.Create(RPS_NoScript);
end;

procedure TPSScriptDebugger.StepOver;
begin
  if (FExec.Status = isRunning) or (FExec.Status = isLoaded) then
    FExec.StepOver
  else
    raise Exception.Create(RPS_NoScript);
end;

{ TPSPluginItem }

procedure TPSPluginItem.Assign(Source: TPersistent); //Birb
begin
  if Source is TPSPluginItem then
   plugin:=((source as TPSPluginItem).plugin)
  else
   inherited;
end;

function TPSPluginItem.GetDisplayName: string;
begin
  if FPlugin <> nil then
    Result := string(FPlugin.Name)
  else
    Result := '<nil>';
end;

procedure TPSPluginItem.SetPlugin(const Value: TPSPlugin);
begin
  FPlugin := Value;
  If Value <> nil then
    Value.FreeNotification(TPSPlugins(Collection).FCompExec);
  Changed(False);
end;

{ TPSPlugins }

constructor TPSPlugins.Create(CE: TPSScript);
begin
  inherited Create(TPSPluginItem);
  FCompExec := CE;
end;

function TPSPlugins.GetOwner: TPersistent;
begin
  Result := FCompExec;
end;

{ TPSBreakPointInfo }

procedure TPSBreakPointInfo.SetFileName(const Value: TbtString);
begin
  FFileName := Value;
  FFileNameHash := MakeHash(Value);
end;

{ TPSCustomPlugin }
procedure TPSCustomPlugin.CompileImport1(CompExec: TPSScript);
begin
  IF @FOnCompileImport1 <> nil then
    FOnCompileImport1(CompExec)
  else
    inherited;
end;

procedure TPSCustomPlugin.CompileImport2(CompExec: TPSScript);
begin
  IF @FOnCompileImport2 <> nil then
    FOnCompileImport2(CompExec)
  else
    inherited;
end;

procedure TPSCustomPlugin.CompOnUses(CompExec: TPSScript);
begin
  IF @FOnCompOnUses <> nil then
    FOnCompOnUses(CompExec)
  else
    inherited;
end;

procedure TPSCustomPlugin.ExecImport1(CompExec: TPSScript;
  const ri: TPSRuntimeClassImporter);
begin
  IF @FOnExecImport1 <> nil then
    FOnExecImport1(CompExec, compExec.Exec, ri)
  else
    inherited;
end;

procedure TPSCustomPlugin.ExecImport2(CompExec: TPSScript;
  const ri: TPSRuntimeClassImporter);
begin
  IF @FOnExecImport2 <> nil then
    FOnExecImport2(CompExec, compExec.Exec, ri)
  else
    inherited;
end;

procedure TPSCustomPlugin.ExecOnUses(CompExec: TPSScript);
begin
  IF @FOnExecOnUses <> nil then
    FOnExecOnUses(CompExec)
  else
    inherited;
end;

end.
