{ uPSComponentExt.pas } // version: 2020.1010.1010
{----------------------------------------------------------------------------}
{ RemObjects Pascal Script                                                   }
{----------------------------------------------------------------------------}
//
// @abstract(Component wrapper for IFPS3 compiler and executer)
// A component wrapper for IFPS3, including debugging support.
//
{----------------------------------------------------------------------------}
unit uPSComponentExt;
{$I PascalScript.inc}

interface

uses
  {$IFDEF DELPHI7UP}Types,{$ENDIF}
  //{$IFDEF DELPHI17UP}System.UITypes,{$ENDIF}
  {$IFNDEF LINUX} Windows, {$ENDIF} SysUtils, Classes, Contnrs, TypInfo
  ,uPSUtils, uPSRuntime, uPSCompiler, uPSComponent, uPSC_dll
  ,{%H-}uPSR_dll ,{%H-}uPSPreProcessor ,{%H-}uPSDebugger
  ;

const
  {alias to @link(ifps3.cdRegister)}
  cdRegister = uPSRuntime.cdRegister;
  {alias to @link(ifps3.cdPascal)}
  cdPascal = uPSRuntime.cdPascal;
  { alias to ifps3.cdCdecl }
  CdCdecl = uPSRuntime.CdCdecl;
  {alias to @link(ifps3.cdStdcall)}
  CdStdCall = uPSRuntime.CdStdCall;

type
  {Alias to @link(ifps3.TPSCallingConvention)}
  TDelphiCallingConvention = uPSRuntime.TPSCallingConvention;
  {Alias to @link(ifps3.TPSRuntimeClassImporter)}
  TPSRuntimeClassImporter = uPSRuntime.TPSRuntimeClassImporter;

  TPSScriptExtension = class;

  {Base class for all plugins for the component}
  TPSOnCompCleanup = Function (Sender: TObject; aComp: TPSPascalCompiler):Boolean of object;
  TPSOnInsertProcedure = Procedure (Sender: TObject; aProc: TbtString; OnTop: Boolean) of object;
  TPSOnException = procedure (Sender: TPSExec; ExError: TPSError; const ExParam: TbtString;
                              ExObject: TObject; ProcNo, Position: Cardinal) of object;
  TMethodList = class;
  TProcObj = Class
  private
    FName  : TbtString;
    fOwner : TMethodList;
    procedure SetName(const Value: TbtString);
  public
    ProcType : TStringList;
    Method : TMethod;
    constructor Create(aOwner: TMethodList);
    destructor Destroy; override;
    property Name: TbtString read FName write SetName;
  end;

  TMethodObj = Class
    Instance  : TPersistent;
    PropName  : TbtString;
    ProcName  : TbtString;
  end;

  TMethodList = class
  private
    fOwner     : TPSScriptExtension;
    fProcList  : TObjectList;
    fEventList : TObjectList;
    function  GetObject(Index: Integer): TMethodObj; virtual;
    function  GetProcObj(Index: Integer): TProcObj;
    function  GetMethodName(Instance: TObject; PropName: TbtString): TbtString;
    procedure SetMethodName(Instance: TObject; PropName: TbtString; const Value: TbtString);
    procedure CreateProc(ProcName: TbtString; aPropType: TTypeData);
  public
    constructor Create(aOwner: TPSScriptExtension);
    destructor Destroy; override;

    function MethodIndexOf(Instance: TObject; PropName: TbtString):Integer;
    function ProcIndexOf(Name: TbtString): Integer;
    procedure ListEventsName(EventType:TbtString; List : TStrings);

    procedure AddProcedure(ProcName, ProcType:TbtString);
    procedure InsertMethod(NewProc: TbtString; OnTop: Boolean = False);

    procedure FillMethods;
    procedure ClearProcList;
    procedure ClearAll;
    function ProcCount :Integer;
    function MethodCount :Integer;

    property Procs[Index: Integer]: TProcObj read GetProcObj ;
    property Methods[Index: Integer]: TMethodObj read GetObject;
    property ProcName[Instance: TObject; PropName:TbtString]: TbtString read GetMethodName write SetMethodName;
  end;

  TPSScriptExtension = class(TPSScriptDebugger)
  private
    FOnBeforeCleanUp: TPSOnCompCleanup;
    FMethodList : TMethodList;
    FOnInsertMethod: TPSOnInsertProcedure;
    FNeedCompiling :Boolean;
    FOnScriptChance: TNotifyEvent;
    FOnException: TPSOnException;

    fItems, fInserts: TStrings;
    fScriptPos : Cardinal;
    fObjectNest: TbtString;

    procedure GetCodeProps ;
    function GetProcName(Instance: TObject; PropName: TbtString): TbtString;
    procedure SetProcName(Instance: TObject; PropName: TbtString; const Value: TbtString);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    procedure DoVerifyProc(Sender: TPSScript; Proc: TPSInternalProcedure;
                           const Decl: TbtString; var Error: Boolean); reintroduce;
    function  DoBeforeCleanup(Sender: TObject; aComp: TPSPascalCompiler):Boolean;
    procedure DoScriptChance(sender:TObject);
  public
    {Create an instance of the CompExec component}
    constructor Create(AOwner: TComponent); override;
    {Destroy the CompExec component}
    destructor Destroy; override;

    function Compile: Boolean; Override;
    function Execute: Boolean; Override;
    { Create a list of all var's, const's, Type's and functions }
    procedure GetValueDefs(aItems, aInserts: TStrings; Const aObjectNest: TbtString=''; aScriptPos: Integer = 0);

    {Compile the source only when the source is modified}
    procedure CompileIfNeeded;

    {Is the source modified}
    property NeedCompiling : Boolean read FNeedCompiling;

    {Fills all function in the script to there connected Events.
     This is called automatic after a succesfull Compilition}
    procedure FillMethods;

    {Removes all events from the Objects Fills all function in the script to there connected Events.
     This function is automatic called before a Compilition}
    procedure ClearProcList;
    procedure RemoveObjEvents(Obj: TObject);

    {This property helps you set the events that must becalled from within the script
     Instance is the object where the Propname must be set.
     You need te create the function yopur self in the script.
     When the new Procname dose not exists in the script, it is automatic created for you.}
    property ProcName[Instance: TObject; PropName:TbtString]: TbtString read GetProcName write SetProcName;
    property MethodList : TMethodList read FMethodList;
  published
    property OnBeforeCleanUp: TPSOnCompCleanup read FOnBeforeCleanUp write FOnBeforeCleanUp; //<NVDS>
    property OnInsertMethod : TPSOnInsertProcedure read FOnInsertMethod write FOnInsertMethod;
    Property OnScriptChance : TNotifyEvent read FOnScriptChance write fOnScriptChance;
    property OnException    : TPSOnException read FOnException write FOnException;
  end;

implementation
{+}
{$IFDEF DELPHI12UP}
uses
  AnsiStrings;
{$ENDIF}

type
  EMethodList = class(EPSError);
{+.}

{$if (defined(DELPHI3UP) or defined(FPC))}
resourcestring
{$else}
const
{$ifend}
  sMissingEndStatment = 'Missing some ''End'' statments';

function CompExportCheck(Sender: TPSPascalCompiler; Proc: TPSInternalProcedure; const ProcDecl: TbtString): Boolean;
begin
{$IFDEF FPC}{$push}
  {$warn 5060 off}  // FPC: Hint: Function result variable does not seem to be initialized
  {$warn 5057 off}  // FPC: Hint: Local variable "$1" does not seem to be initialized
{$ENDIF}
  TPSScriptExtension(Sender.ID).DoVerifyProc(Sender.Id, Proc, ProcDecl, Result);
  Result := not Result;
end; {$IFDEF FPC}{$pop}{$ENDIF}

Function BeforeCleanup(Sender: TPSPascalCompiler):Boolean;
begin
  Result := TPSScriptExtension(Sender.ID).DoBeforeCleanUp(Sender.ID,Sender);
end;

procedure CEException(Sender: TPSExec; ExError: TIFError; const ExParam: TbtString; ExObject: TObject; ProcNo, Position: Cardinal);
begin
  if @TPSScriptExtension(Sender.ID).FOnException <> nil then
    TPSScriptExtension(Sender.ID).FOnException(Sender, ExError, ExParam, ExObject, ProcNo, Position);
end;

{ TPSScriptExtension }

function TPSScriptExtension.Compile: Boolean;
begin
  ClearProcList;

  Result := inherited Compile;
  if result then FillMethods;

 FNeedCompiling := not result;
end;

constructor TPSScriptExtension.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Comp.OnBeforeCleanup := BeforeCleanup;
  Comp.OnExportCheck   := CompExportCheck;
  Exec.OnException     := CEException;

  TStringList(script).OnChange := DoScriptChance;
  FMethodList := TMethodList.create(Self);
  FNeedCompiling := True;
end;

destructor TPSScriptExtension.Destroy;
begin
  FreeAndNil(FMethodList);
  inherited;
end;

procedure TPSScriptExtension.DoVerifyProc(Sender: TPSScript;
  Proc: TPSInternalProcedure; const Decl: TbtString; var Error: Boolean);
var
  n{,m,p} : Integer;
  tstType : TPSProceduralType;
begin
  Error := False;
  for n := 0 to sender.comp.GetTypeCount -1 do begin
    if comp.GetType(n) is TPSProceduralType then begin
      tstType := comp.GetType(n) as TPSProceduralType;
      if tstType.ProcDef.Same(Proc.Decl) then begin
        MethodList.addprocedure(Proc.OriginalName, tstType.Name);
      //Proc. aExport := etExportDecl;
      end;
    end;
  end;
  if Assigned(OnVerifyProc) then
  begin
    onVerifyProc(Sender, Proc, Decl, Error);
  end;
end;

type
  TMyPascalCompiler = class(TPSPascalCompiler);
const
  sIFPSParameterMode : array [pmIn..pmInOut] of TbtString = ('','\style{+B}out\style{-B} ','\style{+B}Var\style{-B} ');

procedure TPSScriptExtension.GetCodeProps;

  Function existsItem(aName:TbtString):Boolean;
  Begin
    Result := FInserts.indexof({+}string(aName){+.})<> -1;
  end;

  Procedure addListItem(aType, aName:TbtString; aDef:TbtString='');
  var
    x : LongInt;
  begin
    if not ((aName ='') or (aName[1]='!')) then begin
      x := FInserts.Add({+}string(aName){+.});
      fItems.Insert(x, format('%s \column{}\style{+B}%s\style{-B} %s',[aType, aName, aDef]));
    end;
  end;

  procedure GetDecl(decl : TPSParametersDecl; var T, v: TbtString);
  var
    m : Integer;
  begin
    v := '';
    for m := 0 to Decl.ParamCount-1 do begin
      v := V +';'+sIFPSParameterMode[Decl.Params[m].Mode]+
              Decl.Params[m].OrgName;
      if Decl.Params[m].aType <> nil then
        v := v +':'+ Decl.Params[m].aType.OriginalName;
    end;
    delete(v,1,1);
    if v <> '' then v := '('+ v +')';
    if Decl.Result<>nil then begin
      v := v +':'+ Decl.Result.OriginalName;
      t := 'Function';
    end else t := 'Procedure';
  end;

  function GetTypeDef(xr: TPSType; aZoek:TbtString = ''):Boolean; forward;

  function GetClassDef(xc: TPSCompileTimeClass; aZoek: TbtString = ''): Boolean;
  var
    Show : Boolean;
    Zoek,bZoek : TbtString;
    tci : TPSDelphiClassItem;
    n : Integer;
    T, V : TbtString;
  begin
    Show := aZoek='';
    Zoek := aZoek;
    if Pos({+}TbtString('.'){+.},aZoek)>0 then begin
      Zoek  := copy(aZoek, 1 ,Pos({+}TbtString('.'){+.},aZoek)-1);
      bZoek := copy(aZoek, Pos({+}TbtString('.'){+.},aZoek)+1, 999);
    end else bZoek := '';

    Result := (xc <> nil) and Show;
    if XC<> nil then begin
      for n := 0 to xc.Count-1 do begin
        tci := xc.Items[n];
        if (tci = nil) or  existsItem(tci.OrgName) then continue;
        if tci is TPSDelphiClassItemConstructor then begin
          GetDecl(tci.decl, {%H-}T, {%H-}V);
          if Show then addListItem('constructor',tci.OrgName, V);
        end else
        if tci is TPSDelphiClassItemMethod then begin
          if Show then begin
            GetDecl(tci.decl, T, V);
            addListItem(T,tci.OrgName, V)
          end else
          if (tci.decl.Result <> nil) and (tci.Name = Zoek) then
            Result := GetTypeDef(tci.decl.Result, bZoek);
        end else
        if tci is TPSDelphiClassItemProperty then begin
          if Show then begin
            t := '';
            if tci.Decl.Result<> nil then t := ': '+ tci.Decl.Result.OriginalName;
            addListItem('Property',tci.OrgName, t);
          end else
          if (tci.decl.Result <> nil) and (tci.Name = Zoek) then
            Result := GetTypeDef(tci.decl.Result, bZoek);
        end;
        if Result and not show then
          Exit;
      end;
      if not Result then
        Result := GetClassDef(XC.ClassInheritsFrom, aZoek);
    end;
  end;

  function GetTypeDef(xr: TPSType; aZoek:TbtString = ''):Boolean;
  var
    Show : Boolean;
    Zoek : TbtString;
    xri : PIFPSRecordFieldTypeDef;
    n : Integer;
  begin
    Show := (aZoek = '');
    Result := (xr <> nil) and Show;
    if xr <> nil then begin
      if xr is TPSRecordType then begin
        Zoek := aZoek;
        if Pos({+}TbtString('.'){+.},aZoek)>0 then begin
          Zoek  := copy(aZoek, 1 ,Pos({+}TbtString('.'){+.},aZoek)-1);
          aZoek := copy(aZoek, Pos({+}TbtString('.'){+.},aZoek)+1, 999);
        end else aZoek := '';
        for n := 0 to (xr as TPSRecordType).RecValCount-1 do begin
          xri := (xr as TPSRecordType).RecVal(n);
          if Show then begin
            addListItem('var',xri.FieldOrgName,xri.aType.OriginalName)
          end else
          if (xri.aType <> nil) and (xri.FieldName = Zoek) then
            Result := GetTypeDef(xri.aType, aZoek);
        end;
      end else
      if (xr is TPSClassType) then begin
        Result := GetClassDef((xr as TPSClassType).Cl, aZoek)
      end else
        Result := False;
    end;
  end;

  function FindVarProc(aVarName:TbtString; aZoek : TbtString= ''):Boolean;
  var
  //cv   : TbtString;
    hh, h, i : Longint;
    proc : TPSProcedure;
    ip   : TPSInternalProcedure;
    ipv  : PIFPSProcVar;
    ipp  : TPSParameterDecl;
  //t    : TbtString;
  begin
    Hh := MakeHash(aVarName);
    Result := False;
    if FScriptPos =0 then
      Exit;
    for i := Comp.GetProcCount -1 downto 0 do begin
      Proc := Comp.GetProc(i);
      if (Proc.ClassType = TPSInternalProcedure) and
         ((Proc as TPSInternalProcedure).DeclarePos < FScriptPos) then begin
        ip := Proc as TPSInternalProcedure;
        for h := 0 to ip.ProcVars.Count-1 do begin
          ipv := PIFPSProcVar(ip.ProcVars[h]);
          if aVarName = '' then begin
            addListItem('var',ipv.OrgName, ': '+ipv.AType.OriginalName);
          end else
          if (ipv.NameHash = HH) and (ipv.Name = aVarName) then begin
            Result := GetTypeDef(ipv.aType, aZoek);
            Exit;
          end;
        end;
        for h := 0 to ip.Decl.ParamCount-1 do begin
          ipp := TPSParameterDecl(ip.Decl.Params[h]);
          if aVarName = '' then begin
            addListItem('var',ipp.OrgName, ': '+ipp.aType.OriginalName);
          end else
          if {(ipp.Hash = HH) and} (ipp.Name = aVarName) then begin
            Result := GetTypeDef(ipp.aType, aZoek);
            Exit;
          end;
        end;
      end;
    end;
  end;

  function FindVarFunctType(aProcName:TbtString): Boolean;
  var
    cv : TbtString;
    h, i : Longint;
    proc : TPSProcedure;
    xr : TPSRegProc;
//    t  : TbtString;
  begin
    cv := aProcName;
    if Pos({+}TbtString('.'){+.},aProcName)>0 then begin
      cv := copy(aProcName, 1 ,Pos({+}TbtString('.'){+.},aProcName)-1);
      aProcName := copy(aProcName, Pos({+}TbtString('.'){+.},aProcName)+1, 999);
    end else aProcName := '';
    H := MakeHash(Cv);
//    Result := False;
    for i :=0 to Comp.GetVarCount -1 do begin
      if (Comp.GetVar(I).NameHash = H) and (Comp.GetVar(I).Name = CV) then begin
        Result := GetTypeDef(Comp.GetVar(I).aType, aProcName);
        Exit;
      end;
    end;
   for i :=0 to Comp.GetTypeCount -1 do begin
    if (Comp.GetType(I).NameHash = H) and (Comp.GetType(I).Name = CV) then begin
      Result := GetTypeDef(Comp.GetType(I), aProcName);
      Exit;
    end;
   end;
    Result := FindVarProc(cv, aProcName);
    if result then Exit;
    for i :=0 to Comp.GetProcCount -1 do begin
      Proc := Comp.GetProc(i);
      if Proc.ClassType = TPSInternalProcedure then begin
        if ((Proc as TPSInternalProcedure).NameHash = H) and
            ((Proc as TPSInternalProcedure).Name = CV) then begin
          Result := GetTypeDef((Proc as TPSInternalProcedure).Decl.Result, aProcName);
          Exit;
        end;
      end;
    end;
    with TMyPascalCompiler(Comp) do begin
      for i := 0 to FRegProcs.Count-1 do begin
        xr := FRegProcs[i];
        if (xr.NameHash = H) and  (xr.Name = CV) then begin
          Result := GetTypeDef(xr.Decl.Result, aProcName);
          Exit;
        end;
      end;
    end;
  end;

var
  n : Integer;
  s, t, v : TbtString;
  proc : TPSProcedure;
  xr : TPSRegProc;
begin // procedure TPSScriptExtension.GetCodeProps;
  if (fItems = nil) or (fInserts = Nil) then Exit;
  fItems.BeginUpdate;
  fInserts.BeginUpdate;
  tStringList(fInserts).Sorted := true;
  tStringList(fInserts).Duplicates := dupAccept;
  try
    fInserts.Clear;
    fItems.Clear;

    if (FObjectNest <> '') then begin
      FindVarFunctType(FastUpperCase(FObjectNest));
      Exit;
    end;

    for n := 0 to Comp.GetTypeCount-1 do begin
      addListItem('Type',Comp.GetType(n).OriginalName);
    end;
    for n := 0 to Comp.GetVarCount-1 do begin
      addListItem('var',Comp.GetVar(n).OrgName, ': '+Comp.Getvar(n).aType.OriginalName);
    end;
    with TMyPascalCompiler(Comp) do begin
      for n := 0 to FConstants.Count-1 do begin
        addListItem('const', TPSConstant(FConstants[n]).OrgName );
      end;
      for n := 0 to FRegProcs.Count-1 do begin
        xr := FRegProcs[n];
        GetDecl(xr.decl, {%H-}T, {%H-}v);
        addListItem(t,xr.OrgName, v );
      end;
    end;
    FindVarProc('');
    for n := 0 to Comp.GetProcCount-1 do begin
      s := '';
      proc := Comp.GetProc(n);
      if Proc.ClassType = TPSInternalProcedure then begin
        s := (Proc as TPSInternalProcedure).OriginalName;
        GetDecl((Proc as TPSInternalProcedure).decl, T, v);
      end;
      if s <> '' then begin
        addListItem(t,s, v );
      end;
    end;
  finally
    fInserts.EndUpdate;
    fItems.EndUpdate;
  end;
end; // procedure TPSScriptExtension.GetCodeProps

procedure TPSScriptExtension.GetValueDefs(aItems, aInserts: TStrings; const aObjectNest: TbtString; aScriptPos: Integer);
begin
  fItems      := aItems;
  fInserts    := aInserts;
  FScriptPos  := aScriptPos;
  fObjectNest := aObjectNest;
  try
    compile;
  finally
    fItems      := Nil;
    fInserts    := Nil;
    FScriptPos  := 0;
    fObjectNest := '';
  end;
end;

function TPSScriptExtension.DoBeforeCleanup(Sender: TObject; aComp: TPSPascalCompiler): Boolean;
begin
  Result := true;
  if fItems <> nil then GetCodeProps;
  if @FOnBeforeCleanUp<> nil then
    Result := FOnBeforeCleanUp(Sender, aComp);
end;

function TPSScriptExtension.Execute: Boolean;
begin
  CompileIfNeeded;
  MethodList.FillMethods;
  Result := inherited Execute;
end;

procedure TPSScriptExtension.DoScriptChance(sender: TObject);
begin
  FNeedCompiling := True;
  self.ClearProcList;
  if @FOnScriptChance <> NIL then
    FOnScriptChance(sender);
end;

procedure TPSScriptExtension.CompileIfNeeded;
begin
  if FNeedCompiling then begin
    Compile;
  end;
end;

procedure TPSScriptExtension.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then begin
    if MethodList <> nil then
      MethodList.SetMethodName(aComponent,'','');
  end;
end;

function TPSScriptExtension.GetProcName(Instance: TObject; PropName: TbtString): TbtString;
begin
  Result := MethodList.ProcName[Instance, Propname];
end;

procedure TPSScriptExtension.SetProcName(Instance: TObject; PropName: TbtString; const Value: TbtString);
begin
  MethodList.ProcName[Instance, Propname] := Value;
end;

procedure TPSScriptExtension.ClearProcList;
begin
  MethodList.ClearProcList;
end;

procedure TPSScriptExtension.RemoveObjEvents(Obj: TObject);
begin
  MethodList.SetMethodName(Obj, '', '');
end;

procedure TPSScriptExtension.FillMethods;
begin
  MethodList.FillMethods;
end;

{ TMethodList }

procedure TMethodList.AddProcedure(ProcName, ProcType: TbtString);
var
  po : TProcObj;
  x,y  : Integer;
begin
  ProcType := Uppercase(ProcType);
  x := ProcIndexOf(ProcName);
  if x <> -1 then begin
    y := Procs[x].ProcType.IndexOf({+}string(ProcType){+.});
    if y = -1 then TProcObj(fProcList.Items[x]).ProcType.add({+}string(ProcType){+.});
  end else begin
    po := TProcObj.create(self);
    po.Name := ProcName;
    po.ProcType.add({+}string(ProcType){+.});
    fProcList.add(po);
  end
end;

procedure TMethodList.ClearProcList;
begin
  fProcList.Clear;
end;

constructor TMethodList.Create(aOwner: TPSScriptExtension);
begin
  inherited create;
  fOwner     := aOwner;
  fProcList  := TObjectList.create(true);
  fEventList := TObjectList.create(true);
end;

procedure TMethodList.CreateProc(ProcName: TbtString; aPropType: TTypeData);
var
  NewProc: TbtString;
  P: PByte;
  i: Integer;
  pf: TParamFlags;

  {$IFDEF FPC}
  // mh: TParamFlags(P^) doesn't compile in FPC, this function will "fix" it.
  // yes it's ugly, but I don't know an other way to fix it
  function GetParamFlags(P: Byte): TParamFlags;
  begin
    Result := [];
    if (Ord(pfVar) and P <> 0) then Include(Result, pfVar);
    if (Ord(pfConst) and P <> 0) then Include(Result, pfConst);
    if (Ord(pfArray) and P <> 0) then Include(Result, pfArray);
    if (Ord(pfAddress) and P <> 0) then Include(Result, pfAddress);
    if (Ord(pfReference) and P <> 0) then Include(Result, pfReference);
    if (Ord(pfOut) and P <> 0) then Include(Result, pfOut);
  end;
  {$ENDIF FPC}

begin
  with aPropType do begin
    if MethodKind = mkProcedure then NewProc:='procedure '
                                else NewProc:='function ';
    NewProc := NewProc + ProcName + '(';
    P:=PByte(@ParamList);
    for i:=0 to Pred(ParamCount) do begin
      {$IFDEF FPC}
      pf := GetParamFlags(P^);
      {$ELSE}
      pf := TParamFlags(P^);
      {$ENDIF}
      if pfVar in pf then
        NewProc := NewProc + 'var ';
      {else }if pfConst in pf then
        NewProc := NewProc + 'const ';
      Inc(P);
      NewProc := NewProc + TbtString(PShortString(P)^) +' : ';
      Inc(P,Succ(P^));
      if pfArray in pf
        then NewProc := NewProc+'array of ';
      NewProc := NewProc + TbtString(PShortString(P)^);
      Inc(P,Succ(P^));
      if i < Pred(ParamCount)
        then NewProc := NewProc + '; ';
    end;
    NewProc := NewProc +')' ;
    if MethodKind = mkFunction then
      NewProc := NewProc + ':' + TbtString(PShortString(P)^);
    NewProc := NewProc + ';'^m^j
                       +'begin'^m^j^m^j
                       +'end;'^m^j;
    if Assigned(fOwner.FOnInsertMethod) then begin
      fOwner.FOnInsertMethod(fOwner, NewProc, false);
    end else begin
      InsertMethod(NewProc);
    end;
    fowner.CompileIfNeeded;
  end;
end;

procedure TMethodList.InsertMethod(NewProc: TbtString; OnTop: Boolean = False);
var
  x: Integer;
  sl: TStringList;
  nBegins: Integer;
  nProcs: Integer;
  line, test: TbtString;

  function IsItem(line, item: TbtString; First: Boolean = False): Boolean;
  var nPos: Integer;
  begin
    repeat
      nPos := pos(item,line);
      Result := (
        (nPos>0) and
        ((length(Line)-nPos<= length(item)) or (not (
          {$if declared(CharInSet)}
          CharInSet(line[nPos+length(item)],   ['0'..'9','A'..'Z','_'])
          {$else}
          (         line[nPos+length(item)] in ['0'..'9','A'..'Z','_'])
          {$ifend}
          ))
        ) and
        ((nPos = 1) or ((not first) and (not(
          {$if declared(CharInSet)}
          CharInSet(line[nPos-1],   ['0'..'9','A'..'Z','_'])
          {$else}
          (         line[nPos-1] in ['0'..'9','A'..'Z','_'])
          {$ifend}
        ))))
      );
      if nPos <> 0 then
        line := copy(line, nPos+Length(Item), Length(line));
    until (Result) or (nPos = 0);
  end;

  function DelSpaces(AText: TbtString): TbtString;
  var i: Integer;
  begin
    Result := '';
    for i := 1 to Length(AText) do
      if AText[i] <> ' ' then
        Result := Result + AText[i];
  end;

  function IsProcDecl(AnOriginalProcDecl: TbtString): Boolean;
  var
    bIsFunc: Boolean;
    iLineNo: Integer;
    sProcKey: TbtString;
    sProcDecl: TbtString;
  begin
    Result := false;
    sProcDecl := Line;
    iLineNo := x;
    bIsFunc := isItem(AnOriginalProcDecl,'FUNCTION',true);

    if bIsFunc
      then sProcKey := 'FUNCTION'
      else sProcKey := 'PROCEDURE';

    sProcDecl := copy(sProcDecl,Pos(sProcKey,sProcDecl),Length(sProcDecl));

    while not IsItem(sProcDecl,'BEGIN') do
    begin
      inc(iLineNo);
      if iLineNo > (fowner.script.Count - 1) then Exit;
      sProcDecl := sProcDecl + {+}TbtString(' ') + TbtString(uppercase(trim(fowner.script[iLineNo]))) + TbtString(' '){+.};
    end;

    sProcDecl := DelSpaces(sProcDecl);
    AnOriginalProcDecl := DelSpaces(AnOriginalProcDecl);

    sProcDecl := copy(sProcDecl,1,Length(AnOriginalProcDecl));

    Result := sProcDecl = AnOriginalProcDecl;
  end;

begin // procedure TMethodList.InsertMethod
  sl := TStringList.Create;
  Try
    sl.Text := {+}string(NewProc){+.};
    test := {+}TbtString(uppercase(trim(sl[0]))){+.};
  finally
    Sl.free;
  end;
  nProcs := 0;
  nBegins := 0;
  x := 0;
  if Not Ontop Then begin
    for x := 0 to fOwner.script.count -1 do begin
      Line := {+}TbtString(fowner.script[x]){+.};
      Line := uppercase(trim(line));
      if IsItem(line,'PROCEDURE', true) or IsItem(line,'FUNCTION', true) then begin
        if nBegins >0 then raise EMethodList.Create('Missing some ''end'' statments');
        if (nProcs = 0) and IsProcDecl(test) and
           (not IsItem(line,'FORWARD')) and (not IsItem(line,'EXTERNAL')) then
          Exit;
        Inc(nProcs);
      end;
      if IsItem(line,'FORWARD') or IsItem(line,'EXTERNAL') then
        dec(nProcs);
      if Pos({+}TbtString('END'){+.},line) < Pos({+}TbtString('BEGIN'){+.},line) then begin
        if IsItem(line,'END') then begin
          if (nBegins = 0) and (nProcs=0) then Break;
          Dec(nBegins);
          if nBegins = 0 then Dec(nProcs);
        end;
        if IsItem(line,'BEGIN') or IsItem(line,'TRY') or IsItem(line,'CASE') then begin
          if nProcs = 0 then Break;
          Inc(nBegins);
        end;
      end else begin
        if IsItem(line,'BEGIN') or IsItem(line,'TRY') or IsItem(line,'CASE') then begin
          if nProcs = 0 then Break;
          Inc(nBegins);
        end;
        if IsItem(line,'END') then begin
          if (nBegins = 0) and (nProcs=0) then Break;
          Dec(nBegins);
          if nBegins = 0 then Dec(nProcs);
        end;
      end;
    end;
  end;
  FOwner.script.BeginUpdate;
  try
    if (nProcs <> 0) or (nBegins<>0) then
      raise EMethodList.create(sMissingEndStatment);
    if (not Ontop) and (x>0) and (Trim(FOwner.script[x-1])<>'') then begin
      FOwner.script.Insert(x,'');
      inc(x);
    end;
    FOwner.script.Insert(x, {+}string(NewProc));
    FOwner.script.Text := FOwner.script.text;
  finally
    FOwner.script.EndUpdate;
  end;
end; // procedure TMethodList.InsertMethod

destructor TMethodList.Destroy;
begin
  FreeAndNil(fProcList);  {<< Needs Eventlist for removing Methods}
  FreeAndNil(fEventList);
  inherited;
end;

procedure TMethodList.FillMethods;
var
  x, y : Integer;
  m : TMethod;
begin
  for x := 0 to fEventList.Count-1 do begin
    Y := ProcIndexOf(MethodS[x].ProcName);
    if (Y >= 0) and assigned(Methods[x].Instance) then begin
      m := Procs[Y].Method;
      if m.Data = nil then begin
        m := fOwner.Exec.GetProcAsMethodN(Procs[Y].name);
        TProcObj(fProcList.Items[Y]).Method := m;
      end;
      SetMethodProp(Methods[x].Instance, {+}string(Methods[x].propname){+.}, m );
    end;
  end;
end;

function TMethodList.GetMethodName(Instance: TObject; PropName: TbtString): TbtString;
var
  x : Integer;
begin
  fOwner.CompileIfNeeded;
  x := MethodIndexOf(Instance,PropName);
  if x>=0 then Result := Methods[x].ProcName
          else Result := '';
end;

function TMethodList.GetObject(Index: Integer): TMethodObj;
begin
  Result := TMethodObj(fEventList.items[Index]);
end;

function TMethodList.GetProcObj(Index: Integer): TProcObj;
begin
  Result := TProcObj(fProcList.items[Index]);
end;

procedure TMethodList.ListEventsName(EventType: TbtString; List: TStrings);
var
  x : Integer;
begin
  if List = nil then Exit;
  EventType := UpperCase(EventType);
  List.Clear;
  fOwner.CompileIfNeeded;
  for x := 0 to fProcList.count-1 do begin
    if Procs[x].ProcType.indexof({+}string(EventType){+})<> -1 then
      List.add({+}string(Procs[x].name){+.});
  end;
end;

function TMethodList.MethodCount: Integer;
begin
  Result := fEventList.count;
end;

function TMethodList.MethodIndexOf(Instance: TObject; PropName: TbtString): Integer;
var x : integer;
begin
  Result := -1;
  for x := 0 to fEventList.count-1 do begin
    if (TMethodObj(fEventList.Items[x]).Instance = Instance) and
       ((propName='') or(TMethodObj(fEventList.Items[x]).PropName = PropName)) then begin
      Result := x;
      Exit;
    end;
  end;
end;

function TMethodList.ProcCount: Integer;
begin
  Result := fProcList.count;
end;

function TMethodList.ProcIndexOf(Name: TbtString): Integer;
var x : integer;
begin
  Result := -1;
  Name := Uppercase(name);
  for x := 0 to fProcList.count-1 do begin
    if UpperCase(TProcObj(fProcList.Items[x]).name) = name then begin
       Result := x;
       Exit;
    end;
  end;
end;

procedure TMethodList.SetMethodName(Instance: TObject; PropName: TbtString; const Value: TbtString);
var
  x, y : Integer;
  mo   : TMethodObj;

  function TypeData(Instance: TObject; const PropName: TbtString): PTypeData;
  var
    PropInfo: PPropInfo;
  begin
    // assume failure
    Result := Nil;
    PropInfo := GetPropInfo(Instance, {+}string(PropName){+});
    if PropInfo <> nil then
    begin
      Result:= GetTypeData(PropInfo^.PropType{$IFNDEF FPC}^{$ENDIF});
    end
  end;

begin
  if PropName = '' then begin
    x := 0;
    while x < MethodCount do begin
      if (Methods[x].Instance = Instance) or (Instance = nil) then
        fEventList.Delete(x)
      else Inc(x);
    end;
  end else begin
    x := MethodIndexOf(Instance, PropName);
    if value = '' then begin
       if x >= 0 then fEventList.Delete(x);
    end else begin
      fOwner.CompileIfNeeded;
      y := ProcIndexOf(Value);
      if (Y = -1) then begin
        CreateProc(Value, TypeData(Instance,propName)^);
        y := 0;
      end;
      if (x = -1) then begin
        if (Y <> -1) then begin
          mo := TMethodObj.create;
          mo.Instance := TPersistent(Instance);
          mo.ProPName := Propname;
          mo.procName := Value;
          if (MethodIndexOf(Instance,'')<>-1) and Instance.InheritsFrom(TComponent) then
            fOwner.FreeNotification(TComponent(Instance));
          fEventList.add(mo);
        end;
      end else
      begin
        Methods[x].procname := Value;
      end;
    end;
  end;
end; // procedure TMethodList.SetMethodName

procedure TMethodList.ClearAll;
begin
  fProclist.Clear;
  fEventList.Clear;
end;

{ TProcObj }

constructor TProcObj.Create(aOwner: TMethodList);
begin
  inherited Create();
  fOwner := aOwner;
  ProcType := TStringList.Create;
end;

destructor TProcObj.Destroy;
var x : Integer; m :TMethod;
begin
  if ((Method.Data <> nil) or (method.Code <> nil))
    and (fOwner <> nil) and Assigned(fOwner) then
  begin
    m.Code := nil;
    m.Data := nil;
    for x := 0 to fOwner.MethodCount-1 do begin
      if Assigned(fOwner.Methods[x].Instance) and (name = fOwner.Methods[x].ProcName) then begin
        try
          SetMethodProp(fOwner.Methods[x].Instance, string(fOwner.Methods[x].PropName),m);
        except;
        end;
      end;
    end;
  end;
  FreeAndNil(ProcType);
  inherited;
end;

procedure TProcObj.SetName(const Value: TbtString);
var x : Integer;
begin
  if FName <> Value then begin
    if FName <> '' then begin
      for x := 0 to fOwner.MethodCount-1 do begin
        if FName = fOwner.Methods[x].ProcName then begin
          fOwner.Methods[x].ProcName := Value;
        end;
      end;
    end;
    FName := Value;
  end;
end;

end.
