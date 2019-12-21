unit uPSDebugger;
{$I PascalScript.inc}
interface
uses
  SysUtils, uPSRuntime, uPSUtils;

type

  TDebugMode = (dmRun
  , dmStepOver
  , dmStepInto
  , dmPaused
  );

  TPSCustomDebugExec = class(TPSExec)
  protected
    {+}
    FDebugDataLoaded: Boolean;
    {+.}
    FDebugDataForProcs: TIfList;
    FLastProc: TPSProcRec;
    FCurrentDebugProc: Pointer;
    FProcNames: TIFStringList;
    FGlobalVarNames: TIfStringList;
    {+}
    FCurrentSourcePos: Cardinal;
    //-FCurrentRow, FCurrentCol: Cardinal; // moved to  base class
    //-FCurrentFile: tbtstring; // moved to  base class
    {+.}

    function GetCurrentProcParams: TIfStringList;

    function GetCurrentProcVars: TIfStringList;
  protected

    procedure ClearDebug; virtual;
  public

    function GetCurrentProcNo: Cardinal;

    function GetCurrentPosition: Cardinal;

    {+}
    //-function TranslatePosition(Proc, Position: Cardinal): Cardinal; // moved to base class
    function TranslatePositionEx(Proc, Position: Cardinal; var Pos, Row, Col: Cardinal; var Fn: tbtstring): Boolean; override;
    {+.}

    {+}
    function LoadDebugData(const Data: tbtstring): Longint; //-override;
    {+.}

    procedure Clear; override;

    {+}
    property DebugDataLoaded: Boolean read FDebugDataLoaded;
    {+.}
    property GlobalVarNames: TIfStringList read FGlobalVarNames;

    property ProcNames: TIfStringList read FProcNames;

    property CurrentProcVars: TIfStringList read GetCurrentProcVars;

    property CurrentProcParams: TIfStringList read GetCurrentProcParams;

    function GetGlobalVar(I: Cardinal): PIfVariant;

    function GetProcVar(I: Cardinal): PIfVariant;

    function GetProcParam(I: Cardinal): PIfVariant;

    function GetCallStack(var Count: Cardinal): tbtString; {+}override;{+.}

    constructor Create;

    destructor Destroy; override;
  end;
  TPSDebugExec = class;

  TOnSourceLine = procedure (Sender: TPSDebugExec; const Name: tbtstring; Position, Row, Col: Cardinal);

  TOnIdleCall = procedure (Sender: TPSDebugExec);

  {+}
  TLineDebugInfo = (ldiEmpty, ldiNone, ldiExist);
  {+.}

  TPSDebugExec = class(TPSCustomDebugExec)
  private
    FDebugMode: TDebugMode;
    FStepOverProc: TPSInternalProcRec;
    FStepOverStackBase: Cardinal;
    FOnIdleCall: TOnIdleCall;
    FOnSourceLine: TOnSourceLine;
    FDebugEnabled: Boolean;
    {+}
    fLastProcDILoaded: TPSProcRec;
    fLineDebugInfo: TLineDebugInfo;
    {+.}
  protected

    procedure SourceChanged;
    procedure ClearDebug; override;
    procedure RunLine; override;
    {+}
    function LoadLineDebugInfo: Boolean; // Allows downloading line debugging information dynamically as needed!
    {+.}
  public
    constructor Create;

    function LoadData(const s: tbtstring): Boolean; override;

    procedure Pause; override;

    procedure Run;

    procedure StepInto;

    procedure StepOver;

    procedure Stop; override;

    property DebugMode: TDebugMode read FDebugMode;

    property OnSourceLine: TOnSourceLine read FOnSourceLine write FOnSourceLine;

    property OnIdleCall: TOnIdleCall read FOnIdleCall write FOnIdleCall;

    property DebugEnabled: Boolean read FDebugEnabled write FDebugEnabled;
  end;
  TIFPSDebugExec = TPSDebugExec;

implementation

{$if (defined(DELPHI3UP) or defined(FPC))} // {+} TODO: check "resourcestring" for modern FPC {+.}
resourcestring
{$else}
const
{$ifend}
  RPS_ExpectedReturnAddressStackBase = 'Expected return address at stack base';

{+}
type
  EPSDebugger = class(EPSError);
  Exception = EPSDebugger;
{+.}

type
  PPositionData = ^TPositionData;
  TPositionData = packed record
    FileName: tbtstring;
    Position,
    Row,
    Col,
    SourcePosition: Cardinal;
  end;
  PFunctionInfo = ^TFunctionInfo;
  TFunctionInfo = packed record
    Func: TPSProcRec;
    FParamNames: TIfStringList;
    FVariableNames: TIfStringList;
    FPositionTable: TIfList;
  end;

{ TPSCustomDebugExec }

procedure TPSCustomDebugExec.Clear;
begin
  inherited Clear;
  if FGlobalVarNames <> nil then ClearDebug;
end;

procedure TPSCustomDebugExec.ClearDebug;
var
  i, j: Longint;
  p: PFunctionInfo;
  {+}
  ppd: PPositionData;
  {+.}
begin
  FCurrentDebugProc := nil;
  FLastProc := nil;
  {+}
  FDebugDataLoaded := False;
  if Assigned(FProcNames) then
  {+.}
    FProcNames.Clear;
  {+}
  if Assigned(FGlobalVarNames) then
  {+.}
    FGlobalVarNames.Clear;
  FCurrentSourcePos := 0;
  FCurrentRow := 0;
  FCurrentCol := 0;
  FCurrentFile := '';
  {+}
  if Assigned(FDebugDataForProcs) then
  begin
  {+.}
    for i := 0 to FDebugDataForProcs.Count -1 do
    begin
      p := FDebugDataForProcs[I];
      {+}
      if p = nil then
        Continue;
      {+.}
      for j := 0 to p^.FPositionTable.Count -1 do
      begin
        {+}
        ppd := PPositionData(P^.FPositionTable[J]);
        if Assigned(ppd) then
        begin
          P^.FPositionTable[J] := nil;
          Dispose(ppd);
        end;
        {+.}
      end;
      p^.FPositionTable.Free;
      {+}
      p^.FPositionTable := nil;
      {+.}
      p^.FParamNames.Free;
      {+}
      p^.FParamNames := nil;
      {+.}
      p^.FVariableNames.Free;
      p^.FVariableNames := nil;

      {+}
      FDebugDataForProcs[I] := nil;
      {+.}
      Dispose(p);
    end;
    FDebugDataForProcs.Clear;
  {+}
  end;
  {+.}
end;

constructor TPSCustomDebugExec.Create;
begin
  inherited Create;
  //FCurrentSourcePos := 0;
  //FCurrentRow := 0;
  //FCurrentCol := 0;
  //FCurrentFile := '';
  FDebugDataForProcs := TIfList.Create;
  //FLastProc := nil;
  //FCurrentDebugProc := nil;
  FProcNames := TIFStringList.Create;
  FGlobalVarNames := TIfStringList.Create;
end;

destructor TPSCustomDebugExec.Destroy;
begin
  Clear;
  FDebugDataForProcs.Free;
  {+}
  FDebugDataForProcs := nil;
  {+.}
  FProcNames.Free;
  {+}
  FProcNames := nil;
  {+.}
  FGlobalVarNames.Free;
  FGlobalVarNames := nil;
  inherited Destroy;
end;

function TPSCustomDebugExec.GetCurrentPosition: Cardinal;
begin
  Result := TranslatePosition(GetCurrentProcNo, 0);
end;

function TPSCustomDebugExec.GetCurrentProcNo: Cardinal;
var
  i: Longint;
begin
  for i := 0 to FProcs.Count -1 do
  begin
    if FProcs[i] = FCurrProc then
    begin
      Result := I;
      Exit;
    end;
  end;
  Result := Cardinal(-1);
end;

function TPSCustomDebugExec.GetCurrentProcParams: TIfStringList;
begin
  if FCurrentDebugProc <> nil then
  begin
    Result := PFunctionInfo(FCurrentDebugProc)^.FParamNames;
  end else Result := nil;
end;

function TPSCustomDebugExec.GetCurrentProcVars: TIfStringList;
begin
  if FCurrentDebugProc <> nil then
  begin
    Result := PFunctionInfo(FCurrentDebugProc)^.FVariableNames;
  end else Result := nil;
end;

function TPSCustomDebugExec.GetGlobalVar(I: Cardinal): PIfVariant;
begin
  Result := FGlobalVars[I];
end;

function TPSCustomDebugExec.GetProcParam(I: Cardinal): PIfVariant;
begin
  Result := FStack[Cardinal(Longint(FCurrStackBase) - Longint(I) - 1)];
end;

function TPSCustomDebugExec.GetProcVar(I: Cardinal): PIfVariant;
begin
  Result := FStack[Cardinal(Longint(FCurrStackBase) + Longint(I) + 1)];
end;

function GetProcDebugInfo(FProcs: TIFList; Proc: TPSProcRec): PFunctionInfo;
var
  i: Longint;
  c: PFunctionInfo;
begin
  if Proc = nil then
  begin
    Result := nil;
    exit;
  end;
  for i := FProcs.Count -1 downto 0 do
  begin
    c := FProcs.Data^[I];
    if c^.Func = Proc then
    begin
      Result := c;
      exit;
    end;
  end;
  new(c);
  c^.Func := Proc;
  c^.FPositionTable := TIfList.Create;
  c^.FVariableNames := TIfStringList.Create;
  c^.FParamNames := TIfStringList.Create;
  FProcs.Add(c);
  REsult := c;
end;

function TPSCustomDebugExec.LoadDebugData(const Data: tbtstring): Longint;
var
  CP, I: Longint;
  c: tbtchar;
  CurrProcNo, LastProcNo: Cardinal;
  LastProc: PFunctionInfo;
  NewLoc: PPositionData;
  s: tbtstring;
begin
  {+}
  Result := 0;
  {+.}
  ClearDebug();
  if FStatus = isNotLoaded then
    exit;
  {+}
  FDebugDataLoaded := False;
  try
  {+.}
    CP := 1;
    LastProcNo := Cardinal(-1);
    LastProc := nil;
    while CP <= length(Data) do
    begin
      c := Data[CP];
      inc(cp);
      case c of
        #0:
          begin
            i := cp;
            if i > length(data) then exit;
            while Data[i] <> #0 do
            begin
              if Data[i] = #1 then
              begin
                FProcNames.Add(Copy(Data, cp, i-cp));
                cp := I + 1;
                {+}
                Inc(Result);
                {+.}
              end;
              inc(I);
              if I > length(data) then exit;
            end;
            cp := i + 1;
          end;
        #1:
          begin
            i := cp;
            if i > length(data) then exit;
            while Data[i] <> #0 do
            begin
              if Data[i] = #1 then
              begin
                FGlobalVarNames.Add(Copy(Data, cp, i-cp));
                cp := I + 1;
                {+}
                Inc(Result);
                {+.}
              end;
              inc(I);
              if I > length(data) then exit;
            end;
            cp := i + 1;
          end;
        #2:
          begin
            if cp + 4 > Length(data) then exit;
            CurrProcNo := Cardinal((@Data[cp])^);
            if CurrProcNo = Cardinal(-1) then Exit;
            if CurrProcNo <> LastProcNo then
            begin
              LastProcNo := CurrProcNo;
              LastProc := GetProcDebugInfo(FDebugDataForProcs, FProcs[CurrProcNo]);
              if LastProc = nil then exit;
              {+}
              Inc(Result);
              {+.}
            end;
            inc(cp, 4);

            i := cp;
            if i > length(data) then exit;
            while Data[i] <> #0 do
            begin
              if Data[i] = #1 then
              begin
                LastProc^.FParamNames.Add(Copy(Data, cp, i-cp));
                cp := I + 1;
                {+}
                Inc(Result);
                {+.}
              end;
              inc(I);
              if I > length(data) then exit;
            end;
            cp := i + 1;
          end;
        #3:
          begin
            if cp + 4 > Length(data) then exit;
            CurrProcNo := Cardinal((@Data[cp])^);
            if CurrProcNo = Cardinal(-1) then Exit;
            if CurrProcNo <> LastProcNo then
            begin
              LastProcNo := CurrProcNo;
              LastProc := GetProcDebugInfo(FDebugDataForProcs, FProcs[CurrProcNo]);
              if LastProc = nil then exit;
              {+}
              Inc(Result);
              {+.}
            end;
            inc(cp, 4);

            i := cp;
            if i > length(data) then exit;
            while Data[i] <> #0 do
            begin
              if Data[i] = #1 then
              begin
                LastProc^.FVariableNames.Add(Copy(Data, cp, i-cp));
                cp := I + 1;
                {+}
                Inc(Result);
                {+.}
              end;
              inc(I);
              if I > length(data) then exit;
            end;
            cp := i + 1;
          end;
        #4:
          begin
            i := cp;
            if i > length(data) then exit;
            while Data[i] <> #0 do
            begin
              if Data[i] = #1 then
              begin
                s := Copy(Data, cp, i-cp);
                cp := I + 1;
                Break;
              end;
              inc(I);
              if I > length(data) then exit;
            end;
            if cp + 4 > Length(data) then exit;
            CurrProcNo := Cardinal((@Data[cp])^);
            if CurrProcNo = Cardinal(-1) then Exit;
            if CurrProcNo <> LastProcNo then
            begin
              LastProcNo := CurrProcNo;
              LastProc := GetProcDebugInfo(FDebugDataForProcs, FProcs[CurrProcNo]);
              if LastProc = nil then exit;
              {+}
              Inc(Result);
              {+.}
            end;
            inc(cp, 4);
            if cp + 16 > Length(data) then exit;
            new(NewLoc);
            NewLoc^.Position := Cardinal((@Data[Cp])^);
            NewLoc^.FileName := s;
            NewLoc^.SourcePosition := Cardinal((@Data[Cp+4])^);
            NewLoc^.Row := Cardinal((@Data[Cp+8])^);
            NewLoc^.Col := Cardinal((@Data[Cp+12])^);
            inc(cp, 16);
            LastProc^.FPositionTable.Add(NewLoc);
            {+}
            Inc(Result);
            {+.}
          end;
        else
          begin
            ClearDebug;
            Exit;
          end;
      end;
    end;
  {+}
  finally
    FDebugDataLoaded := Result > 0;
  end;
  {+.}
end;

//function TPSCustomDebugExec.TranslatePosition(Proc, Position: Cardinal): Cardinal;
//var D1, D2: Cardinal; s: tbtstring;
//begin
//  if not TranslatePositionEx(Proc, Position, Result, D1, D2, s) then
//    Result := 0;
//end;

function TPSCustomDebugExec.TranslatePositionEx(Proc, Position: Cardinal;
  var Pos, Row, Col: Cardinal; var Fn: tbtstring): Boolean;
// Made by Martijn Laan (mlaan@wintax.nl)
var
  i: LongInt;
  fi: PFunctionInfo;
  pt: TIfList;
  r: PPositionData;
  lastfn: tbtstring;
  LastPos, LastRow, LastCol: Cardinal;
  pp: TPSProcRec;
begin
  Result := False;
  //-if (FDebugDataForProcs = nil) or (FDebugDataForProcs.Count = 0) then
  //-  Exit;
  fi := nil;
  pp := FProcs[Proc];
  {+}
  if pp = nil then Exit;
  {+.}
  for i := 0 to FDebugDataForProcs.Count -1 do begin
    fi := FDebugDataForProcs[i];
    if fi^.Func = pp then
      Break;
    fi := nil;
  end;
  LastPos := 0;
  LastRow := 0;
  LastCol := 0;
  if (fi <> nil) then
  begin
    pt := fi^.FPositionTable;
    for i := 0 to pt.Count-1 do begin // @dbg: PPositionData(pt[0]).Position    PPositionData(pt[0]).Row
      r := pt[I]; // TODO: ??? Position >= uPSRuntime.InvalidVal-1
      if (r^.Position >= Position) then begin
        if (r^.Position = Position) {+}or (LastRow = 0){+.} then begin
          Pos := r^.SourcePosition;
          Row := r^.Row;
          Col := r^.Col;
          Fn := r^.Filename;
        end else begin
          Pos := LastPos;
          Row := LastRow;
          Col := LastCol;
          Fn := LastFn;
        end;
        Result := True;
        exit;
      end else begin
        LastPos := r^.SourcePosition;
        LastRow := r^.Row;
        LastCol := r^.Col;
        LastFn := r^.FileName;
      end; //if
    end; // for i
    Pos := LastPos;
    Row := LastRow;
    Col := LastCol;
    Result := True;
  end; //if
end;

function TPSCustomDebugExec.GetCallStack(var Count: Cardinal): tbtString;
  {+}
  function GetProcIndex(Proc: TPSInternalProcRec): Cardinal;
  var
    I: Cardinal;
  begin
    for I := 0 to FProcs.Count-1 do
    begin
      if (FProcs[I] = Proc) then
      begin
        Result := I;
        Exit;
      end;
    end;
    Result := Cardinal(-1);
  end;

  function ParseParams(ParamList: TIfStringList; StackBase: Cardinal): tbtString;
  var
    I: Integer;
    P, V: tbtString;
  begin
    Result := '';
    if (ParamList.Count > 0) then
    begin
      for I := 0 to ParamList.Count-2 do
      begin
        P := ParamList.Items[I];
        if not ( (P = '') or ( (I = 0) and (P = 'Result')) ) then
        begin
          V := PSVariantToString(NewTPSVariantIFC(FStack[Cardinal(Longint(StackBase) - Longint(I) - 1)], False), '');
          Result:= Result + P + ': ' + V + '; ';
        end;
      end;
      I := ParamList.Count-1;
      P := ParamList.Items[I];
      if not ( (P = '') or ( (I = 0) and (P = 'Result')) ) then
      begin
        V := PSVariantToString(NewTPSVariantIFC(FStack[Cardinal(Longint(StackBase) - Longint(I) - 1)], False), '');
        Result:= Result + P + ': ' + V;
      end;
    end;
  end;

var
  StackBase: Cardinal;
  DebugProc: PFunctionInfo;
  Name: tbtString;
  I: Integer;
  OK: Boolean;
begin
  Count := 0;

  OK := LoadDebugInfo();
  if (not OK) then
    Exit;

  try
    StackBase := GetProcIndex(FCurrProc);
    if (StackBase <> Cardinal(-1)) then
    begin
      Result := ProcNames[StackBase] + '(' + ParseParams(GetCurrentProcParams, FCurrStackBase) + ')'#13#10;
      Inc(Count);
    end;

    StackBase := FCurrStackBase;

    while (StackBase > 0) {+}and (FStack[StackBase] <> nil){+.} do
    begin
      DebugProc := nil;

      for I := 0 to FDebugDataForProcs.Count-1 do
      begin
        if (PFunctionInfo(FDebugDataForProcs[I])^.Func = PPSVariantReturnAddress(FStack[StackBase]).Addr.ProcNo) then
        begin
          DebugProc := FDebugDataForProcs[I];
          Break;
        end;
      end;

      I := GetProcIndex(PPSVariantReturnAddress(FStack[StackBase]).Addr.ProcNo);
      if (I <= 0) then
      begin
        if Assigned(PPSVariantReturnAddress(FStack[StackBase]).Addr.ProcNo) then
          Name := PPSVariantReturnAddress(FStack[StackBase]).Addr.ProcNo.ExportName
        else
          Exit;
      end
      else
        Name := ProcNames[I];

      StackBase := PPSVariantReturnAddress(FStack[StackBase]).Addr.StackBase;
      if Assigned(DebugProc) then
        Result := Result + Name + '(' + ParseParams(DebugProc.FParamNames, StackBase) + ')'#13#10
      else
        Result := Result + Name + '(???)#13#10';

      Inc(Count);
    end; // while
  except
  end;

  if (Result <> '') then
    SetLength(Result, Length(Result)-2);
  {+.}
end;

{ TPSDebugExec }
procedure TPSDebugExec.ClearDebug;
begin
  {+}
  fLastProcDILoaded := nil;
  {+.}
  inherited;
  FDebugMode := dmRun;
end;

function TPSDebugExec.LoadData(const s: tbtstring): Boolean;
begin
  Result := inherited LoadData(s);
  FDebugMode := dmRun;
end;

{+}
procedure TPSDebugExec.RunLine;
begin
  //TODO: ?: if @FOnRunLine = nil then Exit;
  inherited RunLine;

  fLineDebugInfo := ldiEmpty;

  if FCurrProc <> FLastProc then
  begin
    FLastProc := FCurrProc;
    FCurrentDebugProc := nil;
  end;

  if DebugEnabled and FDebugDataLoaded then
    LoadLineDebugInfo();
end;
//
function TPSDebugExec.LoadLineDebugInfo: Boolean;
// Allows downloading line debugging information dynamically as needed!
var
  i: Longint;
  pt: TIfList;
  r: PPositionData;
begin
  if (fLineDebugInfo <> ldiEmpty) then
  begin
    Result := (fLineDebugInfo = ldiExist);
    Exit;
  end;

  if (not FDebugDataLoaded) then
  begin
    FCurrentSourcePos := 0;
    FCurrentRow := 0;
    FCurrentCol := 0;
    FCurrentFile := '';

    Result := False;
  end
  else
  begin
    if (FCurrProc <> {FLastProc fLastProcDILoaded}fLastProcDILoaded) then
    begin //@dbg: TPSInternalProcRec(FLastProc),r
      FLastProc := FCurrProc;
      fLastProcDILoaded := FLastProc;
      FCurrentDebugProc := nil;
      for i := 0 to FDebugDataForProcs.Count -1 do
      begin
        if PFunctionInfo(FDebugDataForProcs[i])^.Func = FLastProc then
        begin
          FCurrentDebugProc := FDebugDataForProcs[i];
          break;
        end;
      end;
    end;
    if (FCurrentDebugProc <> nil) then
    begin
      pt := PFunctionInfo(FCurrentDebugProc)^.FPositionTable;
      for i := 0 to pt.Count -1 do
      begin
        r := pt[i]; // @dbg: PPositionData(pt[0]).Position
        if (r^.Position {+}{NEW: ??">=" ;OLD: "="} = {+.} FCurrentPosition) then
        begin
          FCurrentSourcePos := r^.SourcePosition;
          FCurrentRow := r^.Row;
          FCurrentCol := r^.Col;
          FCurrentFile := r^.FileName;
          SourceChanged;
          break;
        end;
      end;
    end else
    begin
      FCurrentSourcePos := 0;
      FCurrentRow := 0;
      FCurrentCol := 0;
      FCurrentFile := '';
    end;

    Result := (FCurrentDebugProc <> nil) and (FCurrentRow <> 0);
  end;

  if Result then
    fLineDebugInfo := ldiExist
  else
    fLineDebugInfo := ldiNone;

  while FDebugMode = dmPaused do
  begin
    if @FOnIdleCall <> nil then
    begin
      FOnIdleCall(Self);
    end else break; // endless loop
  end;
end;
{+.}

procedure TPSDebugExec.SourceChanged;

  function StepOverShouldPause: Boolean;
  var
    I: Cardinal;
    V: PPSVariant;
  begin
    if (FCurrProc <> FStepOverProc) or (FCurrStackBase <> FStepOverStackBase) then
    begin
      { We're not inside the function being stepped, so scan the call stack to
        see if we're inside a function called by the function being stepped }
      I := FCurrStackBase;
      while Longint(I) > Longint(FStepOverStackBase) do
      begin
        V := FStack.Items[I];
        if (V = nil) or (V.FType <> FReturnAddressType) then
          raise Exception.Create(RPS_ExpectedReturnAddressStackBase);
        if (PPSVariantReturnAddress(V).Addr.ProcNo = FStepOverProc) and
           (PPSVariantReturnAddress(V).Addr.StackBase = FStepOverStackBase) then
        begin
          { We are, so don't pause }
          Result := False;
          Exit;
        end;
        I := PPSVariantReturnAddress(V).Addr.StackBase;
      end;
    end;
    Result := True;
  end;

begin
  case FDebugMode of
    dmStepInto:
      begin
        FDebugMode := dmPaused;
      end;
    dmStepOver:
      begin
        if StepOverShouldPause then
        begin
          FDebugMode := dmPaused;
        end;
      end;
  end;
  if @FOnSourceLine <> nil then
    FOnSourceLine(Self, FCurrentFile, FCurrentSourcePos, FCurrentRow, FCurrentCol);
end;

procedure TPSDebugExec.Pause;
begin
  FDebugMode := dmPaused;
end;

procedure TPSDebugExec.Stop;
begin
  FDebugMode := dmRun;
  inherited Stop;
end;

procedure TPSDebugExec.Run;
begin
  FDebugMode := dmRun;
end;

procedure TPSDebugExec.StepInto;
begin
  FDebugMode := dmStepInto;
end;

procedure TPSDebugExec.StepOver;
begin
  FStepOverProc := FCurrProc;
  FStepOverStackBase := FCurrStackBase;
  FDebugMode := dmStepOver;
end;

constructor TPSDebugExec.Create;
begin
  inherited Create;
  FDebugEnabled := True;
end;

end.
