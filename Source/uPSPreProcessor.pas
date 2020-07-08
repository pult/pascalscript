{ uPSPreProcessor.pas } // version: 2020.0628.1520
{----------------------------------------------------------------------------}
{ RemObjects Pascal Script                                                   }
{----------------------------------------------------------------------------}
unit uPSPreProcessor;
{$I PascalScript.inc}

interface
{$IFDEF FPC}        // FPC: {%H-} https://wiki.freepascal.org/Turn_warnings_and_hints_on_or_off
  {-warn 4015 off}  // FPC: Hint: Use DIV instead to get an integer result
{$ENDIF}

uses
  Classes, SysUtils, uPSCompiler, uPSUtils;

type
  EPSPreProcessor = class(Exception); //- jgv
  //TODO: ?EPSPreProcessor = class({+}EPSError{+.}); //- jgv

  TPSPreProcessor = class;
  TPSPascalPreProcessorParser = class;

  TPSOnNeedFile = function (Sender: TPSPreProcessor;
    const callingFilename: TbtString; var FileName, Output: TbtString): Boolean;

  //jgv - TPSOnProcessDirective - application set continue to false to stop the normal directive processing
  TPSOnProcessDirective = procedure (Sender: TPSPreProcessor;
    Parser: TPSPascalPreProcessorParser; const Active: Boolean;
    const DirectiveName, DirectiveParam: TbtString;
    var Continue: Boolean);

  TPSLineInfo = class
  private
    function GetLineOffset(I: Integer): Cardinal;
    function GetLineOffsetCount: Longint;
  protected
    FEndPos: Cardinal;
    FStartPos: Cardinal;
    FFileName: TbtString;
    FLineOffsets: TIfList;
  public
    constructor Create;
    destructor Destroy; override;

    property FileName: TbtString read FFileName;
    property StartPos: Cardinal read FStartPos;
    property EndPos: Cardinal read FEndPos;
    property LineOffsetCount: Longint read GetLineOffsetCount;
    property LineOffset[I: Longint]: Cardinal read GetLineOffset;
  end;

  TPSLineInfoResults = record
    Row, Col, Pos: Cardinal;
    Name: TbtString;
  end;

  TPSLineInfoList = class
  private
    FItems: TIfList;
    FCurrent: Longint;
    function GetCount: Longint;
    function GetItem(I: Integer): TPSLineInfo;
  protected
    function Add: TPSLineInfo;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;
    function GetLineInfo(const ModuleName: TbtString; Pos: Cardinal; var Res: TPSLineInfoResults): Boolean;

    property Count: Longint read GetCount;
    property Items[I: Longint]: TPSLineInfo read GetItem; default;
    property Current: Longint read FCurrent write FCurrent;
  end;

  TPSDefineStates = class;

  TPSPreProcessor = class
  private
    FID: Pointer;
    FCurrentDefines, FDefines: TStringList;
    FCurrentLineInfo: TPSLineInfoList;
    FOnNeedFile: TPSOnNeedFile;
    FAddedPosition: Cardinal;
    FDefineState: TPSDefineStates;
    FMaxLevel: Longint;
    FMainFileName: tbtstring;
    FMainFile: tbtstring;
    FOnProcessDirective: TPSOnProcessDirective;
    FOnProcessUnknowDirective: TPSOnProcessDirective;

    procedure ParserNewLine(Sender: TPSPascalPreProcessorParser; {%H-}Row, {%H-}Col, Pos: Cardinal);
    procedure IntPreProcess(Level: Integer; const OrgFileName: TbtString; FileName: TbtString; Dest: TStream);
  protected
    procedure doAddStdPredefines; virtual; // jgv
  public
    constructor Create;
    destructor Destroy; override;

    procedure AdjustMessages(Comp: TPSPascalCompiler);
    procedure AdjustMessage(Msg: TPSPascalCompilerMessage); //-jgv
    procedure PreProcess(const Filename: tbtstring; var Output: tbtstring);
    procedure Clear;

    {The maximum number of levels deep the parser will go, defaults to 20}
    property MaxLevel: Longint read FMaxLevel write FMaxLevel;
    property CurrentLineInfo: TPSLineInfoList read FCurrentLineInfo;

    property Defines: TStringList read FDefines write FDefines;
    property MainFile: TbtString read FMainFile write FMainFile;
    property MainFileName: TbtString read FMainFileName write FMainFileName;
    property ID: Pointer read FID write FID;

    property OnNeedFile: TPSOnNeedFile read FOnNeedFile write FOnNeedFile;
    property OnProcessDirective: TPSOnProcessDirective read fOnProcessDirective write fOnProcessDirective;
    property OnProcessUnknowDirective: TPSOnProcessDirective read fOnProcessUnknowDirective write fOnProcessUnknowDirective;
  end;

  TPSPascalPreProcessorType = (ptEOF, ptOther, ptDefine);

  TPSOnNewLine = procedure(Sender: TPSPascalPreProcessorParser; Row, Col, Pos: Cardinal) of object;

  TPSPascalPreProcessorParser = class
  private
    FData: tbtstring;
    FText: PAnsichar;
    FToken: tbtstring;
    FTokenId: TPSPascalPreProcessorType;
    FLastEnterPos, FLen, FRow, FCol, FPos: Cardinal;
    FOnNewLine: TPSOnNewLine;
  public
    procedure SetText(const dta: TbtString);
    procedure Next;

    property Token: TbtString read FToken;
    property TokenId: TPSPascalPreProcessorType read FTokenId;
    property Row: Cardinal read FRow;
    property Col: Cardinal read FCol;
    property Pos: Cardinal read FPos;
    property OnNewLine: TPSOnNewLine read FOnNewLine write FOnNewLine;
  end;

  TPSDefineState = class
  private
    FInElse: Boolean;
    FDoWrite: Boolean;
  public
    property InElse: Boolean read FInElse write FInElse;
    property DoWrite: Boolean read FDoWrite write FDoWrite;
  end;

  TPSDefineStates = class
  private
    FItems: TIfList;
    function GetCount: Longint;
    function GetItem(I: Integer): TPSDefineState;
    function GetWrite: Boolean;
    function GetPrevWrite: Boolean; //JeromeWelsh - nesting fix
  public
    constructor Create;
    destructor Destroy; override;

    function Add: TPSDefineState;
    procedure Delete(I: Longint);
    procedure Clear;

    property Count: Longint read GetCount;
    property Item[I: Longint]: TPSDefineState read GetItem; default;
    property DoWrite: Boolean read GetWrite;
    property DoPrevWrite: Boolean read GetPrevWrite; //JeromeWelsh - nesting fix
  end;

implementation

{+}
{$IFDEF DELPHI12UP}
uses
  AnsiStrings;
{$ENDIF}
{+.}

{$if (defined(DELPHI3UP) or defined(FPC))}
resourcestring
{$else}
const
{$ifend}
  RPS_TooManyNestedInclude = 'Too many nested include files while processing ''%s'' from ''%s''';
  {+}
  RPS_IncludeNotFound = 'Unable to find file ''%s''';
  RPS_IncludeNotFoundFrom = ' used from ''%s''';
  {+.}
  RPS_DefineTooManyParameters = 'Too many parameters in ''%s'' at %d:%d';
  RPS_NoIfdefForEndif = 'No IFDEF for ENDIF in ''%s'' at %d:%d';
  RPS_NoIfdefForElse = 'No IFDEF for ELSE in ''%s'' at %d:%d';
  RPS_ElseTwice = 'Can''t use ELSE twice in ''%s'' at %d:%d';
  RPS_UnknownCompilerDirective = 'Unknown compiler directives in ''%s'' at %d:%d';
  RPs_DefineNotClosed = 'Define not closed';

{ TPSLineInfoList }

function TPSLineInfoList.Add: TPSLineInfo;
begin
  Result := TPSLineInfo.Create;
  FItems.Add(Result);
end;

procedure TPSLineInfoList.Clear;
begin
  FItems.ClearAsObjects();
end;

constructor TPSLineInfoList.Create;
begin
  inherited Create;
  FItems := TIfList.Create;
end;

destructor TPSLineInfoList.Destroy;
begin
  if Assigned(FItems) then begin
    FItems.ClearAsObjects({Destroying:}True);
    FreeAndNil(FItems);
  end;
  inherited Destroy;
end;

function TPSLineInfoList.GetCount: Longint;
begin
  Result := FItems.Count;
end;

function TPSLineInfoList.GetItem(I: Integer): TPSLineInfo;
begin
  Result := TPSLineInfo(FItems[i]);
end;

function TPSLineInfoList.GetLineInfo(const ModuleName: TbtString; Pos: Cardinal; var Res: TPSLineInfoResults): Boolean;
var
  OK: Boolean;
  i,j: Longint;
  linepos: Cardinal;
  Item: TPSLineInfo;
  lModuleName: tbtstring;
begin
  lModuleName := FastUpperCase(ModuleName);
  for i := FItems.Count-1 downto 0 do begin
    Item := FItems[i];
    OK := (Pos >= Item.StartPos) and (Pos < Item.EndPos) and
      (lModuleName = '') or (lModuleName = Item.FileName);
    if OK then
    begin
      Res.Name := Item.FileName;
      Pos := Pos - Item.StartPos;
      Res.Pos := Pos;
      Res.Col := 1;
      Res.Row := 1;
      LinePos := 0; // DCC64: Hint: H2077 Value assigned to 'linepos' never used
      for j := 0 to Item.LineOffsetCount-1 do begin
        if Pos >= Item.LineOffset[j] then begin
          linepos := Item.LineOffset[j];
        end else
        begin
          //[-] https://github.com/remobjects/pascalscript/pull/221
          //Res.Row := j; // j -1, but line counting starts at 1
          //Res.Col := pos - linepos + 1;
          //[-]
          Break;
        end;
        //[+] https://github.com/remobjects/pascalscript/pull/221
        Res.Row := j + 1; // line counting starts at 1
        Res.Col := pos - linepos + 1;
        //[+]
      end; // for j
      {+}
      {$IFNDEF FPC}{$IFDEF CPU64}
      if LinePos = 0 then ; // DCC64: Hint: H2077 Value assigned to 'linepos' never used
      {$ENDIF}{$ENDIF}
      {+.}
      Result := True;
      Exit;
    end;
  end; // for
  Result := False;
end; // function TPSLineInfoList.GetLineInfo

{ TPSLineInfo }

constructor TPSLineInfo.Create;
begin
  inherited Create;
  FLineOffsets := TIfList.Create;
end;

destructor TPSLineInfo.Destroy;
begin
  FreeAndNil(FLineOffsets);
  inherited Destroy;
end;

function TPSLineInfo.GetLineOffset(I: Integer): Cardinal;
begin
  Result := {%H-}Longint(FLineOffsets[I]);
end;

function TPSLineInfo.GetLineOffsetCount: Longint;
begin
  Result := FLineOffsets.Count;
end;

{ TPSPascalPreProcessorParser }

procedure TPSPascalPreProcessorParser.Next;
var ci: Cardinal;
begin
  FPos := FPos + FLen;
  case FText[FPos] of
    #0:
      begin
        FLen := 0;
        FTokenId := ptEof;
      end;
    '''':
      begin
        ci := FPos;
        while (FText[ci] <> #0) do begin
          Inc(ci);
          while FText[ci] = '''' do begin
            if FText[ci+1] <> '''' then
              Break;
            Inc(ci);
            Inc(ci);
          end;
          if FText[ci] = '''' then
            Break;
          if FText[ci] = #13 then begin
            inc(FRow);
            if FText[ci] = #10 then
              Inc(ci);
            FLastEnterPos := ci -1;
            if @FOnNewLine <> nil then
              FOnNewLine(Self, FRow, FPos - FLastEnterPos + 1, ci+1);
            Break;
          end else if FText[ci] = #10 then begin
            Inc(FRow);
            FLastEnterPos := ci -1;
            if @FOnNewLine <> nil then
              FOnNewLine(Self, FRow, FPos - FLastEnterPos + 1, ci+1);
            Break;
          end;
        end;
        FLen := ci - FPos + 1;
        FTokenId := ptOther;
      end;
    '(':
      begin
        if FText[FPos + 1] = '*' then begin
          ci := FPos + 1;
          while (FText[ci] <> #0) do begin
            if (FText[ci] = '*') and (FText[ci + 1] = ')') then
              Break;
            if FText[ci] = #13 then begin
              Inc(FRow);
              if FText[ci+1] = #10 then
                Inc(ci);
              FLastEnterPos := ci -1;
              if @FOnNewLine <> nil then
                FOnNewLine(Self, FRow, FPos - FLastEnterPos + 1, ci+1);
            end else if FText[ci] = #10 then begin
              inc(FRow);
              FLastEnterPos := ci -1;
              if @FOnNewLine <> nil then
                FOnNewLine(Self, FRow, FPos - FLastEnterPos + 1, ci+1);
            end;
            Inc(ci);
          end;
          FTokenId := ptOther;
          if (FText[ci] <> #0) then
            Inc(ci, 2);
          FLen := ci - FPos;
        end else begin
          FTokenId := ptOther;
          FLen := 1;
        end;
      end;
      '/':
        begin
          if FText[FPos + 1] = '/' then begin
            ci := FPos + 1;
            while (FText[ci] <> #0) and (FText[ci] <> #13) and
              (FText[ci] <> #10) do
            begin
              Inc(ci);
            end;
            FTokenId := ptOther;
            FLen := ci - FPos;
          end else begin
            FTokenId := ptOther;
            FLen := 1;
          end;
        end;
      '{':
        begin
          ci := FPos + 1;
          while (FText[ci] <> #0) and (FText[ci] <> '}') do begin
            if FText[ci] = #13 then begin
              Inc(FRow);
              if FText[ci+1] = #10 then
                Inc(ci);
              FLastEnterPos := ci - 1;
              if @FOnNewLine <> nil then
                FOnNewLine(Self, FRow, FPos - FLastEnterPos + 1, ci+1);
            end else if FText[ci] = #10 then begin
              Inc(FRow);
              FLastEnterPos := ci - 1;
              if @FOnNewLine <> nil then
                FOnNewLine(Self, FRow, FPos - FLastEnterPos + 1, ci+1);
            end;
            Inc(ci);
          end;
          if FText[FPos + 1] = '$' then
            FTokenId := ptDefine
          else
            FTokenId := ptOther;
          FLen := ci - FPos + 1;
        end;
      else begin
        ci := FPos + 1;
        while not (FText[ci] in [#0,'{', '(', '''', '/']) do begin
          if FText[ci] = #13 then begin
            Inc(FRow);
            if FText[ci+1] = #10 then
              Inc(ci);
            FLastEnterPos := ci - 1;
            if @FOnNewLine <> nil then
              FOnNewLine(Self, FRow, FPos - FLastEnterPos + 1, ci+1);
          end else if FText[ci] = #10 then begin
            Inc(FRow);
            FLastEnterPos := ci -1 ;
            if @FOnNewLine <> nil then
              FOnNewLine(Self, FRow, FPos - FLastEnterPos + 1, ci+1);
          end;
          Inc(Ci);
        end;
        FTokenId := ptOther;
        FLen := ci - FPos;
      end;
  end; // case
  FCol := FPos - FLastEnterPos + 1;
  FToken := Copy(FData, FPos +1, FLen);
end; // procedure TPSPascalPreProcessorParser.Next

procedure TPSPascalPreProcessorParser.SetText(const dta: TbtString);
begin
  FData := dta;
  FText := PAnsiChar(FData);
  FLen := 0;
  FPos := 0;
  FCol := 1;
  FLastEnterPos := 0;
  FRow := 1;
  if @FOnNewLine <> nil then
    FOnNewLine(Self, 1, 1, 0);
  Next;
end;

{ TPSPreProcessor }

procedure TPSPreProcessor.AdjustMessage(Msg: TPSPascalCompilerMessage);
var Res: TPSLineInfoResults;
begin
  if CurrentLineInfo.GetLineInfo(Msg.ModuleName, Msg.Pos, {%H-}Res) then begin
    Msg.SetCustomPos(res.Pos, Res.Row, Res.Col);
    Msg.ModuleName := Res.Name;
  end;
end;

procedure TPSPreProcessor.AdjustMessages(Comp: TPSPascalCompiler);
var i: Longint;
begin
  for i := 0 to Comp.MsgCount -1 do
    AdjustMessage(Comp.Msg[i]);
end;

procedure TPSPreProcessor.Clear;
begin
  FDefineState.Clear;
  FDefines.Clear;
  FCurrentDefines.Clear;
  FCurrentLineInfo.Clear;
  FMainFile := '';
end;

constructor TPSPreProcessor.Create;
begin
  inherited Create;
  FDefines := TStringList.Create;
  FCurrentLineInfo := TPSLineInfoList.Create;
  FCurrentDefines := TStringList.Create;
  FDefines.Duplicates := dupIgnore;
  FCurrentDefines.Duplicates := dupIgnore;
  FDefineState := TPSDefineStates.Create;
  FMaxLevel := 20;

  doAddStdPredefines;
end;

destructor TPSPreProcessor.Destroy;
begin
  FreeAndNil(FDefineState);
  FreeAndNil(FCurrentDefines);
  FreeAndNil(FDefines);
  FreeAndNil(FCurrentLineInfo);
  inherited Destroy;
end;

procedure TPSPreProcessor.doAddStdPredefines;
begin
  //--- 20050708_jgv
  FCurrentDefines.Add (Format ('VER%d', [PSCurrentBuildNo]));
  {$IFDEF CPU386 }
  FCurrentDefines.Add ('CPU386');
  {$ENDIF }
  {$IFDEF MSWINDOWS }
    FCurrentDefines.Add ('MSWINDOWS');
    FCurrentDefines.Add ('WIN32');
  {$ENDIF }
  {$IFDEF LINUX }
    FCurrentDefines.Add ('LINUX');
  {$ENDIF }
end;

procedure TPSPreProcessor.IntPreProcess(Level: Integer; const OrgFileName: TbtString; FileName: TbtString;
  Dest: TStream);
var
  Parser: TPSPascalPreProcessorParser;
  dta: TbtString;
  item: TPSLineInfo;
  s, name: TbtString;
  current, i: Longint;
  ds: TPSDefineState;
  AppContinue: Boolean;
  ADoWrite: Boolean;
begin
  if Level > MaxLevel then raise EPSPreProcessor.CreateFmt(RPS_TooManyNestedInclude, [FileName, OrgFileName]);
  Parser := TPSPascalPreProcessorParser.Create;
  try
    Parser.OnNewLine := ParserNewLine;
    if FileName = MainFileName then begin
      dta := MainFile;
    end else if (@OnNeedFile = nil)
      or (not OnNeedFile(Self, OrgFileName, FileName, dta)) then
    {+}
    begin
      if OrgFileName <> '' then
        raise EPSPreProcessor.CreateFmt(RPS_IncludeNotFound + RPS_IncludeNotFoundFrom, [FileName, OrgFileName])
      else
        raise EPSPreProcessor.CreateFmt(RPS_IncludeNotFound, [FileName])
    end;
    {+.}
    Item := FCurrentLineInfo.Add;
    current := FCurrentLineInfo.Count-1;
    FCurrentLineInfo.Current := current;
    Item.FStartPos := Dest.Position;
    Item.FFileName := FileName;
    Parser.SetText(dta);
    while Parser.TokenId <> ptEOF do begin
      s := Parser.Token;
      if Parser.TokenId = ptDefine then begin
        Delete(s,1,2);  // delete the {$
        Delete(s,Length(s), 1); // delete the }

        //-- 20050707_jgv trim right
        i := Length(s);
        while (i > 0) and (s[i] = ' ') do begin
          Delete (s, i, 1);
          Dec (i);
        end;
        //-- end_jgv

        if Pos(TbtChar(' '), s) = 0 then begin
          name := {+}tbtstring(uppercase(s)){+.};
          s := '';
        end else begin
          Name := {+}TbtString(UpperCase(Copy(s,1,Pos(TbtString(' '), s)-1))){+.};
          Delete(s, 1, Pos({+}TbtString(' '){+.}, s));
        end;

        //-- 20050707_jgv - ask the application
        AppContinue := True;
        If @OnProcessDirective <> nil then
           OnProcessDirective (Self, Parser, FDefineState.DoWrite, name, {+}TbtString(s){+.}, AppContinue);

        if AppContinue then begin
        //-- end jgv
            if (Name = 'I') or (Name = 'INCLUDE') then begin
            if FDefineState.DoWrite then begin
              FAddedPosition := 0;
              IntPreProcess(Level +1, FileName, {+}tbtstring(s){+.}, Dest);
              FCurrentLineInfo.Current := current;
              FAddedPosition := Cardinal(Dest.Position) - Item.StartPos - Parser.Pos;
            end;
          end else if (Name = 'DEFINE') then begin
            if FDefineState.DoWrite then begin
              if pos(' ', {+}string(S){+.}) <> 0 then
                raise EPSPreProcessor.CreateFmt(RPS_DefineTooManyParameters, [string(FileName), Parser.Row, Parser.Col]);
              FCurrentDefines.Add(Uppercase({+}string(S){+.}));
            end;
          end else if (Name = 'UNDEF') then begin
            if FDefineState.DoWrite then begin
              if pos(' ', {+}string(S){+.}) <> 0 then
                raise EPSPreProcessor.CreateFmt(RPS_DefineTooManyParameters, [string(FileName), Parser.Row, Parser.Col]);
              i := FCurrentDefines.IndexOf(Uppercase({+}string(S){+.}));
              if i <> -1 then
                FCurrentDefines.Delete(i);
            end;
          end else if (Name = 'IFDEF') then begin
            if pos(' ', {+}string(S){+.}) <> 0 then
              raise EPSPreProcessor.CreateFmt(RPS_DefineTooManyParameters, [string(FileName), Parser.Row, Parser.Col]);
            {+}
            //JeromeWelsh - nesting fix
            ADoWrite := (FCurrentDefines.IndexOf(UpperCase(string(s))) >= 0) and FDefineState.DoWrite;
            FDefineState.Add.DoWrite := ADoWrite;
            {+.}
          end else if (Name = 'IFNDEF') then begin
            if pos(' ', {+}string(S){+.}) <> 0 then
              raise EPSPreProcessor.CreateFmt(RPS_DefineTooManyParameters, [string(FileName), Parser.Row, Parser.Col]);
            //JeromeWelsh - nesting fix
            {+}
            //JeromeWelsh - nesting fix
            ADoWrite := (FCurrentDefines.IndexOf(UpperCase(string(s))) < 0) and FDefineState.DoWrite;
            FDefineState.Add.DoWrite := ADoWrite;
            {+.}
          end else if (Name = 'ENDIF') then begin
            //- jgv remove - borland use it (sysutils.pas)
            //- if s <> '' then raise EPSPreProcessor.CreateFmt(RPS_DefineTooManyParameters, [string(FileName), Parser.Row, Parser.Col]);
            if FDefineState.Count = 0 then
              raise EPSPreProcessor.CreateFmt(RPS_NoIfdefForEndif, [string(FileName), Parser.Row, Parser.Col]);
            FDefineState.Delete(FDefineState.Count -1); // remove define from list
          end else if (Name = 'ELSE') then begin
            if s<> '' then raise EPSPreProcessor.CreateFmt(RPS_DefineTooManyParameters, [string(FileName), Parser.Row, Parser.Col]);
            if FDefineState.Count = 0 then
              raise EPSPreProcessor.CreateFmt(RPS_NoIfdefForElse, [string(FileName), Parser.Row, Parser.Col]);
            ds := FDefineState[FDefineState.Count -1];
            if ds.InElse then
              raise EPSPreProcessor.CreateFmt(RPS_ElseTwice, [string(FileName), Parser.Row, Parser.Col]);
            ds.FInElse := True;
            //JeromeWelsh - nesting fix
            ds.DoWrite := not ds.DoWrite and FDefineState.DoPrevWrite;
          end
          //-- 20050710_jgv custom application error process
          else begin
            If @OnProcessUnknowDirective <> nil then begin
              OnProcessUnknowDirective (Self, Parser, FDefineState.DoWrite, name, {+}tbtstring(s){+.}, AppContinue);
            end;
            if AppContinue then
          //-- end jgv
              raise EPSPreProcessor.CreateFmt(RPS_UnknownCompilerDirective, [string(FileName), Parser.Row, Parser.Col]);
          end;
        end; // if AppContinue
      end; // if Parser.TokenId = ptDefine

      if (not FDefineState.DoWrite) or (Parser.TokenId = ptDefine) then begin
        SetLength(s, Length(Parser.Token));
        for i := length(s) downto 1 do
          s[i] := #32; // space
      end;
      Dest.Write(s[1], length(s));
      Parser.Next;
    end; // while
    Item.FEndPos := Dest.Position;
  finally
    Parser.Free;
  end;
end; // procedure TPSPreProcessor.IntPreProcess

procedure TPSPreProcessor.ParserNewLine(Sender: TPSPascalPreProcessorParser; {%H-}Row, {%H-}Col, Pos: Cardinal);
begin
  if FCurrentLineInfo.Current >= FCurrentLineInfo.Count then
    Exit; //errr ???
  with FCurrentLineInfo.Items[FCurrentLineInfo.Current] do begin
    Pos := Pos + FAddedPosition;
    FLineOffsets.Add({%H-}Pointer(Pos));
  end;
end;

procedure TPSPreProcessor.PreProcess(const Filename: TbtString; var Output: TbtString);
var Stream: TMemoryStream;
begin
  FAddedPosition := 0;
  {$IFDEF FPC}
  FCurrentDefines.AddStrings(FDefines);
  {$ELSE !FPC}
  FCurrentDefines.Assign(FDefines);
  {$ENDIF !FPC}
  Stream := TMemoryStream.Create;
  try
    IntPreProcess(0, '', FileName, Stream);
    Stream.Position := 0;
    SetLength(Output, Stream.Size);
    Stream.Read(Output[1], Length(Output));
  finally
    Stream.Free;
  end;
  if FDefineState.Count <> 0 then
    raise EPSPreProcessor.Create(RPs_DefineNotClosed);
end;

{ TPSDefineStates }

function TPSDefineStates.Add: TPSDefineState;
begin
  Result := TPSDefineState.Create;
  FItems.Add(Result);
end;

procedure TPSDefineStates.Clear;
begin
  FItems.ClearAsObjects();
end;

constructor TPSDefineStates.Create;
begin
  inherited Create;
  FItems := TIfList.Create;
end;

procedure TPSDefineStates.Delete(I: Integer);
begin
  TPSDefineState(FItems[i]).Free;
  FItems.Delete(i);
end;

destructor TPSDefineStates.Destroy;
begin
  if Assigned(FItems) then begin
    FItems.ClearAsObjects({Destroying:}True);
    FreeAndNil(FItems);
  end;
  inherited Destroy;
end;

function TPSDefineStates.GetCount: Longint;
begin
  Result := FItems.Count;
end;

function TPSDefineStates.GetItem(I: Integer): TPSDefineState;
begin
  Result := FItems[i];
end;

function TPSDefineStates.GetWrite: Boolean;
begin
  if FItems.Count = 0 then
    Result := True
  else
    Result := TPSDefineState(FItems[FItems.Count -1]).DoWrite;
end;

//JeromeWelsh - nesting fix
function TPSDefineStates.GetPrevWrite: Boolean;
begin
  if FItems.Count < 2 then
    Result := True
  else
    Result := TPSDefineState(FItems[FItems.Count -2]).DoWrite;
end;

end.
