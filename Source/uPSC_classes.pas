{ Compiletime Classes support }
unit uPSC_classes;

{$I PascalScript.inc}
interface
uses
  uPSCompiler, uPSUtils;

{
  Will register files from:
    Classes (exception TPersistent and TComponent)

  Register STD first

}

procedure SIRegister_Classes_TypesAndConsts(Cl: TPSPascalCompiler);

procedure SIRegisterTStrings(cl: TPSPascalCompiler; Streams: Boolean);
procedure SIRegisterTStringList(cl: TPSPascalCompiler);
{$IFNDEF PS_MINIVCL}
procedure SIRegisterTBITS(Cl: TPSPascalCompiler);
{$ENDIF}
procedure SIRegisterTSTREAM(Cl: TPSPascalCompiler);
procedure SIRegisterTHANDLESTREAM(Cl: TPSPascalCompiler);
{$IFNDEF PS_MINIVCL}
procedure SIRegisterTMEMORYSTREAM(Cl: TPSPascalCompiler);
{$ENDIF}
procedure SIRegisterTFILESTREAM(Cl: TPSPascalCompiler);
procedure SIRegisterTSTRINGSTREAM(Cl: TPSPascalCompiler);
{$IFNDEF PS_MINIVCL}
procedure SIRegisterTCUSTOMMEMORYSTREAM(Cl: TPSPascalCompiler);
procedure SIRegisterTRESOURCESTREAM(Cl: TPSPascalCompiler);
procedure SIRegisterTPARSER(Cl: TPSPascalCompiler);
procedure SIRegisterTCOLLECTIONITEM(CL: TPSPascalCompiler);
procedure SIRegisterTCOLLECTION(CL: TPSPascalCompiler);
{$IFDEF DELPHI3UP}
procedure SIRegisterTOWNEDCOLLECTION(CL: TPSPascalCompiler);
{$ENDIF}
{$ENDIF}

procedure SIRegister_Classes(Cl: TPSPascalCompiler; Streams: Boolean{$IFDEF D4PLUS}=True{$ENDIF});

implementation

procedure SIRegisterTStrings(cl: TPSPascalCompiler; Streams: Boolean); // requires TPersistent
begin
  with Cl.AddClassN(cl.FindClass('TPersistent'), 'TStrings') do
  begin
    IsAbstract := True;
{$IFDEF DELPHI2005UP}
    RegisterMethod('constructor Create;');
{$ENDIF}
    RegisterMethod('function Add(S: '+C_NAME_TYPE_NATIVE_STRING+'): Integer;');
    RegisterMethod('procedure Append(S: '+C_NAME_TYPE_NATIVE_STRING+');');
    RegisterMethod('procedure AddStrings(Strings: TStrings);');
    RegisterMethod('procedure Clear;');
    RegisterMethod('procedure Delete(Index: Integer);');
    RegisterMethod('function IndexOf(const S: '+C_NAME_TYPE_NATIVE_STRING+'): Integer; ');
    RegisterMethod('procedure Insert(Index: Integer; S: '+C_NAME_TYPE_NATIVE_STRING+'); ');
    RegisterProperty('Capacity', 'Integer', iptRW);
    RegisterProperty('Delimiter', C_NAME_TYPE_NATIVE_CHAR, iptRW);
{$IFDEF DELPHI2006UP}
    RegisterProperty('StrictDelimiter', 'Boolean', iptRW);
{$ENDIF}
    RegisterProperty('DelimitedText', C_NAME_TYPE_NATIVE_STRING, iptrw);
    RegisterProperty('NameValueSeparator', C_NAME_TYPE_NATIVE_CHAR, iptRW);
    RegisterProperty('QuoteChar', C_NAME_TYPE_NATIVE_CHAR, iptRW);
    RegisterProperty('Count', 'Integer', iptR);
    RegisterProperty('Text', C_NAME_TYPE_NATIVE_STRING, iptrw);
    RegisterProperty('CommaText', C_NAME_TYPE_NATIVE_STRING, iptrw);
    if Streams then
    begin
      RegisterMethod('procedure LoadFromFile(FileName: '+C_NAME_TYPE_NATIVE_STRING+'); ');
      RegisterMethod('procedure SaveToFile(FileName: '+C_NAME_TYPE_NATIVE_STRING+'); ');
    end;
    RegisterProperty('Strings', C_NAME_TYPE_NATIVE_STRING+' Integer', iptRW);
    SetDefaultPropery('Strings');
    RegisterProperty('Objects', 'TObject Integer', iptRW);

    {$IFNDEF PS_MINIVCL}
    RegisterMethod('procedure BeginUpdate;');
    RegisterMethod('procedure EndUpdate;');
    RegisterMethod('function Equals(Strings: TStrings): Boolean;');
    RegisterMethod('procedure Exchange(Index1, Index2: Integer);');
    RegisterMethod('function IndexOfName(Name: '+C_NAME_TYPE_NATIVE_STRING+'): Integer;');
    if Streams then
      RegisterMethod('procedure LoadFromStream(Stream: TStream); ');
    RegisterMethod('procedure Move(CurIndex, NewIndex: Integer); ');
    if Streams then
      RegisterMethod('procedure SaveToStream(Stream: TStream); ');
    RegisterMethod('procedure SetText(Text: '+C_NAME_TYPE_NATIVE_PCHAR+'); ');
    RegisterProperty('Names', C_NAME_TYPE_NATIVE_STRING+' Integer', iptr);
    RegisterProperty('Values', C_NAME_TYPE_NATIVE_STRING+' string', iptRW);
    RegisterProperty('ValueFromIndex', C_NAME_TYPE_NATIVE_STRING+' Integer', iptRW);
    RegisterMethod('function AddObject(S: '+C_NAME_TYPE_NATIVE_STRING+'; AObject: TObject): Integer');
    RegisterMethod('function GetText: '+C_NAME_TYPE_NATIVE_PCHAR);
    RegisterMethod('function IndexOfObject(AObject: TObject): Integer');
    RegisterMethod('procedure InsertObject(Index: Integer; S: '+C_NAME_TYPE_NATIVE_STRING+'; AObject: TObject)');
    {$ENDIF}
  end;
end;

procedure SIRegisterTSTRINGLIST(Cl: TPSPascalCompiler);
begin
  with Cl.AddClassN(cl.FindClass('TStrings'), 'TStringList') do
  begin
{$IFDEF DELPHI2005UP}
    RegisterMethod('constructor Create;');
{$ENDIF}
    RegisterMethod('function Find(S: '+C_NAME_TYPE_NATIVE_STRING+'; var Index: Integer): Boolean');
    RegisterMethod('procedure Sort');
    RegisterProperty('CaseSensitive', 'Boolean', iptrw);
    RegisterProperty('Duplicates', 'TDuplicates', iptrw);
    RegisterProperty('Sorted', 'Boolean', iptrw);
    RegisterProperty('OnChange', 'TNotifyEvent', iptrw);
    RegisterProperty('OnChanging', 'TNotifyEvent', iptrw);
  end;
end;

{$IFNDEF PS_MINIVCL}
procedure SIRegisterTBITS(Cl: TPSPascalCompiler);
begin
  with Cl.AddClassN(cl.FindClass('TObject'), 'TBits') do
  begin
    RegisterMethod('function OpenBit: Integer');
    RegisterProperty('Bits', 'Boolean Integer', iptrw);
    RegisterProperty('Size', 'Integer', iptrw);
  end;
end;
{$ENDIF}

procedure SIRegisterTSTREAM(Cl: TPSPascalCompiler);
begin
  with Cl.AddClassN(cl.FindClass('TObject'), 'TStream') do
  begin
    IsAbstract := True;
    {+}
    //RegisterMethod('function Read(Buffer: string; Count: LongInt): LongInt');
    RegisterMethod('function Read(var Buffer:AnsiString;Count:LongInt):LongInt');
    //RegisterMethod('function Write(Buffer: string; Count: LongInt): LongInt');
    RegisterMethod('function Write(const Buffer:AnsiString;Count:LongInt):LongInt');
    {+}
    RegisterMethod('function ReadA(var Buffer:AnsiString;Count:LongInt):LongInt');
    RegisterMethod('function ReadB(var Buffer:TBytes;Count:LongInt):LongInt');
    RegisterMethod('function ReadW(var Buffer:UnicodeString;Count:LongInt):LongInt');
    RegisterMethod('function WriteA(const Buffer:AnsiString;Count:LongInt):LongInt');
    RegisterMethod('function WriteB(const Buffer:TBytes;Count:LongInt):LongInt');
    RegisterMethod('function WriteW(const Buffer:UnicodeString;Count:LongInt):LongInt');
    {.$IFNDEF PS_NOINT64}
    {$IFDEF UNICODE}
    RegisterMethod('function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64');
    {$ELSE}
    RegisterMethod('function Seek(Offset: LongInt; Origin: Word): LongInt');
    {$ENDIF}
    //RegisterMethod('procedure ReadBuffer(Buffer: string; Count: LongInt)');
    RegisterMethod('procedure ReadBuffer(const Buffer:AnsiString;Count:LongInt)');
    //RegisterMethod('procedure WriteBuffer(Buffer: string; Count: LongInt)');
    RegisterMethod('procedure WriteBuffer(var Buffer:AnsiString;Count:LongInt)');
    //
    RegisterMethod('procedure ReadBufferA(var Buffer:AnsiString;Count:LongInt)');
    RegisterMethod('procedure ReadBufferB(var Buffer:TBytes;Count:LongInt)');
    RegisterMethod('procedure ReadBufferW(var Buffer:UnicodeString;Count:LongInt)');
    RegisterMethod('procedure WriteBufferA(const Buffer:AnsiString;Count:LongInt)');
    RegisterMethod('procedure WriteBufferB(const Buffer:TBytes;Count:LongInt)');
    RegisterMethod('procedure WriteBufferW(const Buffer:UnicodeString;Count:LongInt)');
    {+.}
    {$IFDEF DELPHI4UP}
    {$IFNDEF PS_NOINT64}
    RegisterMethod('function CopyFrom(Source: TStream; Count: Int64): Int64');
    {$ELSE}
    RegisterMethod('function CopyFrom(Source: TStream; Count: Integer): LongInt');
    {$ENDIF}
    {$ELSE !DELPHI4UP}
    RegisterMethod('function CopyFrom(Source: TStream; Count: Integer): LongInt');
    {$ENDIF !DELPHI4UP}
    //
    {$IFDEF DELPHI4UP}
    {$IFNDEF PS_NOINT64}
    RegisterMethod('function CopyFromBuffer(Source: TStream; Count: Int64; BufferSize: Integer): Int64');
    {$ELSE}
    RegisterMethod('function CopyFromBuffer(Source: TStream; Count: Integer; BufferSize: Integer): LongInt');
    {$ENDIF}
    {$ELSE !DELPHI4UP}
    RegisterMethod('function CopyFromBuffer(Source: TStream; Count: Integer; BufferSize: Integer): LongInt');
    {$ENDIF !DELPHI4UP}
    {+}
    {$IFNDEF PS_NOINT64}
    RegisterProperty('Position', 'Int64', iptrw);
    RegisterProperty('Size', 'Int64', iptrw);
    {$ELSE}
    RegisterProperty('Position', 'LongInt', iptrw);
    RegisterProperty('Size', 'LongInt', iptrw);
    {$ENDIF}
    RegisterProperty('DataBytes', 'TBytes', iptr);
    {+.}
  end;
end;

procedure SIRegisterTHANDLESTREAM(Cl: TPSPascalCompiler);
begin
  with Cl.AddClassN(cl.FindClass('TStream'), 'THandleStream') do
  begin
    RegisterMethod('constructor Create(AHandle: Integer)');
    {+}
    RegisterProperty('Handle', 'NativeUInt', iptr); // THandle == NativeUInt
    {+.}
  end;
end;

{$IFNDEF PS_MINIVCL}
procedure SIRegisterTMEMORYSTREAM(Cl: TPSPascalCompiler);
begin
  with Cl.AddClassN(cl.FindClass('TCustomMemoryStream'), 'TMemoryStream') do
  begin
    RegisterMethod('procedure Clear');
    RegisterMethod('procedure LoadFromStream(Stream: TStream)');
    RegisterMethod('procedure LoadFromFile(FileName: string)');
    RegisterMethod('procedure SetSize(NewSize: LongInt)');
  end;
end;
{$ENDIF}

procedure SIRegisterTFILESTREAM(Cl: TPSPascalCompiler);
begin
  with Cl.AddClassN(cl.FindClass('THandleStream'), 'TFileStream') do
  begin
    RegisterMethod('constructor Create(FileName: string; Mode: Word)');
  end;
end;

procedure SIRegisterTSTRINGSTREAM(Cl: TPSPascalCompiler);
begin
  with Cl.AddClassN(cl.FindClass('TStream'), 'TStringStream') do
  begin
    {+}
    RegisterMethod('constructor Create(AString: string)');
    //RegisterMethod('constructor Create(AString: '+C_NAME_TYPE_NATIVE_STRING+')');
    {+.}
    {+}
    //RegisterProperty('DataString', C_NAME_TYPE_NATIVE_STRING, iptr);
    RegisterProperty('DataString', 'string', iptr);
    {+.}
  end;
end;

{$IFNDEF PS_MINIVCL}
procedure SIRegisterTCUSTOMMEMORYSTREAM(Cl: TPSPascalCompiler);
begin
  with Cl.AddClassN(cl.FindClass('TStream'), 'TCustomMemoryStream') do
  begin
    IsAbstract := True;
    RegisterMethod('procedure SaveToStream(Stream: TStream)');
    RegisterMethod('procedure SaveToFile(FileName: string)');
  end;
end;

procedure SIRegisterTRESOURCESTREAM(Cl: TPSPascalCompiler);
begin
  with Cl.AddClassN(cl.FindClass('TCustomMemoryStream'), 'TResourceStream') do
  begin
    RegisterMethod('constructor Create(Instance: THandle; ResName: string; ResType: PChar)');
    RegisterMethod('constructor CreateFromID(Instance: THandle; ResID: Integer; ResType: PChar)');
  end;
end;

procedure SIRegisterTPARSER(Cl: TPSPascalCompiler);
begin
  with Cl.AddClassN(cl.FindClass('TObject'), 'TParser') do
  begin
    RegisterMethod('constructor Create(Stream: TStream)');
    RegisterMethod('procedure CheckToken(T: Char)');
    RegisterMethod('procedure CheckTokenSymbol(S: string)');
    RegisterMethod('procedure Error(Ident: Integer)');
    RegisterMethod('procedure ErrorStr(Message: string)');
    RegisterMethod('procedure HexToBinary(Stream: TStream)');
    RegisterMethod('function NextToken: Char');
    RegisterMethod('function SourcePos: LongInt');
    RegisterMethod('function TokenComponentIdent: string');
    RegisterMethod('function TokenFloat: Extended');
    RegisterMethod('function TokenInt: LongInt');
    RegisterMethod('function TokenString: string');
    RegisterMethod('function TokenSymbolIs(S: string): Boolean');
    RegisterProperty('SourceLine', 'Integer', iptr);
    RegisterProperty('Token', 'Char', iptr);
  end;
end;

procedure SIRegisterTCOLLECTIONITEM(CL: TPSPascalCompiler);
Begin
  if cl.FindClass('TCollection') = nil then cl.AddClassN(cl.FindClass('TPersistent'), 'TCollection');
  With cl.AddClassN(cl.FindClass('TPersistent'),'TCollectionItem') do
  begin
  RegisterMethod('constructor Create(Collection: TCollection)');
  RegisterProperty('Collection', 'TCollection', iptrw);
{$IFDEF DELPHI3UP}  RegisterProperty('ID', 'Integer', iptr); {$ENDIF}
  RegisterProperty('Index', 'Integer', iptrw);
{$IFDEF DELPHI3UP}  RegisterProperty('DisplayName', 'string', iptrw); {$ENDIF}
  end;
end;

procedure SIRegisterTCOLLECTION(CL: TPSPascalCompiler);
var
  cr: TPSCompileTimeClass;
Begin
  cr := CL.FindClass('TCollection');
  if cr = nil then cr := cl.AddClassN(cl.FindClass('TPersistent'), 'TCollection');
With cr do
  begin
//  RegisterMethod('constructor Create(ItemClass: TCollectionItemClass)');
{$IFDEF DELPHI3UP}  RegisterMethod('function Owner: TPersistent'); {$ENDIF}
  RegisterMethod('function Add: TCollectionItem');
  RegisterMethod('procedure BeginUpdate');
  RegisterMethod('procedure Clear');
{$IFDEF DELPHI5UP}  RegisterMethod('procedure Delete(Index: Integer)'); {$ENDIF}
  RegisterMethod('procedure EndUpdate');
{$IFDEF DELPHI3UP}  RegisterMethod('function FindItemID(ID: Integer): TCollectionItem'); {$ENDIF}
{$IFDEF DELPHI3UP}  RegisterMethod('function Insert(Index: Integer): TCollectionItem'); {$ENDIF}
  RegisterProperty('Count', 'Integer', iptr);
{$IFDEF DELPHI3UP}  RegisterProperty('ItemClass', 'TCollectionItemClass', iptr); {$ENDIF}
  RegisterProperty('Items', 'TCollectionItem Integer', iptrw);
  end;
end;

{$IFDEF DELPHI3UP}
procedure SIRegisterTOWNEDCOLLECTION(CL: TPSPascalCompiler);
Begin
With Cl.AddClassN(cl.FindClass('TCollection'),'TOwnedCollection') do
  begin
//  RegisterMethod('constructor Create(AOwner: TPersistent; ItemClass: TCollectionItemClass)');
  end;
end;
{$ENDIF}
{$ENDIF}

procedure SIRegister_Classes_TypesAndConsts(Cl: TPSPascalCompiler);
begin
  {+}
  {.$IFNDEF PS_NOINT64}
  {$IFDEF UNICODE}
  cl.AddTypeS('TSeekOrigin', '( soBeginning, soCurrent, soEnd )');
  //
  CL.AddConstantN('soFromBeginning','TSeekOrigin').Value.tu8 := 0;//Byte(soBeginning);
  CL.AddConstantN('soFromCurrent','TSeekOrigin').Value.tu8 := 1;//Byte(soCurrent);
  CL.AddConstantN('soFromEnd','TSeekOrigin').Value.tu8 := 2;//Byte(soEnd);
  {$ELSE}
  cl.AddConstantN('soFromBeginning', 'LongInt').Value.ts32 := 0;
  cl.AddConstantN('soFromCurrent', 'LongInt').Value.ts32 := 1;
  cl.AddConstantN('soFromEnd', 'LongInt').Value.ts32 := 2;
  //
  cl.AddConstantN('soBeginning', 'LongInt').Value.ts32 := 0;
  cl.AddConstantN('soCurrent', 'LongInt').Value.ts32 := 1;
  cl.AddConstantN('soEnd', 'LongInt').Value.ts32 := 2;
  {$ENDIF}
  {+.}
  cl.AddConstantN('toEOF', 'Char').SetString(#0);
  cl.AddConstantN('toSymbol', 'Char').SetString(#1);
  cl.AddConstantN('toString', 'Char').SetString(#2);
  cl.AddConstantN('ToInteger', 'Char').SetString(#3);
  cl.AddConstantN('toFloat', 'Char').SetString(#4);
  cl.AddConstantN('fmCreate', 'LongInt').Value.ts32 := $FFFF;
  cl.AddConstantN('fmOpenRead', 'LongInt').Value.ts32 := 0;
  cl.AddConstantN('fmOpenWrite', 'LongInt').Value.ts32 := 1;
  cl.AddConstantN('fmOpenReadWrite', 'LongInt').Value.ts32 := 2;
  cl.AddConstantN('fmShareCompat', 'LongInt').Value.ts32 := 0;
  cl.AddConstantN('fmShareExclusive', 'LongInt').Value.ts32 := $10;
  cl.AddConstantN('fmShareDenyWrite', 'LongInt').Value.ts32 := $20;
  cl.AddConstantN('fmShareDenyRead', 'LongInt').Value.ts32 := $30;
  cl.AddConstantN('fmShareDenyNone', 'LongInt').Value.ts32 := $40;
  cl.AddConstantN('SecsPerDay', 'LongInt').Value.ts32 := 86400;
  cl.AddConstantN('MSecPerDay', 'LongInt').Value.ts32 := 86400000;
  cl.AddConstantN('DateDelta', 'LongInt').Value.ts32 := 693594;
  cl.AddTypeS('TAlignment', '(taLeftJustify, taRightJustify, taCenter)');
  cl.AddTypeS('THelpEvent', 'function (Command: Word; Data: LongInt; var CallHelp: Boolean): Boolean');
  cl.AddTypeS('TGetStrProc', 'procedure(const S: string)');
  cl.AddTypeS('TDuplicates', '(dupIgnore, dupAccept, dupError)');
  cl.AddTypeS('TOperation', '(opInsert, opRemove)');
  {+}
  cl.AddTypeS('THandle', {$IFDEF MSWINDOWS}'NativeUInt'{$ELSE}'Cardinal'{$ENDIF}); // 'Longint'
  {+.}
  cl.AddTypeS('TNotifyEvent', 'procedure (Sender: TObject)');
end;

procedure SIRegister_Classes(Cl: TPSPascalCompiler; Streams: Boolean);
begin
  SIRegister_Classes_TypesAndConsts(Cl);
  if Streams then
    SIRegisterTSTREAM(Cl);
  SIRegisterTStrings(cl, Streams);
  SIRegisterTStringList(cl);
  {$IFNDEF PS_MINIVCL}
  SIRegisterTBITS(cl);
  {$ENDIF}
  if Streams then
  begin
    SIRegisterTHANDLESTREAM(Cl);
    SIRegisterTFILESTREAM(Cl);
    SIRegisterTSTRINGSTREAM(Cl);
    {$IFNDEF PS_MINIVCL}
    SIRegisterTCUSTOMMEMORYSTREAM(Cl);
    SIRegisterTMEMORYSTREAM(Cl);
    SIRegisterTRESOURCESTREAM(Cl);
    {$ENDIF}
  end;
  {$IFNDEF PS_MINIVCL}
  SIRegisterTPARSER(Cl);
  SIRegisterTCOLLECTIONITEM(Cl);
  SIRegisterTCOLLECTION(Cl);
  {$IFDEF DELPHI3UP}
  SIRegisterTOWNEDCOLLECTION(Cl);
  {$ENDIF}
  {$ENDIF}
end;

// PS_MINIVCL changes by Martijn Laan (mlaan at wintax _dot_ nl)

end.
