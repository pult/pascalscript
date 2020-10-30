{ uPSUtils.pas } // version: 2020.1030.1825
{----------------------------------------------------------------------------}
{ RemObjects Pascal Script                                                   }
{----------------------------------------------------------------------------}
unit uPSUtils;
{$I PascalScript.inc}

interface

uses
  {$IFDEF DELPHI7UP}Types,{$ENDIF}
  //{$IFDEF DELPHI17UP}System.UITypes,{$ENDIF}
  Classes, SysUtils
  {$IFNDEF FPC}
  {$IFDEF MSWINDOWS}
  ,Windows
  {$ENDIF MSWINDOWS}
  {$ENDIF !FPC}
  ;

{+}
const
  uPSVersion = 202010301825; // format: yyyymmddhhnn
            // yyyymmddhhnn
  {$EXTERNALSYM uPSVersion}
  (*
  // Sample for checking library version:
  // <sample>
  uses ... uPSUtils ...
  {$warn comparison_true off}
  {$if (not declared(uPSVersion)) or (uPSVersion < 202010301825)}
    //{$warn message_directive on}{$MESSAGE WARN 'Need update RemObjects Pascal Script Library'}
    {$MESSAGE FATAL 'Need update RemObjects Pascal Script Library'}
  {$ifend}{$warnings on}
  // <\sample>
  //*)
{+.}

const
  PSMainProcName           = '!MAIN';
  PSMainProcNameOrg        = 'Main Proc';
  PSLowBuildSupport        = 12;
  PSCurrentBuildNo         = 23;
  PSCurrentversion         = '1.31';
  PSValidHeader            = 1397769801; // == "IFPS"
  PSAddrStackStart         = 1610612736;
  PSAddrNegativeStackStart = 1073741824;

type
  {$IFDEF NEXTGEN}
    //AnsiChar = Char;
    //PAnsiChar = ^AnsiChar;
    //AnsiString = string;
    //PAnsiString = ^AnsiString;
  {$ELSE !NEXTGEN}
    {.$if not declared(AnsiChar)}
    {$IFNDEF FPC}{$IFNDEF DELPHI4UP}
    AnsiChar = Char;
    PAnsiChar = ^AnsiChar;
    {$ENDIF}{$ENDIF}
    {.$ifend}
    //
    {.$if not declared(AnsiString)}
    {$IFNDEF FPC}{$IFNDEF DELPHI4UP}
    AnsiString = string;
    PAnsiString = ^AnsiString;
    {$ENDIF}{$ENDIF}
    {.$ifend}
  {$ENDIF !NEXTGEN}
  //
  {$IFDEF PS_BTSTRINGNATIVE}
  TbtChar = Char;
  PTbtChar = PChar;
  TbtString = {$IFDEF UNICODE_OR_FPC}type {$ENDIF} string;
  PTbtString = PString;
  {$ELSE !PS_PS_BTSTRINGNATIVE}
  TbtChar = AnsiChar;
  PTbtChar = PAnsiChar;
  TbtString = {$IFDEF UNICODE_OR_FPC}type {$ENDIF} AnsiString;
  PTbtString = PAnsiString;
  {$ENDIF !PS_BTSTRINGNATIVE}
  //
  TPSBaseType = Byte;
  TPSVariableType = (ivtGlobal, ivtParam, ivtVariable);

const
  btCharSize = SizeOf(TbtChar);
  {$EXTERNALSYM btCharSize}
  {$IFNDEF FPC}{$warn comparison_true off}{$ENDIF}
  btCharIsWide = {$if btCharSize=2}True{$else}False{$ifend};
  {$EXTERNALSYM btCharIsWide}
  btCharIsAnsi = not btCharIsWide;
  {$EXTERNALSYM btCharIsAnsi}
  btCharSizeNative = SizeOf(Char);
  btCharIsNative = {$if btCharSize=btCharSizeNative}True{$else}False{$ifend};
  {$EXTERNALSYM btCharIsNative}
{$IFDEF _DCC_MSG_}
  {$IFDEF _PS_TRACE_}
    {$MESSAGE 'Note: RPS defined _PS_TRACE_'}
  {$ENDIF}
  {$IFDEF _PS_TRACE_INVOK_}
    {$MESSAGE 'Note: RPS defined _PS_TRACE_INVOK_'}
  {$ENDIF}
  {$if btCharIsWide}
    {$MESSAGE 'Note: RPS btChar is Wide'}
  {$else}
    {$MESSAGE 'Note: RPS btChar is Ansi'}
  {$ifend}
  {$if btCharIsNative}
    {$MESSAGE 'Note: RPS btChar == System.Char'}
  {$else}
    {$MESSAGE 'Note: RPS btChar <> System.Char (==Ansi)'}
  {$ifend}
{$ENDIF _DCC_MSG_}

const
  btReturnAddress       = 0;
  btU8                  = 1;
  btS8                  = 2;
  btU16                 = 3;
  btS16                 = 4;
  btU32                 = 5;
  btS32                 = 6;
  btSingle              = 7;
  btDouble              = 8;
  btExtended            = 9;
  btString              = 10;
  btRecord              = 11;
  btArray               = 12;
  btPointer             = 13;
  btPChar               = 14;
  btResourcePointer     = 15;
  btVariant             = 16;
{$IFNDEF PS_NOINT64}
  btS64                 = 17;
  {+}
  btU64                 = btS64{30}; // TODO: change value to unique and add this type of processing
  {+.}
{$ENDIF}
  btChar                = 18;
{$IFNDEF PS_NOWIDESTRING}
  btWideString          = 19;
  btWideChar            = 20;
{$ELSE}
  {$IFDEF UNICODE}
  btWideChar            = 20;
  {$ENDIF}
{$ENDIF}
  btProcPtr             = 21;
  btStaticArray         = 22;
  btSet                 = 23;
  btCurrency            = 24;
  btClass               = 25;
  btInterface           = 26;
  btNotificationVariant = 27;
  btUnicodeString       = 28;
  {+}
  btPWideChar           = 29;
  {$EXTERNALSYM btPWideChar}
  {+.}
  btType                = 130;
  btEnum                = 129;
  btExtClass            = 131;

type
  TPSTypeRecBase = class
  //private
  protected
    FExportNameHash: Longint;
    FExportName: TbtString;
    FBaseType: TPSBaseType;
  protected
    FRealSize: Cardinal;
  public
    function BaseTypeToStr: string; virtual;

    property RealSize: Cardinal read FRealSize;
    property BaseType: TPSBaseType read FBaseType write FBaseType;
    property ExportName: TbtString read FExportName write FExportName;
    property ExportNameHash: Longint read FExportNameHash write FExportNameHash;
  end;
  PIFTypeRecBase = TPSTypeRecBase;

function PSBaseTypeToStr(BaseType: TPSBaseType): string; overload;
function PSBaseTypeToStr(BaseType: PIFTypeRecBase): string; overload;

// Small hash maker:
function MakeHash(const S: ShortString): Longint; overload;
function MakeHash(const S: AnsiString): Longint; overload;
{$if btCharIsWide}
function MakeHash(const V: TbtString): Longint; overload;
{$ifend}

const
{ Script internal command: Assign command<br>
    Command: TPSCommand;<br>
    VarDest, // no data<br>
    VarSrc: TPSVariable;<br>
}
  CM_A = 0;
{ Script internal command: Calculate Command<br>
    Command: TPSCommand; <br>
    CalcType: Byte;<br>
    <i><br>
      0 = +<br>
      1 = -<br>
      2 = *<br>
      3 = /<br>
      4 = MOD<br>
      5 = SHL<br>
      6 = SHR<br>
      7 = AND<br>
      8 = OR<br>
      9 = XOR<br>
    </i><br>
    VarDest, // no data<br>
    VarSrc: TPSVariable;<br>
<br>
}
  CM_CA = 1;
{ Script internal command: Push<br>
    Command: TPSCommand; <br>
    Var: TPSVariable;<br>
}
  CM_P = 2;
{ Script internal command: Push Var<br>
    Command: TPSCommand; <br>
    Var: TPSVariable;<br>
}
  CM_PV = 3;
{ Script internal command: Pop<br>
    Command: TPSCommand; <br>
}
  CM_PO = 4;
{ Script internal command: Call<br>
    Command: TPSCommand; <br>
    ProcNo: Longword;<br>
}
  Cm_C = 5;
{ Script internal command: Goto<br>
    Command: TPSCommand; <br>
    NewPosition: Longint; //relative to end of this instruction<br>
}
  Cm_G = 6;
{ Script internal command: Conditional Goto<br>
    Command: TPSCommand; <br>
    NewPosition: LongWord; //relative to end of this instruction<br>
    Var: TPSVariable; // no data<br>
}
  Cm_CG = 7;
{ Script internal command: Conditional NOT Goto<br>
    Command: TPSCommand; <br>
    NewPosition: LongWord; // relative to end of this instruction<br>
    Var: TPSVariable; // no data<br>
}
  Cm_CNG = 8;
{ Script internal command: Ret<br>
    Command: TPSCommand; <br>
}
  Cm_R = 9;
{ Script internal command: Set Stack Type<br>
    Command: TPSCommand; <br>
    NewType: LongWord;<br>
    OffsetFromBase: LongWord;<br>
}
  Cm_ST = 10;
{ Script internal command: Push Type<br>
    Command: TPSCommand; <br>
    FType: LongWord;<br>
}
  Cm_Pt = 11;
{ Script internal command: Compare<br>
    Command: TPSCommand; <br>
    CompareType: Byte;<br>
    <i><br>
     0 = &gt;=<br>
     1 = &lt;=<br>
     2 = &gt;<br>
     3 = &lt;<br>
     4 = &lt;&gt<br>
     5 = =<br>
    <i><br>
    IntoVar: TPSAssignment;<br>
    Compare1, Compare2: TPSAssigment;<br>
}
  CM_CO = 12;
{ Script internal command: Call Var<br>
    Command: TPSCommand; <br>
    Var: TPSVariable;<br>
}
  Cm_cv = 13;
{ Script internal command: Set Pointer<br>
    Command: TPSCommand; <br>
    VarDest: TPSVariable;<br>
    VarSrc: TPSVariable;<br>
}
  cm_sp = 14;
{ Script internal command: Boolean NOT<br>
    Command: TPSCommand; <br>
    Var: TPSVariable;<br>
}
  cm_bn = 15;
{ Script internal command: Var Minus<br>
    Command: TPSCommand; <br>
    Var: TPSVariable;
}
  cm_vm = 16;
{ Script internal command: Set Flag<br>
    Command: TPSCommand; <br>
    Var: TPSVariable;<br>
    DoNot: Boolean;<br>
}
  cm_sf = 17;
{ Script internal command: Flag Goto<br>
    Command: TPSCommand; <br>
    Where: Cardinal;<br>
}
  cm_fg = 18;
{ Script internal command: Push Exception Handler<br>
    Command: TPSCommand; <br>
    FinallyOffset,<br>
    ExceptionOffset, // FinallyOffset or ExceptionOffset need to be set.<br>
    Finally2Offset,<br>
    EndOfBlock: Cardinal;<br>
}
  cm_puexh = 19;
{ Script internal command: Pop Exception Handler<br>
    Command:TPSCommand; <br>
    Position: Byte;<br>
    <i> 0 = end of try/finally/exception block;<br>
      1 = end of first finally<br>
      2 = end of except<br>
      3 = end of second finally<br>
    </i><br>
}
  cm_poexh = 20;
{ Script internal command: Integer NOT<br>
    Command: TPSCommand; <br>
    Where: Cardinal;<br>
}
  cm_in = 21;
  {Script internal command: Set Stack Pointer To Copy<br>
      Command: TPSCommand; <br>
    Where: Cardinal;<br>
}
  cm_spc = 22;
  {Script internal command: Inc<br>
    Command: TPSCommand; <br>
    Var: TPSVariable;<br>
  }
  cm_inc = 23;
  {Script internal command: Dec<br>
      Command: TPSCommand; <br>
    Var: TPSVariable;<br>
  }
  cm_dec = 24;
  {Script internal command: nop<br>
      Command: TPSCommand; <br>}
  cm_nop = 255;
{ Script internal command: Pop and Goto<br>
    Command: TPSCommand; <br>
    NewPosition: Longint; //relative to end of this instruction<br>
}
  Cm_PG = 25;
{ Script internal command: Pop*2 and Goto<br>
    Command: TPSCommand; <br>
    NewPosition: Longint; //relative to end of this instruction<br>
}
  Cm_P2G = 26;

type
  TbtU8 = Byte;
  TbtS8 = ShortInt;
  TbtU16 = Word;
  TbtS16 = SmallInt;
  TbtU32 = Cardinal;
  TbtS32 = Longint;
  TbtSingle = Single;
  TbtDouble = Double;
  TbtExtended = Extended;
  TbtCurrency = Currency;
{$IFNDEF PS_NOINT64}
  TbtS64 = Int64;
  {+} // TODO: add this type of processing
  {$if declared(UInt64)}
  TbtU64 = UInt64;
  {$else}
  TbtU64 = Int64;
  {$ifend}
  {+.}
{$ENDIF}
  {$if not declared(ShortString)}
  ShortString = AnsiString;
  {$ifend}
{.$IFNDEF PS_NOWIDESTRING}
  TbtWideString = WideString;
  {$if not declared(UnicodeString)}
  {-IFNDEF UNICODE}
  UnicodeString = WideString;
  {-ENDIF}
  {$ifend}
  TbtUnicodeString = UnicodeString;
  TbtWideChar = WideChar;
  TbtNativeString = {$IFDEF UNICODE}tbtUnicodeString{$ELSE}tbtString{$ENDIF};
{.$ENDIF !PS_NOWIDESTRING}
{+}
{$IFDEF FPC}
  {$if not declared(NativeUInt)}
  NativeInt = PtrInt;
  NativeUInt = PtrUInt;
  {$ifend}
  IPointer = PtrUInt;
{$ELSE !FPC}
  {.$if not declared(NativeUInt)} // failed for Delphi 2007
  {$IFNDEF DELPHI16UP}
  NativeInt = Integer;
  NativeUInt = Cardinal;
  {$ENDIF}
  {.$ifend}
  {.$if not declared(PtrUInt)}
  PtrInt = NativeInt;
  PtrUInt = NativeUInt;
  {.$ifend}
  IPointer = NativeUInt;
{$ENDIF !FPC}
  PIPointer = ^IPointer;
  PNativeInt = ^NativeInt;
  PNativeUInt = ^NativeUInt;
{+.}
  TPSCallingConvention = (cdRegister, cdPascal, cdCDecl, cdStdCall, cdSafeCall);

const
  PointerSize = IPointer({$IFDEF CPU64}8{$ELSE}4{$ENDIF});
  PointerSize2 = IPointer(2*PointerSize);
  MaxListSize = MaxInt div 16;

type
  {+}
  EPSError = class(Exception);

  TPSHeader = packed record
    HDR: Cardinal;
    PSBuildNo: Cardinal;
    TypeCount: Cardinal;
    ProcCount: Cardinal;
    VarCount: Cardinal;
    MainProcNo: Cardinal;
    ImportTableSize: Cardinal;
  end;

  {TPSExportItem = packed record
    ProcNo: Cardinal;
    NameLength: Cardinal;
    DeclLength: Cardinal;
  end;}
  {+.}

  PPointerList = ^TPointerList;

  TPointerList = array[0..MaxListSize - 1] of Pointer;

  TPSList = class
  protected
    FData: PPointerList;
    FCapacity: Cardinal;
    FCount: Cardinal;
    FCheckCount: Cardinal;
  private
    function GetItem(Nr: Cardinal): Pointer;
    procedure SetItem(Nr: Cardinal; P: Pointer);
  public
    constructor Create;
    destructor Destroy; override;

    {$IFNDEF PS_NOSMARTLIST}
    procedure Recreate;
    {$ENDIF}
    function IndexOf(P: Pointer): Longint;
    function Add(P: Pointer): Longint;
    procedure AddBlock(List: PPointerList; Count: Longint);
    procedure Remove(P: Pointer);
    procedure Delete(Nr: Cardinal);
    procedure DeleteLast;
    procedure Clear; virtual;
    {+}
    procedure ClearAsObjects(bDestroying: Boolean = False);
    {+.}

    property Data: PPointerList read FData;
    property Count: Cardinal read FCount;
    property Items[nr: Cardinal]: Pointer read GetItem write SetItem; default;
  end;
  TIFList = TPSList;

  TPSStringList = class
  private
    List: TPSList;
    function GetItem(Nr: LongInt): TbtString;
    procedure SetItem(Nr: LongInt; const s: TbtString);
  public
    constructor Create;
    destructor Destroy; override;

    function Count: LongInt;
    procedure Add(const P: TbtString);
    procedure Delete(Nr: LongInt);
    procedure Clear;

    property Items[Nr: Longint]: TbtString read GetItem write SetItem; default;
  end;
  TIFStringList = TPSStringList;

  TPSUnitList = class;

  TPSUnit = class
  private
    fList     : TPSUnitList;
    fUnits    : TPSList;
    fUnitName : TbtString;
    procedure SetUnitName(const Value: TbtString);
  public
    constructor Create(List: TPSUnitList);
    destructor Destroy; override;

    procedure AddUses(const pUnitName: TbtString);
    function HasUses(pUnitName: TbtString): Boolean;

    {$WARNINGS OFF}
    property UnitName: TbtString read fUnitName write SetUnitName;
    {$WARNINGS ON}
  end;

  TPSUnitList = class
  private
    fList: TPSList;
    function Add: TPSUnit;
  public
    constructor Create;
    destructor Destroy; override;

    function GetUnit(UnitName: TbtString): TPSUnit;
  end;

type
  TPSPasToken = (
    CSTI_EOF,
    //
    CSTIINT_Comment,
    CSTIINT_WhiteSpace,
    //
    CSTI_Identifier,
    CSTI_SemiColon,
    CSTI_Comma,
    CSTI_Period,
    CSTI_Colon,
    CSTI_OpenRound,
    CSTI_CloseRound,
    CSTI_OpenBlock,
    CSTI_CloseBlock,
    CSTI_Assignment,
    CSTI_Equal,
    CSTI_NotEqual,
    CSTI_Greater,
    CSTI_GreaterEqual,
    CSTI_Less,
    CSTI_LessEqual,
    CSTI_Plus,
    CSTI_Minus,
    CSTI_Divide,
    CSTI_Multiply,
    CSTI_Integer,
    CSTI_Real,
    CSTI_String,
    CSTI_Char,
    CSTI_HexInt,
    CSTI_AddressOf,
    CSTI_Dereference,
    CSTI_TwoDots,
    //
    CSTII_and,
    CSTII_array,
    CSTII_begin,
    CSTII_case,
    CSTII_const,
    CSTII_div,
    CSTII_do,
    CSTII_downto,
    CSTII_else,
    CSTII_end,
    CSTII_for,
    CSTII_function,
    CSTII_if,
    CSTII_in,
    CSTII_mod,
    CSTII_not,
    CSTII_of,
    CSTII_or,
    CSTII_procedure,
    CSTII_program,
    CSTII_repeat,
    CSTII_record,
    CSTII_set,
    CSTII_shl,
    CSTII_shr,
    CSTII_then,
    CSTII_to,
    CSTII_type,
    CSTII_until,
    CSTII_uses,
    CSTII_var,
    CSTII_while,
    CSTII_with,
    CSTII_xor,
    CSTII_exit,
    CSTII_class,
    CSTII_constructor,
    CSTII_destructor,
    CSTII_inherited,
    CSTII_private,
    CSTII_public,
    CSTII_published,
    CSTII_protected,
    CSTII_property,
    CSTII_virtual,
    CSTII_override,
  //CSTII_default,         //Birb
    CSTII_As,
    CSTII_Is,
    CSTII_Unit,
    CSTII_Try,
    CSTII_Except,
    CSTII_Finally,
    CSTII_External,
    CSTII_Forward,
    CSTII_Export,
    CSTII_Label,
    CSTII_Goto,
    CSTII_Chr,
    CSTII_Ord,
    CSTII_Interface,
    CSTII_Implementation,
    CSTII_initialization,  //* Nvds
    CSTII_finalization,    //* Nvds
    CSTII_out,
    CSTII_nil
    );

  TPSParserErrorKind = (
    iNoError,
    iCommentError,
    iStringError,
    iCharError,
    iSyntaxError
  );
  TPSParserErrorEvent = procedure (Parser: TObject; Kind: TPSParserErrorKind) of object;

  {+}
  TPSPascalParserStateLevel = (pslMini, pslFull);
  TPSPascalParserStateData = record
    FLevel: TPSPascalParserStateLevel;
    //
    FLastEnterPos, FRow, FRealPosition, FTokenLength: Cardinal;
    FTokenId: TPSPasToken;
    FToken: TbtString;
    FOriginalToken: TbtString;
  end;
  {+.}

  TPSPascalParser = class
  protected
    FData: TbtString;
    FText: PTbtChar;
    FLastEnterPos, FRow, FRealPosition, FTokenLength: Cardinal;
    FTokenId: TPSPasToken;
    FToken: TbtString;
    FOriginalToken: TbtString;
    FParserError: TPSParserErrorEvent;
    FEnableComments: Boolean;
    FEnableWhitespaces: Boolean;
    function GetCol: Cardinal;
    // only applicable when Token in [CSTI_Identifier, CSTI_Integer, CSTI_Real, CSTI_String, CSTI_Char, CSTI_HexInt]
  public
    {+}
    procedure StateSave(out AState: TPSPascalParserStateData; ALevel: TPSPascalParserStateLevel);
    procedure StateRestore(var AState: TPSPascalParserStateData);
    {+.}
    procedure SetText(const Data: TbtString); virtual;

    property EnableComments: Boolean read FEnableComments write FEnableComments;
    property EnableWhitespaces: Boolean read FEnableWhitespaces write FEnableWhitespaces;
    procedure Next; virtual;
    property GetToken: TbtString read FToken;
    property OriginalToken: TbtString read FOriginalToken;
    property CurrTokenPos: Cardinal read FRealPosition;
    property CurrTokenID: TPSPasToken read FTokenId;
    property Row: Cardinal read FRow;
    property Col: Cardinal read GetCol;
    property OnParserError: TPSParserErrorEvent read FParserError write FParserError;
  end;

function FloatToStr(E: Extended): TbtString; {$ifdef INLINE_SUPPORT} inline; {$endif}
function FastLowerCase(const s: TbtString): TbtString;
function Fw(const S: TbtString): TbtString;
function IntToStr(I: LongInt): TbtString;
{$if not declared(UIntToStr)}
function UIntToStr(I: Cardinal): string; {$define _UIntToStr_}
{$ifend}
function StrToIntDef(const S: TbtString; Def: LongInt): LongInt;
function StrToInt(const S: TbtString): LongInt;
function StrToFloat(const s: TbtString): Extended;

function FastUpperCase(const S: TbtString): TbtString; {$ifdef _inline_}{$if btCharIsWide} inline;{$ifend}{$endif}

function GRFW(var s: TbtString): TbtString;
function GRLW(var s: TbtString): TbtString;

{+}
function PointerShift(Ptr: Pointer; Offset: NativeInt): Pointer;
  {$ifdef INLINE_SUPPORT} inline; {$endif}
  overload;
function PointerShift(Ptr: Pointer; Offset: Pointer): Pointer;
  {$ifdef INLINE_SUPPORT} inline; {$endif}
  overload;

function string_starts_with(const S, Look: tbtString): Boolean;
  //{$ifdef INLINE_SUPPORT} inline; {$endif}
  overload;
{$IFDEF UNICODE}
function string_starts_with(const S, Look: string): Boolean;
  //{$ifdef INLINE_SUPPORT} inline; {$endif}
  overload;
{$ENDIF UNICODE}//*)
{+.}

const
  FCapacityInc = 32;
  {$IFNDEF PS_NOSMARTLIST}
  FMaxCheckCount = (FCapacityInc div 4) * 64;
  {$ENDIF}

{$IFNDEF DELPHI5UP}{$IFNDEF FPC}
function WideUpperCase(const S: WideString): WideString;
function WideLowerCase(const S: WideString): WideString;
{$ENDIF}{$ENDIF !DELPHI5UP}

// function Pos(const SubStr, Str: AnyString): Integer;
type
  TPosS = function (const SubStr, Str: string): Integer;
  TPosA = function (const SubStr, Str: AnsiString): Integer;
  TPosW = function (const SubStr, Str: WideString): Integer;
  {$IFDEF UNICODE}
  TPosU = function (const SubStr, Str: UnicodeString): Integer;
  {$ELSE}
  TPosU = TPosW;
  {$ENDIF}
var
  PosS: TPosS;
  PosA: TPosA;
  PosW: TPosW;
  PosU: TPosU;

// function PosEx(const SubStr, Str: AnyString; Offset: Integer = 1): Integer;
type
  TPosExS = function(const SubStr, S: string; Offset: Integer = 1): Integer;
  TPosExA = function(const SubStr, S: AnsiString; Offset: Integer = 1): Integer;
  TPosExW = function(const SubStr, S: WideString; Offset: Integer = 1): Integer;
  {$IFDEF UNICODE}
  TPosExU = function(const SubStr, S: UnicodeString; Offset: Integer = 1): Integer;
  {$ELSE}
  TPosExU = TPosExW;
  {$ENDIF}
var
  PosExS: TPosExS;
  PosExA: TPosExA;
  PosExW: TPosExW;
  PosExU: TPosExU;

function StrLenA(p: PAnsiChar): Longint;
//function StrLenW(P: PWideChar): Longint;

function TrimLenS(const S: string): Integer;
function TrimLeftLenS(const S: string): Integer;
function TrimRightLenS(const S: string): Integer;

function TrimLenA(const S: AnsiString): Integer;
function TrimLeftLenA(const S: AnsiString): Integer;
function TrimRightLenA(const S: AnsiString): Integer;

function TrimLenW(const S: WideString): Integer;
function TrimLeftLenW(const S: WideString): Integer;
function TrimRightLenW(const S: WideString): Integer;

{$IFDEF UNICODE}
function TrimLenU(const S: UnicodeString): Integer;
function TrimLeftLenU(const S: UnicodeString): Integer;
function TrimRightLenU(const S: UnicodeString): Integer;
{$ENDIF UNICODE}

function SameTextW(const s1,s2: WideString): Boolean;
  {$ifdef INLINE_SUPPORT} inline; {$endif}
function AnsiSameTextW(const s1,s2: WideString): Boolean;
  {$ifdef INLINE_SUPPORT} inline; {$endif}
{$IFDEF UNICODE}
function SameTextU(const s1,s2: UnicodeString): Boolean;
  {$ifdef INLINE_SUPPORT} inline; {$endif}
function AnsiSameTextU(const s1,s2: UnicodeString): Boolean;
  {$ifdef INLINE_SUPPORT} inline; {$endif}
{$ENDIF UNICODE}

{$IFDEF FPC}
  {$IFNDEF PS_NOWIDESTRING}
function StringOfChar(Ch: WideChar; Count: Integer): UnicodeString; overload;
  {$ENDIF !PS_NOWIDESTRING}
{$ENDIF FPC}

function BoolToTxt(B: Boolean): string;

// for comfortly debug
type TPSDebugMessage = procedure(const S: string);
var  dbg: TPSDebugMessage;
procedure psdbg_null(const S: string);
procedure psdbg(const S: string);

implementation

uses
  TypInfo
  {$IF DEFINED(DELPHI11UP) or DEFINED(FPC)} // TODO: FPC need check
  {$DEFINE _STRUTILS_}
  ,StrUtils
  {$IFEND}
  {$IF (NOT DEFINED (NEXTGEN)) AND (NOT DEFINED (MACOS)) AND DEFINED(DELPHI17UP)} // DELPHI18UP == DELPHIXE3UP
  ,AnsiStrings {$DEFINE _ANSISTRINGS_}
  {$IFEND}
  ;

{$if (defined(DELPHI3UP) or defined(FPC))}
resourcestring
{$else}
const
{$ifend}
  RPS_InvalidFloat = 'Invalid float';

{$IFNDEF DELPHI5UP}{$IFNDEF FPC}
function WideUpperCase(const S: WideString): WideString;
var
  Len: Integer;
begin
  // CharUpperBuffW is stubbed out on Win9x platofmrs
  if Win32Platform = VER_PLATFORM_WIN32_NT then
  begin
    Len := Length(S);
    SetString(Result, PWideChar(S), Len);
    if Len > 0 then CharUpperBuffW(Pointer(Result), Len);
  end
  else
    Result := AnsiUpperCase(S);
end;

function WideLowerCase(const S: WideString): WideString;
var
  Len: Integer;
begin
  // CharLowerBuffW is stubbed out on Win9x platofmrs
  if Win32Platform = VER_PLATFORM_WIN32_NT then
  begin
    Len := Length(S);
    SetString(Result, PWideChar(S), Len);
    if Len > 0 then CharLowerBuffW(Pointer(Result), Len);
  end
  else
    Result := AnsiLowerCase(S);
end;
{$ENDIF}{$ENDIF !DELPHI5UP}

{$define _SysPosEx_}
{$if not declared(PosEx)}
{$undef _SysPosEx_}
function PosEx(const SubStr, S: string; Offset: Integer = 1): Integer; // {$ifdef _inline_}inline;{$endif}
var
  i, X: Integer;
  Len, LenSubStr: Integer;
begin
  if Offset = 1 then
    Result := Pos(SubStr, S)
  else
  begin
    i := Offset;
    LenSubStr := Length(SubStr);
    Len := Length(S) - LenSubStr + 1;
    while i <= Len do
    begin
      if S[i] = SubStr[1] then
      begin
        X := 1;
        while (X < LenSubStr) and (S[i + X] = SubStr[X + 1]) do
          Inc(X);
        if (X = LenSubStr) then
        begin
          Result := i;
          Exit;
        end;
      end;
      Inc(i);
    end;
    Result := 0;
  end;
end;
{$ifend}

function PosExS_(const SubStr, S: string; Offset: Cardinal = 1): Integer;
begin
  Result := PosEx(SubStr, S, Offset);
end;

{$if defined(_ANSISTRINGS_) or ((not defined(UNICODE)) and defined(_SysPosEx_))}
function PosExA_(const SubStr, S: AnsiString; Offset: Integer = 1): Integer;
begin
  Result :=
    {$IFDEF _ANSISTRINGS_}
    AnsiStrings.PosEx
    {$ELSE}
    PosEx
    {$ENDIF}
      (SubStr, S, Offset);
end;
{$else}
function PosExA_(const SubStr, S: AnsiString; Offset: Integer = 1): Integer;
var
  i, X: Integer;
  Len, LenSubStr: Integer;
begin
  if Offset = 1 then
    Result := Pos(SubStr, S)
  else
  begin
    i := Offset;
    LenSubStr := Length(SubStr);
    Len := Length(S) - LenSubStr + 1;
    while i <= Len do
    begin
      if S[i] = SubStr[1] then
      begin
        X := 1;
        while (X < LenSubStr) and (S[i + X] = SubStr[X + 1]) do
          Inc(X);
        if (X = LenSubStr) then
        begin
          Result := i;
          Exit;
        end;
      end;
      Inc(i);
    end;
    Result := 0;
  end;
end;
{$ifend} // if defined(_ANSISTRINGS_) or ((not defined(UNICODE)) and defined(_SysPosEx_))}

{$if (defined(UNICODE) and defined(_SysPosEx_))}
function PosExW_(const SubStr, S: WideString; Offset: Integer = 1): Integer;
begin
  Result := PosEx(SubStr, S, Offset);
end;
{$else}
function PosExW_(const SubStr, S: WideString; Offset: Integer = 1): Integer;
var
  i, X: Integer;
  Len, LenSubStr: Integer;
begin
  if Offset = 1 then
    Result := Pos(SubStr, S)
  else
  begin
    i := Offset;
    LenSubStr := Length(SubStr);
    Len := Length(S) - LenSubStr + 1;
    while i <= Len do
    begin
      if S[i] = SubStr[1] then
      begin
        X := 1;
        while (X < LenSubStr) and (S[i + X] = SubStr[X + 1]) do
          Inc(X);
        if (X = LenSubStr) then
        begin
          Result := i;
          Exit;
        end;
      end;
      Inc(i);
    end;
    Result := 0;
  end;
end;
{$ifend} // (defined(UNICODE) and defined(_SysPosEx_))

{$if (defined(UNICODE) and defined(_SysPosEx_))}
function PosExU_(const SubStr, S: UnicodeString; Offset: Integer = 1): Integer;
begin
  Result := PosEx(SubStr, S, Offset);
end;
{$else}
function PosExU_(const SubStr, S: WideString; Offset: Integer = 1): Integer;
var
  i, X: Integer;
  Len, LenSubStr: Integer;
begin
  if Offset = 1 then
    Result := Pos(SubStr, S)
  else
  begin
    i := Offset;
    LenSubStr := Length(SubStr);
    Len := Length(S) - LenSubStr + 1;
    while i <= Len do
    begin
      if S[i] = SubStr[1] then
      begin
        X := 1;
        while (X < LenSubStr) and (S[i + X] = SubStr[X + 1]) do
          Inc(X);
        if (X = LenSubStr) then
        begin
          Result := i;
          Exit;
        end;
      end;
      Inc(i);
    end;
    Result := 0;
  end;
end;
{$ifend} // $if (defined(UNICODE) and defined(_SysPosEx_))

function PosS_(const SubStr, Str: string): Integer;
begin
  Result := Pos(SubStr, Str);
end;

function PosA_(const SubStr, Str: AnsiString): Integer;
begin
  {$IFDEF _ANSISTRINGS_}
  Result := AnsiStrings.AnsiPos(SubStr, Str);
  {$ELSE}
  Result := Pos(SubStr, Str);
  {$ENDIF}
end;

function PosW_(const SubStr, Str: WideString): Integer;
begin
  Result := Pos(SubStr, Str);
end;

{$IFDEF UNICODE}
function PosU_(const SubStr, Str: UnicodeString): Integer;
begin
  Result := Pos(SubStr, Str);
end;
{$ENDIF}

function StrLenA(p: PAnsiChar): Longint;
begin
  Result :=
   //{$IF (NOT DEFINED (NEXTGEN)) AND (NOT DEFINED (MACOS)) AND DEFINED (DELPHI18UP)}
   {$IF DEFINED(_ANSISTRINGS_) AND DEFINED (DELPHI18UP)} // DELPHI18UP == DELPHIXE4UP
   AnsiStrings.StrLen(p)
   {$ELSE}
     {$IFDEF NEXTGEN}
   Length(p)
     {$ELSE}
   StrLen(p)
     {$ENDIF}
   //{$IFEND}
   {$IFEND}
end;

{function StrLenW(P: PWideChar): Longint;
begin
  Result := 0;
  if P <> nil then
    while P[Result] <> #0 do
      Inc(Result);
end;}

function TrimLenS(const S: string): Integer;
var
  I, L: Integer;
begin
  L := Length(S);
  I := 1;
  while (I <= L) and (S[I] <= #32) do
    Inc(I);
  if I > L then
    Result := 0
  else
  begin
    while S[L] <= #32 do
      Dec(L);
    Result := L - I + 1;
  end;
end;

function TrimLeftLenS(const S: string): Integer;
var
  I, L: Integer;
begin
  L := Length(S);
  I := 1;
  while (I <= L) and (S[I] <= #32) do
    Inc(I);
  Result := L - I + 1;
end;

function TrimRightLenS(const S: string): Integer;
var
  I: Integer;
begin
  I := Length(S);
  while (I > 0) and (S[I] <= #32) do
    Dec(I);
  Result := I;
end;

function TrimLenA(const S: AnsiString): Integer;
var
  I, L: Integer;
begin
  L := Length(S);
  I := 1;
  while (I <= L) and (S[I] <= #32) do
    Inc(I);
  if I > L then
    Result := 0
  else
  begin
    while S[L] <= #32 do
      Dec(L);
    Result := L - I + 1;
  end;
end;

function TrimLeftLenA(const S: AnsiString): Integer;
var
  I, L: Integer;
begin
  L := Length(S);
  I := 1;
  while (I <= L) and (S[I] <= #32) do
    Inc(I);
  Result := L - I + 1;
end;

function TrimRightLenA(const S: AnsiString): Integer;
var
  I: Integer;
begin
  I := Length(S);
  while (I > 0) and (S[I] <= #32) do
    Dec(I);
  Result := I;
end;

function TrimLenW(const S: WideString): Integer;
var
  I, L: Integer;
begin
  L := Length(S);
  I := 1;
  while (I <= L) and (S[I] <= WideChar(#32)) do
    Inc(I);
  if I > L then
    Result := 0
  else
  begin
    while S[L] <= WideChar(#32) do
      Dec(L);
    Result := L - I + 1;
  end;
end;

function TrimLeftLenW(const S: WideString): Integer;
var
  I, L: Integer;
begin
  L := Length(S);
  I := 1;
  while (I <= L) and (S[I] <= WideChar(#32)) do
    Inc(I);
  Result := L - I + 1;
end;

function TrimRightLenW(const S: WideString): Integer;
var
  I: Integer;
begin
  I := Length(S);
  while (I > 0) and (S[I] <= WideChar(#32)) do
    Dec(I);
  Result := I;
end;

{$IFDEF UNICODE}
function TrimLenU(const S: UnicodeString): Integer;
var
  I, L: Integer;
begin
  L := Length(S);
  I := 1;
  while (I <= L) and (S[I] <= WideChar(#32)) do
    Inc(I);
  if I > L then
    Result := 0
  else
  begin
    while S[L] <= WideChar(#32) do
      Dec(L);
    Result := L - I + 1;
  end;
end;

function TrimLeftLenU(const S: UnicodeString): Integer;
var
  I, L: Integer;
begin
  L := Length(S);
  I := 1;
  while (I <= L) and (S[I] <= WideChar(#32)) do
    Inc(I);
  Result := L - I + 1;
end;

function TrimRightLenU(const S: UnicodeString): Integer;
var
  I: Integer;
begin
  I := Length(S);
  while (I > 0) and (S[I] <= WideChar(#32)) do
    Dec(I);
  Result := I;
end;
{$ENDIF UNICODE}

procedure PosInit();
begin
  //
  // POS:
  //

  PosS := @PosS_;

  //-PosA := @PosA_;
       // or:
  {$IFDEF _ANSISTRINGS_}
  PosA := @AnsiStrings.AnsiPos;
  {$ELSE}
  //{$IFNDEF UNICODE}
  //PosA := {$IFDEF FPC}@{$ENDIF}Pos; // XE3 FOR "@": Fatal: F2084 Internal Error: AV00000000-R00000000-0
  //{$ELSE}
  PosA := @PosA_;
  //{$ENDIF}
  {$ENDIF}

  PosW := @PosW_;

  {$IFDEF UNICODE}
  PosU := @PosU_;
  {$ELSE}
  PosU := @PosW_;
  {$ENDIF}

  //
  // POSEX:
  //

  //-PosExS := @PosExS_;
         // or:
  PosExS := {$IFDEF FPC}@{$ENDIF}PosEx; // XE3 FOR "@": Fatal: F2084 Internal Error: AV00000000-R00000000-0

  {$IFDEF _ANSISTRINGS_}
  PosExA := @AnsiStrings.PosEx;
  {$ELSE}
  PosExA := @PosExA_;
  {$ENDIF}

  {$IFDEF UNICODE}
  PosExW := @PosExW_;
  {$ELSE}
  PosExW := @PosExW_;
  {$ENDIF}

  {$IFDEF UNICODE}
  PosExU := @PosExU_;
  {$ELSE}
  PosExU := @PosExW_;
  {$ENDIF}
end;

function SameTextW(const s1,s2: WideString): Boolean;
{$IFDEF FPC}{$push} // FPC: https://wiki.freepascal.org/Turn_warnings_and_hints_on_or_off
  {$warn 4105 off}  // FPC: Warning: Implicit string type conversion with potential data loss from "WideString" to "AnsiString"
{$ENDIF}
begin
  Result := SameText(s1, s2);
end;{$IFDEF FPC}{$pop}{$ENDIF}

function AnsiSameTextW(const s1,s2: WideString): Boolean;
{$IFDEF FPC}{$push} {$warn 4105 off} {$ENDIF}
begin
  Result := AnsiSameText(s1, s2);
end;{$IFDEF FPC}{$pop}{$ENDIF}

{$IFDEF UNICODE}
function SameTextU(const s1,s2: UnicodeString): Boolean;
{$IFDEF FPC}{$push} {$warn 4105 off} {$ENDIF}
begin
  Result := SameText(s1, s2);
end;{$IFDEF FPC}{$pop}{$ENDIF}

function AnsiSameTextU(const s1,s2: UnicodeString): Boolean;
{$IFDEF FPC}{$push} {$warn 4105 off} {$ENDIF}
begin
  Result := AnsiSameText(s1, s2);
end;{$IFDEF FPC}{$pop}{$ENDIF}
{$ENDIF UNICODE}

{$IFDEF FPC}
  {$IFNDEF PS_NOWIDESTRING}
function StringOfChar(Ch: WideChar; Count: Integer): UnicodeString; overload;
var
  R: PWideChar; // Result buffer pointer
  S: PWideChar; // Source buffer pointer
  C: Integer;   // Source chars count
  D: Integer;   // Result buffer count dupes (of AText)
begin
  if Count < 0  then
    Count := 0;
  C := Count;
  {$IFDEF FPC}{$push}
    {$warn 5094 off}  // FPC: Hint: Function result variable of a managed type does not seem to be initialized
  {$ENDIF}
  //Result := '';
  SetLength(Result, Count);
  {$IFDEF FPC}{$pop}{$ENDIF}
  if Count = 0  then
    Exit;
  R := PWideChar(Result);
  R^ := Ch;
  Dec(Count);
  if Count = 0 then
    Exit;
  S := R;
  Inc(R, C);
  D := 1;
  while Count > 0 do
  begin
    Move(S^, R^, C * SizeOf(WideChar));
    Inc(R, C);
    Dec(Count, D);
    Inc(D, D);
    if D < Count then
      Inc(C, C)
    else if Count > 0 then
      C := Count;
  end;
end;
  {$ENDIF !PS_NOWIDESTRING}
{$ENDIF FPC}

function BoolToTxt(B: Boolean): string;
begin
  if B
  then Result := 'True'
  else Result := 'False';
end;

procedure psdbg_null(const S: string);
begin
  { empry }
end;

procedure psdbg(const S: string);
begin
  {$IFDEF MSWINDOWS}
  begin
    OutputDebugString(PChar('rps:> ' + S));
    begin
      {$IFDEF _DCC_MSG_}
      if IsConsole then
        writeln('rps:> ', S);
      {$ENDIF}
    end;
  end;
  {$ELSE !MSWINDOWS}
  begin
    //writeln(stderr, S); //?
  end;
  {$ENDIF !MSWINDOWS}
end;

{ TPSTypeRecBase }

function TPSTypeRecBase.BaseTypeToStr: string;
//var i: Longint;
begin
  case FBaseType of
    btReturnAddress       : Result := 'ReturnAddress';
    btU8                  : Result := 'U8';
    btS8                  : Result := 'S8';
    btU16                 : Result := 'U16';
    btS16                 : Result := 'S16';
    btU32                 : Result := 'U32';
    btS32                 : Result := 'S32';
    btSingle              : Result := 'Single';
    btDouble              : Result := 'Double';
    btExtended            : Result := 'Extended';
    btString              : Result := {$if btCharIsWide}'UnicodeString'{$else}'AnsiString'{$ifend};
    btRecord              : begin
                              Result := 'Record';
                              {i := TPSTypeRec_Record(Self).FieldTypes.Count;
                              if (i > 0) then begin
                                Result := Result+'('
                                  + BaseTypeToStr(PIFTypeRecBase(TPSTypeRec_Record(Self).FieldTypes[0]));
                                for i := 1 to i-1 do
                                  Result := Result+','
                                    + BaseTypeToStr(PIFTypeRecBase(TPSTypeRec_Record(Self).FieldTypes[i]));
                                Result := Result + ')';
                              end;}
                            end;
    btArray               : Result := 'Array';//+' of '+BaseTypeToStr(TPSTypeRec_Array(Self).ArrayType);
    btPointer             : Result := 'Pointer';
    btPChar               : Result := {$if btCharIsWide}'PWideChar'{$else}'PAnsiChar'{$ifend};
    btResourcePointer     : Result := 'ResourcePointer';
    btVariant             : Result := 'Variant';
  {$IFNDEF PS_NOINT64}
    btS64                 : Result := 'S64';
    {+}
  //btU64                 : Result := 'U64';
    {+.}
  {$ENDIF}
    btChar                : Result := {$if btCharIsWide}'WideChar'{$else}'AnsiChar'{$ifend};
  {$IFNDEF PS_NOWIDESTRING}
    btWideString          : Result := 'WideString';
    btWideChar            : Result := 'WideChar';
  {$ELSE}
    {$IFDEF UNICODE}
    btWideChar            : Result := 'WideChar';
    {$ENDIF}
  {$ENDIF}
    btProcPtr             : Result := 'ProcPtr';
    btStaticArray         : Result := 'StaticArray'
                              //+ '['+IntToStr(TPSTypeRec_StaticArray(Self).Size)
                              //+ '] of '+BaseTypeToStr(TPSTypeRec_Array(Self).ArrayType)
                              ;
    btSet                 : Result := 'Set';
    btCurrency            : Result := 'Currency';
    btClass               : Result := 'Class'
                              //+ ': '+string(TPSTypeRec_Class(Self).CN)
                              ;
    btInterface           : Result := 'Interface';
    btNotificationVariant : Result := 'NotificationVariant';
    btUnicodeString       : Result := 'UnicodeString';
    {+}
    btPWideChar           : Result := 'PWideChar';
    {$EXTERNALSYM btPWideChar}
    {+.}
    btType                : Result := 'Type';
    btEnum                : Result := 'Enum';
    btExtClass            : Result := 'ExtClass';
    else                    Result := 'Unknown '+SysUtils.IntToStr(FBaseType);
  end; // case
end;

function PSBaseTypeToStr(BaseType: TPSBaseType): string;
begin
  case BaseType of
    btReturnAddress       : Result := 'ReturnAddress';
    btU8                  : Result := 'U8';
    btS8                  : Result := 'S8';
    btU16                 : Result := 'U16';
    btS16                 : Result := 'S16';
    btU32                 : Result := 'U32';
    btS32                 : Result := 'S32';
    btSingle              : Result := 'Single';
    btDouble              : Result := 'Double';
    btExtended            : Result := 'Extended';
    btString              : Result := {$if btCharIsWide}'UnicodeString'{$else}'AnsiString'{$ifend};
    btRecord              : Result := 'Record';
    btArray               : Result := 'Array';
    btPointer             : Result := 'Pointer';
    btPChar               : Result := {$if btCharIsWide}'PWideChar'{$else}'PAnsiChar'{$ifend};
    btResourcePointer     : Result := 'ResourcePointer';
    btVariant             : Result := 'Variant';
  {$IFNDEF PS_NOINT64}
    btS64                 : Result := 'S64';
    {+}
  //btU64                 : Result := 'U64';
    {+.}
  {$ENDIF}
    btChar                : Result := {$if btCharIsWide}'WideChar'{$else}'AnsiChar'{$ifend};
  {$IFNDEF PS_NOWIDESTRING}
    btWideString          : Result := 'WideString';
    btWideChar            : Result := 'WideChar';
  {$ELSE}
    {$IFDEF UNICODE}
    btWideChar            : Result := 'WideChar';
    {$ENDIF}
  {$ENDIF}
    btProcPtr             : Result := 'ProcPtr';
    btStaticArray         : Result := 'StaticArray';
    btSet                 : Result := 'Set';
    btCurrency            : Result := 'Currency';
    btClass               : Result := 'Class';
    btInterface           : Result := 'Interface';
    btNotificationVariant : Result := 'NotificationVariant';
    btUnicodeString       : Result := 'UnicodeString';
    {+}
    btPWideChar           : Result := 'PWideChar';
    {$EXTERNALSYM btPWideChar}
    {+.}
    btType                : Result := 'Type';
    btEnum                : Result := 'Enum';
    btExtClass            : Result := 'ExtClass';
    else                    Result := 'Unknown '+SysUtils.IntToStr(BaseType);
  end; // case
end; // function BaseTypeToStr

function PSBaseTypeToStr(BaseType: PIFTypeRecBase): string;
begin
  Result := TPSTypeRecBase(BaseType).BaseTypeToStr();
end;

//
// Small hash maker:
//
function MakeHash(const S: ShortString): Longint;
var
  I: Integer;
begin
  Result := 0;
  for I := 1 to Length(S) do
    Result := ((Result shl 7) or (Result shr 25)) + Ord(S[I]);
end;

function MakeHash(const S: AnsiString): Longint;
var
  I: Integer;
begin
  Result := 0;
  for I := 1 to Length(S) do
    Result := ((Result shl 7) or (Result shr 25)) + Ord(S[I]);
end;

{$if btCharIsWide}
function MakeHash(const V: TbtString): Longint;
var
  I: Integer;
  S: AnsiString;
begin
  Result := 0;
  S := AnsiString(V);
  for I := 1 to Length(S) do
    Result := ((Result shl 7) or (Result shr 25)) + Ord(S[I]);
end;
{$ifend}

function GRFW(var s: TbtString): TbtString;
var l: Longint;
begin
  l := 1;
  while l <= Length(s) do begin
    if s[l] = ' ' then begin
      Result := copy(s, 1, l - 1);
      Delete(s, 1, l);
      Exit;
    end;
    l := l + 1;
  end;
  Result := s;
  s := '';
end;

function GRLW(var s: TbtString): TbtString;
var l: Longint;
begin
  l := Length(s);
  while l >= 1 do begin
    if s[l] = ' ' then begin
      Result := copy(s, l+1, MaxInt);
      Delete(s, l, MaxInt);
      Exit;
    end;
    Dec(l);
  end;
  Result := s;
  s := '';
end;

{+}
function PointerShift(Ptr: Pointer; Offset: NativeInt): Pointer; overload;
{$IFDEF FPC}{$push}
  {$warn 4055 off}  // FPC: Hint: Conversion between ordinals and pointers is not portable
  {$warn 4082 off}  // FPC: Warning: Converting pointers to signed integers may result in wrong comparison results and range errors, use an unsigned type instead.
{$ENDIF}
begin
  Result := Pointer(NativeUInt(NativeInt(Ptr) + Offset));
end; {$IFDEF FPC}{$pop}{$ENDIF}
// OR:
//var NPtr: NativeInt absolute Ptr; RPtr: NativeInt absolute Result;
//begin RPtr := NPtr + Offset; end;

function PointerShift(Ptr: Pointer; Offset: Pointer): Pointer; overload;
{$IFDEF FPC}{$push} {$warn 4055 off} {$warn 4082 off} {$ENDIF}
begin
  Result := Pointer(NativeUInt(NativeInt(Ptr) + NativeInt(Offset)));
end; {$IFDEF FPC}{$pop}{$ENDIF}

function string_starts_with(const S, Look: TbtString): Boolean; overload;
var len: integer; p1,p2: PTbtChar;
begin
  //Result := Copy(S, 1, Length(S)) = Look;

  len := Length(Look);

//[1]
  (*Result := ( Length(S) >= len )
    {$if btCharIsNative}
    and ( {$if btCharIsAnsi}{$IFDEF DELPHI18UP}AnsiStrings.{$ENDIF}Ansi{$ifend}StrLComp(PTbtChar(S), PTbtChar(Look), len) = 0 )
    {$else} // ansi
    and ( {$IFDEF DELPHI18UP}AnsiStrings.{$ENDIF}AnsiStrLComp(PAnsiChar(S), PAnsiChar(Look), len) = 0 )
    {$ifend}
  ;//*)

//[2] // best for small strings
  Result := ( Length(S) >= len );
  if Result then begin
    p1 := PTbtChar(S);
    p2 := PTbtChar(Look);
    while len > 0 do begin
      Result := p1^ = p2^;
      if not Result then
        Exit;
      Inc(p1);
      Inc(p2);
      Dec(len);
    end;
  end;{}
end;
{$IFDEF UNICODE}
function string_starts_with(const S, Look: string): Boolean; overload;
var len: integer; p1,p2: PChar;
begin
  len := Length(Look);

//[1]
  {Result := ( Length(S) >= len )
    and ( StrLComp(PChar(S), PChar(Look), len) = 0 );}

//[2] // best for small strings
  Result := ( Length(S) >= len );
  if Result then begin
    p1 := PChar(S);
    p2 := PChar(Look);
    while len > 0 do begin
      Result := p1^ = p2^;
      if not Result then
        Exit;
      Inc(p1);
      Inc(p2);
      Dec(len);
    end;
  end;
end;
{$ENDIF UNICODE}//*)
{+.}

function StrToFloat(const s: TbtString): Extended;
var E: Longint;
begin
  Val(string(s), Result, E);
  if E <> 0 then
    raise {SysUtils.}EConvertError.Create(RPS_InvalidFloat);
end;

function IntToStr(I: LongInt): TbtString;
var S: ShortString;
begin
  Str(i, S);
  Result := TbtString(S);
end;

{$ifdef _UIntToStr_}
function UIntToStr(I: Cardinal): string;
var S: ShortString;
begin
  Str(i, S);
  Result := string(S);
end;
{$endif}

function FloatToStr(E: Extended): TbtString;
{+}
begin
  Result := TbtString(SysUtils.FloatToStr(E));
end;
{var S: string;
begin
  Str(e:0:12, s);
  Result := s;
end;}
{+.}

function StrToInt(const S: TbtString): LongInt;
var
  e: Integer;
  Res: LongInt;
begin
  Val(string(S), Res, e);
  if e <> 0 then
    StrToInt := -1
  else
    StrToInt := Res;
end;

function StrToIntDef(const S: TbtString; Def: LongInt): LongInt;
var
  e: Integer;
  Res: LongInt;
begin
  Val(string(S), Res, e);
  if e <> 0 then
    StrToIntDef := Def
  else
    StrToIntDef := Res;
end;

constructor TPSList.Create;
begin
  inherited Create;
  FCount := 0;
  FCapacity := 16;
  {$IFNDEF PS_NOSMARTLIST}
  FCheckCount := 0;
  {$ENDIF}
  GetMem(FData, FCapacity * PointerSize);
end;

function MM(i1, i2: Integer): Integer;
begin
  if ((i1 div i2) * i2) < i1 then
    mm := (i1 div i2 + 1) * i2
  else
    mm := (i1 div i2) * i2;
end;

{$IFNDEF PS_NOSMARTLIST}
procedure TPSList.Recreate;
var
  NewData: PPointerList;
  NewCapacity: Cardinal;
  I: Longint;
begin
  FCheckCount := 0;
  NewCapacity := mm(FCount, FCapacityInc);
  if NewCapacity < 64 then NewCapacity := 64;
  GetMem(NewData, NewCapacity * PointerSize);
  for I := 0 to Longint(FCount)-1 do
    NewData^[i] := FData[I];
  FreeMem(FData, FCapacity * PointerSize);
  FData := NewData;
  FCapacity := NewCapacity;
end;
{$ENDIF}

function TPSList.Add(P: Pointer): Longint;
begin
  if FCount >= FCapacity then
  begin
    Inc(FCapacity, FCapacityInc);// := FCount + 1;
    ReAllocMem(FData, FCapacity * PointerSize);
  end;
  FData[FCount] := P; // Instead of SetItem
  Result := FCount;
  Inc(FCount);
  {$IFNDEF PS_NOSMARTLIST}
  Inc(FCheckCount);
  if FCheckCount > FMaxCheckCount then
    Recreate;
  {$ENDIF}
end;

procedure TPSList.AddBlock(List: PPointerList; Count: Longint);
var
  L: Longint;
begin
  if Longint(FCount) + Count > Longint(FCapacity) then
  begin
    {$hints off}
    Inc(FCapacity, mm(Count, FCapacityInc));
    {$hints off}
    ReAllocMem(FData, FCapacity *PointerSize);
  end;
  for L := 0 to Count-1 do
  begin
    FData[FCount] := List[L];
    Inc(FCount);
  end;
  {$IFNDEF PS_NOSMARTLIST}
  Inc(FCheckCount);
  if FCheckCount > FMaxCheckCount then
    Recreate;
  {$ENDIF}
end;

procedure TPSList.DeleteLast;
begin
  if FCount = 0 then Exit;
  Dec(FCount);
  {$IFNDEF PS_NOSMARTLIST}
  Inc(FCheckCount);
  if FCheckCount > FMaxCheckCount then
    Recreate;
  {$ENDIF}
end;

procedure TPSList.Delete(Nr: Cardinal);
begin
  if FCount = 0 then Exit;
  if Nr < FCount then
  begin
    {dec count first, so we move one element less in the move below}
    Dec(FCount);
    {only move if we aren't deleting the last element}
    if Nr < FCount then
      Move(FData[Nr + 1], FData[Nr], (FCount - Nr) * PointerSize);
    {$IFNDEF PS_NOSMARTLIST}
    Inc(FCheckCount);
    if FCheckCount > FMaxCheckCount then
      Recreate;
    {$ENDIF}
  end;
end;

procedure TPSList.Remove(P: Pointer);
var
  I: Cardinal;
begin
  if FCount = 0 then Exit;
  I := 0;
  while I < FCount do
  begin
    if FData[I] = P then
    begin
      Delete(I);
      Exit;
    end;
    Inc(I);
  end;
end;

procedure TPSList.Clear;
begin
  FCount := 0;
  {$IFNDEF PS_NOSMARTLIST}
  Recreate;
  {$ENDIF}
end;

{+}
procedure TPSList.ClearAsObjects(bDestroying: Boolean);
var
  O: TObject;
begin
  if FCount > 0 then begin
    while FCount > 0 do begin
      Dec(FCount);
      O := TObject(FData[FCount]);
      FData[FCount] := nil;
      O.Free;
    end;
  end;
  {$IFNDEF PS_NOSMARTLIST}
  if not bDestroying then
    Recreate;
  {$ENDIF}
end;
{+.}

destructor TPSList.Destroy;
begin
  if Assigned(FData) then begin
    FreeMem(FData, FCapacity * PointerSize);
    FData := nil;
  end;
  inherited;
end;

procedure TPSList.SetItem(Nr: Cardinal; P: Pointer);
begin
  if (FCount = 0) or (Nr >= FCount) then
    Exit;
  FData[Nr] := P;
end;

function TPSList.GetItem(Nr: Cardinal): Pointer;
begin
  if Nr < FCount then
    Result := FData[Nr]
  else
    Result := nil;
end;

{ TPSStringList }

function TPSStringList.Count: LongInt;
begin
  Result := List.Count;
end;

function TPSStringList.GetItem(Nr: LongInt): TbtString;
var
  S: PTbtString;
begin
  s := List.GetItem(Nr);
  if s = nil then
    Result := ''
  else
    Result := s^;
end;

procedure TPSStringList.SetItem(Nr: LongInt; const s: TbtString);
var
  p: PTbtString;
begin
  p := List.GetItem(Nr);
  if p = nil then
    Exit;
  p^ := s;
end;

procedure TPSStringList.Add(const P: TbtString);
var
  w: PTbtString;
begin
  New(w);
  w^ := P;
  List.Add(w);
end;

procedure TPSStringList.Delete(Nr: LongInt);
var
  W: PTbtString;
begin
  W := List.GetItem(Nr);
  if w <> nil then
    Dispose(w);
  List.Delete(Nr);
end;

procedure TPSStringList.Clear;
var
  W: PTbtString;
  i: Integer;
begin
  if Assigned(List) and (List.Count > 0) then
  begin
    for i := List.Count-1 downto 0 do
    begin
      W := List.FData[i]; // == List.GetItem(i);
      if W <> nil then
      begin
        List.FData[i] := nil;
        Dispose(W);
      end;
    end;
    List.Clear;
  end;
end;

constructor TPSStringList.Create;
begin
  inherited Create;
  List := TPSList.Create;
end;

destructor TPSStringList.Destroy;
begin
  if Assigned(List) then begin
    Clear();
    FreeAndNil(List);
  end;
  inherited;
end;

function Fw(const S: TbtString): TbtString; //  First word
var
  x: integer;
begin
  x := Pos(tbtString(' '), s);
  if x > 0 then
    Fw := Copy(S, 1, x - 1)
  else
    Fw := S;
end;

function FastUpperCase(const S: TbtString): TbtString;
{$if btCharIsWide}
begin
  Result := UpperCase(S);
end;
{$else}
{Fast uppercase}
var p: PTbtChar;
begin
  Result := S;
  if Length(Result) > 0 then
  begin
    p := PTbtChar(Result);
    while p^ <> #0 do begin
      {$if declared(CharInSet)}
      if CharInSet(p^,   [#97..#122]) then
      {$else}
      if (         p^ in [#97..#122]) then
      {$ifend}
        p^ := TbtChar(Ord(p^) - 32);
      Inc(p);
    end;
  end;
end;
{$ifend}

function FastLowerCase(const s: TbtString): TbtString;
{$if btCharIsWide}
begin
  Result := LowerCase(S);
end;
{$else}
{Fast lowercase}
var p: PTbtChar;
begin
  Result := S;
  if Length(Result) > 0 then
  begin
    p := PTbtChar(Result);
    while p^ <> #0 do begin
      {$if declared(CharInSet)}
      if CharInSet(p^,   [#65..#90]) then
      {$else}
      if (         p^ in [#65..#90]) then
      {$ifend}
        p^ := TbtChar(Ord(p^) + 32);
      Inc(p);
    end;
  end;
end;
{$ifend}

type
  TRTab = record
    name: TbtString;
    c: TPSPasToken;
  end;

const
  KEYWORD_COUNT = 65;  //*NVDS
  LookupTable: array[0..KEYWORD_COUNT - 1] of TRTab = (
      (name: 'AND';            c: CSTII_and),
      (name: 'ARRAY';          c: CSTII_array),
      (name: 'AS';             c: CSTII_as),
      (name: 'BEGIN';          c: CSTII_begin),
      (name: 'CASE';           c: CSTII_case),
      (name: 'CHR';            c: CSTII_chr),
      (name: 'CLASS';          c: CSTII_class),
      (name: 'CONST';          c: CSTII_const),
      (name: 'CONSTRUCTOR';    c: CSTII_constructor),
      (name: 'DESTRUCTOR';     c: CSTII_destructor),
      (name: 'DIV';            c: CSTII_div),
      (name: 'DO';             c: CSTII_do),
      (name: 'DOWNTO';         c: CSTII_downto),
      (name: 'ELSE';           c: CSTII_else),
      (name: 'END';            c: CSTII_end),
      (name: 'EXCEPT';         c: CSTII_except),
      (name: 'EXIT';           c: CSTII_exit),
      (name: 'EXPORT';         c: CSTII_Export),
      (name: 'EXTERNAL';       c: CSTII_External),
      (Name: 'FINALIZATION';   c : CSTII_finalization),//* Nvds
      (name: 'FINALLY';        c: CSTII_finally),
      (name: 'FOR';            c: CSTII_for),
      (name: 'FORWARD';        c: CSTII_Forward),
      (name: 'FUNCTION';       c: CSTII_function),
      (name: 'GOTO';           c: CSTII_Goto),
      (name: 'IF';             c: CSTII_if),
      (name: 'IMPLEMENTATION'; c: CSTII_Implementation),
      (name: 'IN';             c: CSTII_in),
      (name: 'INHERITED';      c: CSTII_inherited),
      (Name: 'INITIALIZATION'; c: CSTII_initialization), //* Nvds
      (name: 'INTERFACE';      c: CSTII_Interface),
      (name: 'IS';             c: CSTII_is),
      (name: 'LABEL';          c: CSTII_Label),
      (name: 'MOD';            c: CSTII_mod),
      (name: 'NIL';            c: CSTII_nil),
      (name: 'NOT';            c: CSTII_not),
      (name: 'OF';             c: CSTII_of),
      (name: 'OR';             c: CSTII_or),
      (name: 'ORD';            c: CSTII_ord),
      (name: 'OUT';            c: CSTII_Out),
      (name: 'OVERRIDE';       c: CSTII_override),
    //(name: 'DEFAULT';        c: CSTII_default), //Birb (if added, don't forget to increase KEYWORD_COUNT)
      (name: 'PRIVATE';        c: CSTII_private),
      (name: 'PROCEDURE';      c: CSTII_procedure),
      (name: 'PROGRAM';        c: CSTII_program),
      (name: 'PROPERTY';       c: CSTII_property),
      (name: 'PROTECTED';      c: CSTII_protected),
      (name: 'PUBLIC';         c: CSTII_public),
      (name: 'PUBLISHED';      c: CSTII_published),
      (name: 'RECORD';         c: CSTII_record),
      (name: 'REPEAT';         c: CSTII_repeat),
      (name: 'SET';            c: CSTII_set),
      (name: 'SHL';            c: CSTII_shl),
      (name: 'SHR';            c: CSTII_shr),
      (name: 'THEN';           c: CSTII_then),
      (name: 'TO';             c: CSTII_to),
      (name: 'TRY';            c: CSTII_try),
      (name: 'TYPE';           c: CSTII_type),
      (name: 'UNIT';           c: CSTII_Unit),
      (name: 'UNTIL';          c: CSTII_until),
      (name: 'USES';           c: CSTII_uses),
      (name: 'VAR';            c: CSTII_var),
      (name: 'VIRTUAL';        c: CSTII_virtual),
      (name: 'WHILE';          c: CSTII_while),
      (name: 'WITH';           c: CSTII_with),
      (name: 'XOR';            c: CSTII_xor));

function TPSPascalParser.GetCol: Cardinal;
begin
  Result := FRealPosition - FLastEnterPos + 1;
end;

{+}
procedure TPSPascalParser.StateSave(out AState: TPSPascalParserStateData; ALevel: TPSPascalParserStateLevel);
begin
  AState.FLevel := ALevel;
  //
  // pslMini
  AState.FRealPosition := FRealPosition;
  AState.FTokenLength := FTokenLength;
  if ALevel > pslMini then
  begin
    //
    // pslFull
    AState.FLastEnterPos := FLastEnterPos;
    AState.FRow := FRow;
    AState.FTokenId := FTokenId;
    AState.FToken := FToken;
    AState.FOriginalToken := FOriginalToken;
  end;
end;
procedure TPSPascalParser.StateRestore(var AState: TPSPascalParserStateData);
begin
  //
  // pslMini
  FRealPosition := AState.FRealPosition;
  FTokenLength := AState.FTokenLength;
  if AState.FLevel > pslMini then
  begin
    //
    // pslFull
    FLastEnterPos := AState.FLastEnterPos;
    FRow := AState.FRow;
    FTokenId := AState.FTokenId;
    FToken := AState.FToken;
    FOriginalToken := AState.FOriginalToken;
  end;
end;
{+.}

procedure TPSPascalParser.Next;
var
  Err: TPSParserErrorKind;
  FLastUpToken: TbtString;

  function CheckReserved(Const S: TbtString; var CurrTokenId: TPSPasToken): Boolean;
  var
    L, H, I, J: LongInt;
    SName: TbtString;
  begin
    L := 0;
    J := Length(S);
    H := KEYWORD_COUNT-1;
    while L <= H do
    begin
      I := (L + H) shr 1;
      SName := LookupTable[i].Name;
      if J = Length(SName) then
      begin
        if S = SName then
        begin
          Result := True;
          CurrTokenId := LookupTable[I].c;
          Exit;
        end;
        if S > SName then
          L := I + 1
        else
          H := I - 1;
      end else
        if S > SName then
          L := I + 1
        else
          H := I - 1;
    end;
    Result := False;
  end;

  function _GetToken(CurrTokenPos, CurrTokenLen: Cardinal): TbtString;
  var
    S: tbtString;
  begin
    {$hints off}
    SetLength(S, CurrTokenLen);
    {$hints on}
    Move(FText[CurrTokenPos], S[1], CurrtokenLen*btCharSize);
    Result := S;
  end;

  function ParseToken(var CurrTokenPos, CurrTokenLen: Cardinal; var CurrTokenId: TPSPasToken): TPSParserErrorKind;
  {Parse the token}
  var
    ct, ci: Cardinal;
    hs: Boolean;
    {$if btCharIsAnsi}
    p: PTbtChar;
    {$ifend}
  begin
    Result := iNoError;
    ct := CurrTokenPos;
    case FText[ct] of
      #0: begin
        CurrTokenId := CSTI_EOF;
        CurrTokenLen := 0;
      end;

      'A'..'Z', 'a'..'z', '_': begin
        ci := ct + 1;
        while {$if declared(CharInSet)}
              CharInSet(FText[ci],   ['_', '0'..'9', 'a'..'z', 'A'..'Z'])
              {$else}
              (         FText[ci] in ['_', '0'..'9', 'a'..'z', 'A'..'Z'])
              {$ifend}
        do begin
          Inc(ci);
        end;
        CurrTokenLen := ci - ct;

        FLastUpToken := _GetToken(CurrTokenPos, CurrtokenLen);
        {$if btCharIsWide}
        FLastUpToken := FastUpperCase(FLastUpToken);
        {$else} // FastUpperCase
        p := PTbtChar(FLastUpToken);
        while p^ <> #0 do begin
          {$if declared(CharInSet)}
          if CharInSet(p^,   [#97..#122]) then
          {$else}
          if (         p^ in [#97..#122]) then
          {$ifend}
            p^ := TbtChar(Ord(p^) - 32);
          Inc(p);
        end;
        {$ifend}
        if not CheckReserved(FLastUpToken, CurrTokenId) then begin
          CurrTokenId := CSTI_Identifier;
        end;
      end;

      '$': begin
        ci := ct + 1;

        while {$if declared(CharInSet)}
              CharInSet(FText[ci],   ['0'..'9', 'a'..'f', 'A'..'F'])
              {$else}
              (         FText[ci] in ['0'..'9', 'a'..'f', 'A'..'F'])
              {$ifend}
        do
          Inc(ci);

        CurrTokenId := CSTI_HexInt;
        CurrTokenLen := ci - ct;
      end;

      '0'..'9': begin
        hs := False;
        ci := ct;
        while {$if declared(CharInSet)}
              CharInSet(FText[ci],   ['0'..'9'])
              {$else}
              (         FText[ci] in ['0'..'9'])
              {$ifend}
        do begin
          Inc(ci);
          if (FText[ci] = '.') and (not hs) then begin
            if (FText[ci+1] = '.') then
              Break;
            hs := True;
            Inc(ci);
          end;
        end;

        if {$if declared(CharInSet)}
           CharInSet(FText[ci],   ['E','e'])
           {$else}
           (         FText[ci] in ['E','e'])
           {$ifend}
           //
           and
           //
           (
             {$if declared(CharInSet)}
             CharInSet(FText[ci+1],  ['0'..'9'])
             {$else}
             (         FText[ci+1] in ['0'..'9'])
             {$ifend}
             //
             or
             //
             (
               {$if declared(CharInSet)}
               CharInSet(FText[ci+1],   ['+','-'])
               {$else}
               (         FText[ci+1] in ['+','-'])
               {$ifend}
               //
               and
               //
               {$if declared(CharInSet)}
               CharInSet(FText[ci+2],   ['0'..'9'])
               {$else}
               (         FText[ci+2] in ['0'..'9'])
               {$ifend}
             )
           ) then
        begin
          hs := True;
          Inc(ci);
          if {$if declared(CharInSet)}
             CharInSet(FText[ci],   ['+','-'])
             {$else}
             (         FText[ci] in ['+','-'])
             {$ifend}
          then
            Inc(ci);
          repeat
            Inc(ci);
          until (not
            {$if declared(CharInSet)}
            CharInSet(FText[ci],   ['0'..'9'])
            {$else}
            (         FText[ci] in ['0'..'9'])
            {$ifend}
          );
        end;

        if hs then
          CurrTokenId := CSTI_Real
        else
          CurrTokenId := CSTI_Integer;

        CurrTokenLen := ci - ct;
      end;

      #39: begin
        ci := ct + 1;
        while True do begin
          if (FText[ci] = #0) or (FText[ci] = #13) or (FText[ci] = #10) then
            Break;
          if (FText[ci] = #39) then begin
            if FText[ci+1] = #39 then
              Inc(ci)
            else
              Break;
          end;
          Inc(ci);
        end;
        if (FText[ci] = #39) then
          CurrTokenId := CSTI_String
        else begin
          CurrTokenId := CSTI_String;
          Result := iStringError;
        end;
        CurrTokenLen := ci - ct + 1;
      end;

      '#': begin
        ci := ct + 1;
        if (FText[ci] = '$') then begin
          inc(ci);
          while  {$if declared(CharInSet)}
                 CharInSet(FText[ci],   ['A'..'F', 'a'..'f', '0'..'9'])
                 {$else}
                 (         FText[ci] in ['A'..'F', 'a'..'f', '0'..'9'])
                 {$ifend}
          do begin
            Inc(ci);
          end;
          CurrTokenId := CSTI_Char;
          CurrTokenLen := ci - ct;
        end else begin
          while  {$if declared(CharInSet)}
                 CharInSet(FText[ci],   ['0'..'9'])
                 {$else}
                 (         FText[ci] in ['0'..'9'])
                 {$ifend}
          do begin
            Inc(ci);
          end;
          if {$if declared(CharInSet)}
             CharInSet(FText[ci],   ['A'..'Z', 'a'..'z', '_'])
             {$else}
             (         FText[ci] in ['A'..'Z', 'a'..'z', '_'])
             {$ifend}
          then begin
            Result := iCharError;
            CurrTokenId := CSTI_Char;
          end else
            CurrTokenId := CSTI_Char;
          CurrTokenLen := ci - ct;
        end;
      end;

      '=': begin
        CurrTokenId := CSTI_Equal;
        CurrTokenLen := 1;
      end;

      '>': begin
        if (FText[ct + 1] = '=') then begin
          CurrTokenid := CSTI_GreaterEqual;
          CurrTokenLen := 2;
        end else begin
          CurrTokenid := CSTI_Greater;
          CurrTokenLen := 1;
        end;
      end;

      '<': begin
        if (FText[ct + 1] = '=') then begin
          CurrTokenId := CSTI_LessEqual;
          CurrTokenLen := 2;
        end else begin
          if (FText[ct + 1] = '>') then begin
            CurrTokenId := CSTI_NotEqual;
            CurrTokenLen := 2;
          end else begin
            CurrTokenId := CSTI_Less;
            CurrTokenLen := 1;
          end;
        end;
      end;

      ')': begin
        CurrTokenId := CSTI_CloseRound;
        CurrTokenLen := 1;
      end;

      '(': begin
        if (FText[ct + 1] = '*') then begin
          ci := ct + 1;
          while (FText[ci] <> #0) do begin
            if (FText[ci] = '*') and (FText[ci + 1] = ')') then
              Break;
            if FText[ci] = #13 then begin
              inc(FRow);
              if FText[ci+1] = #10 then
                Inc(ci);
              FLastEnterPos := ci + 1;
            end else if FText[ci] = #10 then begin
              Inc(FRow);
              FLastEnterPos := ci + 1;
            end;
            Inc(ci);
          end;
          CurrTokenId := CSTIINT_Comment;
          if (FText[ci] = #0) then
            Result := iCommentError
          else
            Inc(ci, 2);
          CurrTokenLen := ci - ct;
        end else begin
          CurrTokenId := CSTI_OpenRound;
          CurrTokenLen := 1;
        end;
      end;

      '[': begin
        CurrTokenId := CSTI_OpenBlock;
        CurrTokenLen := 1;
      end;

      ']': begin
        CurrTokenId := CSTI_CloseBlock;
        CurrTokenLen := 1;
      end;

      ',': begin
        CurrTokenId := CSTI_Comma;
        CurrTokenLen := 1;
      end;

      '.': begin
        if (FText[ct + 1] = '.') then begin
          CurrTokenLen := 2;
          CurrTokenId := CSTI_TwoDots;
        end else begin
          CurrTokenId := CSTI_Period;
          CurrTokenLen := 1;
        end;
      end;

      '@': begin
        CurrTokenId := CSTI_AddressOf;
        CurrTokenLen := 1;
      end;

      '^': begin
        CurrTokenId := CSTI_Dereference;
        CurrTokenLen := 1;
      end;

      ';': begin
        CurrTokenId := CSTI_Semicolon;
        CurrTokenLen := 1;
      end;

      ':': begin
        if (FText[ct + 1] = '=') then begin
          CurrTokenId := CSTI_Assignment;
          CurrTokenLen := 2;
        end else begin
          CurrTokenId := CSTI_Colon;
          CurrTokenLen := 1;
        end;
      end;

      '+': begin
        CurrTokenId := CSTI_Plus;
        CurrTokenLen := 1;
      end;

      '-': begin
        CurrTokenId := CSTI_Minus;
        CurrTokenLen := 1;
      end;

      '*': begin
        CurrTokenId := CSTI_Multiply;
        CurrTokenLen := 1;
      end;

      '/': begin
        if (FText[ct + 1] = '/') then begin
          ci := ct + 1;
          while (FText[ci] <> #0) and (FText[ci] <> #13) and (FText[ci] <> #10) do begin
            Inc(ci);
          end;
          CurrTokenId := CSTIINT_Comment;
          //if (FText[ci] = #0) then
          //  ?
          //else
          //  ?
          CurrTokenLen := ci - ct;
        end else begin
          CurrTokenId := CSTI_Divide;
          CurrTokenLen := 1;
        end;
      end;

      #32, #9, #13, #10: begin
        ci := ct;
        while
          {$if declared(CharInSet)}
          CharInSet(FText[ci],   [#32, #9, #13, #10])
          {$else}
          (         FText[ci] in [#32, #9, #13, #10])
          {$ifend}
        do begin
          if (FText[ci] = #13) then begin
            inc(FRow);
            if (FText[ci+1] = #10) then
              inc(ci);
            FLastEnterPos := ci +1;
          end else if (FText[ci] = #10) then begin
            inc(FRow);
            FLastEnterPos := ci +1;
          end;
          Inc(ci);
        end;
        CurrTokenId := CSTIINT_WhiteSpace;
        CurrTokenLen := ci - ct;
      end;

      '{': begin
        ci := ct + 1;
        while (FText[ci] <> #0) and (FText[ci] <> '}') do begin
          if (FText[ci] = #13) then begin
            inc(FRow);
            if (FText[ci+1] = #10) then
              Inc(ci);
            FLastEnterPos := ci + 1;
          end else if (FText[ci] = #10) then begin
            Inc(FRow);
            FLastEnterPos := ci + 1;
          end;
          Inc(ci);
        end;
        CurrTokenId := CSTIINT_Comment;
        if (FText[ci] = #0) then
          Result := iCommentError;
        CurrTokenLen := ci - ct + 1;
      end;
      else begin
        Result := iSyntaxError;
        CurrTokenId := CSTIINT_Comment;
        CurrTokenLen := 1;
      end;
    end; // case
  end; // function ParseToken

begin // procedure TPSPascalParser.Next
  if FText = nil then begin
    FTokenLength := 0;
    FRealPosition := 0;
    FTokenId := CSTI_EOF;
    Exit;
  end;
  FLastUpToken := '';
  repeat
    FRealPosition := FRealPosition + Cardinal(FTokenLength);
    Err := ParseToken(FRealPosition, Cardinal(FTokenLength), FTokenID);
    if Err <> iNoError then begin
      FTokenLength := 0;
      FTokenId := CSTI_EOF;
      FToken := '';
      FOriginalToken := '';
      if @FParserError <> nil then
        FParserError(Self, Err);
      Exit;
    end;

    case FTokenID of
      CSTIINT_Comment: begin
        if not FEnableComments then
         Continue
        else begin
          SetLength(FOriginalToken, FTokenLength);
          Move(FText[CurrTokenPos], FOriginalToken[1], FTokenLength*btCharSize);
          FToken := FOriginalToken;
        end;
      end;

      CSTIINT_WhiteSpace: begin
        if not FEnableWhitespaces then
          Continue
        else begin
          SetLength(FOriginalToken, FTokenLength);
          Move(FText[CurrTokenPos], FOriginalToken[1], FTokenLength*btCharSize);
          FToken := FOriginalToken;
        end;
      end;

      CSTI_Integer, CSTI_Real, CSTI_String, CSTI_Char, CSTI_HexInt: begin
        SetLength(FOriginalToken, FTokenLength);
        Move(FText[CurrTokenPos], FOriginalToken[1], FTokenLength*btCharSize);
        FToken := FOriginalToken;
      end;

      CSTI_Identifier: begin
        SetLength(FOriginalToken, FTokenLength);
        Move(FText[CurrTokenPos], FOriginalToken[1], FTokenLength*btCharSize);
        FToken := FLastUpToken;
      end;
      else begin
        {+}
        if FTokenID >= CSTII_and then begin // NEW: allow variant/interface call like: vOutlook.Class or vOutlook.Type
          SetLength(FOriginalToken, FTokenLength);
          Move(FText[CurrTokenPos], FOriginalToken[1], FTokenLength*btCharSize);
        end
        else begin
          FOriginalToken := '';
        end;
        {+.}
        FToken := '';
      end;
    end; // case FTokenID
    Break;
  until False;
end; // procedure TPSPascalParser.Next

procedure TPSPascalParser.SetText(const Data: TbtString);
begin
  FData := Data;
  FText := Pointer(FData);
  FTokenLength := 0;
  FRealPosition := 0;
  FTokenId := CSTI_EOF;
  FLastEnterPos := 0;
  FRow := 1;
  Next();
end;

function TPSList.IndexOf(P: Pointer): Longint;
var i: Integer;
begin
  for i := FCount-1 downto 0 do begin
    if FData[i] = p then begin
      Result := i;
      Exit;
    end;
  end;
  Result := -1;
end;

{ TPSUnitList }

function TPSUnitList.Add: TPSUnit;
begin
  Result := TPSUnit.Create(Self);
  fList.Add(Result);
end;

constructor TPSUnitList.Create;
begin
  inherited Create;
  fList := TPSList.Create;
end;

destructor TPSUnitList.Destroy;
var i: Integer; O: TObject;
begin
  if Assigned(fList) then begin
    for i := fList.Count-1 downto 0 do begin
      O := TObject(fList.FData[i]);
      fList.FData[i] := nil;
      O.Free;
    end;
    FreeAndNil(fList);
  end;
  inherited;
end;

function TPSUnitList.GetUnit(UnitName: TbtString): TPSUnit;
var i: Integer;
begin
  UnitName := FastUpperCase(UnitName);
  for i := 0 to fList.Count-1 do begin
    if TPSUnit(fList[i]).UnitName = UnitName then begin
      Result := TPSUnit(fList[i]);
      Exit;
    end;
  end;
  Result := Add();
  Result.UnitName := UnitName;
end;

{ TPSUnit }

procedure TPSUnit.AddUses(const pUnitName: TbtString);
var UsesUnit: TPSUnit;
begin
  UsesUnit := fList.GetUnit(pUnitName);
  fUnits.Add(UsesUnit);
end;

constructor TPSUnit.Create(List: TPSUnitList);
begin
  inherited Create;
  fUnits := TPSList.Create;
  fList := List;
end;

destructor TPSUnit.Destroy;
begin
  FreeAndNil(fUnits);
  inherited;
end;

function TPSUnit.HasUses(pUnitName: TbtString): Boolean;
var i: Integer;
begin
  pUnitName := FastUpperCase(pUnitName);
  Result := fUnitName = pUnitName;
  if Result then
    Exit;
  for i := 0 to fUnits.Count-1 do begin
    Result := TPSUnit(fUnits[i]).HasUses(pUnitName);
    if Result then
      Exit;
  end;
end;

procedure TPSUnit.SetUnitName(const Value: TbtString);
begin
  fUnitName := FastUpperCase(Value);
end;

procedure _linkdbginfo; // for comfortly debug
begin
  if (@_linkdbginfo <> nil) then Exit;
  BoolToTxt(False);
  TPSTypeRecBase(nil).BaseTypeToStr();
  PSBaseTypeToStr(PIFTypeRecBase(nil));
  PSBaseTypeToStr(0);
  psdbg('');
  psdbg_null('');
end;
//
initialization
  PosInit();
  //dbg := psdbg_null;
  dbg := psdbg;
  if (@_linkdbginfo = nil) then // for comfortly debug
    _linkdbginfo();
end.
