{ uPSRuntime.pas } // version: 2020.1010.1010
{----------------------------------------------------------------------------}
{ RemObjects Pascal Script                                                   }
{----------------------------------------------------------------------------}
unit uPSRuntime;
{$I PascalScript.inc}
{

RemObjects Pascal Script III
Copyright (C) 2000-2009 by Carlo Kok (ck@carlo-kok.com)

}

interface
{$IFDEF FPC}        // FPC: {%H-} https://wiki.freepascal.org/Turn_warnings_and_hints_on_or_off
  {$warn 4015 off}  // FPC: Hint: Use DIV instead to get an integer result
  {$warn 4035 off}  // FPC: Hint: Mixing signed expressions and longwords gives a 64bit result
  {$warn 4055 off}  // FPC: Hint: Conversion between ordinals and pointers is not portable
  {$warn 4056 off}  // FPC: Warning: Conversion between ordinals and pointers is not portable
  {$warn 4081 off}  // FPC: Hint: Converting the operands to "$1" before doing the multiply could prevent overflow errors.
  {-warn 5057 off}  // FPC: Hint: Local variable "$1" does not seem to be initialized
{$ENDIF}

uses
  {$IFDEF DELPHI7UP}Types,{$ENDIF}
  //{$IFDEF DELPHI17UP}System.UITypes,{$ENDIF}
  {$IFNDEF FPC}{$IFDEF DELPHI2010UP}
  System.Rtti,
  {$ENDIF}{$ENDIF !FPC}
  {$IFDEF FPC}{$IFDEF _INVOKECALL_IMPL_}
  {%H-}Rtti, // Warning: Unit "Rtti" is experimental
  {$ENDIF}{$ENDIF FPC}
  SysUtils,
  {$IFNDEF MSWINDOWS}
  Classes,
  {$ENDIF}
  uPSUtils
  {$IFDEF DELPHI6UP} // D6_UP OR FPC
    {+}{$DEFINE _VARIANTS_}{+.}
  ,Variants
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  ,Windows
  {$ENDIF}
  {$IFNDEF PS_NOIDISPATCH}
    {$IFDEF DELPHI3UP} // or FPC
    ,ActiveX
    {$ELSE}
    ,Ole2
    {$ENDIF}
  {$ENDIF !PS_NOIDISPATCH}
  ;

{+}
const // TPSRuntimeClass.Register*  => RTCLRG_*
  RTCLRG_CONSTRUCTOR     = 4;
  RTCLRG_CONTRUCTOR_VIRT = 5;

  RTCLRG_METHOD               = 0;
  RTCLRG_METHOD_NAME          = 8;
  RTCLRG_METHOD_VIRT_ABSTRACT = 1;
  RTCLRG_METHOD_VIRT          = 1;

//RTCLRG_PROP_INFO        = 2;
  RTCLRG_PROP_HELPER      = 3;
  RTCLRG_PROP_HELPER_NAME = 7;
  RTCLRG_PROP_NAME_HELPER = 9;

  RTCLRG_EVENT_PROP_HELPER = 6;

//RTCLRG_CLASS_METHOD      = 10; // TODO: implementation
//RTCLRG_CLASS_PROP        = 11; // TODO: implementation
{+.}

type
  TPSExec = class;
  TPSStack = class;
  TPSRuntimeAttributes = class;
  TPSRuntimeAttribute = class;

  TPSError = (erNoError, erCannotImport, erInvalidType, erInternalError,
    erInvalidHeader, erInvalidOpcode, erInvalidOpcodeParameter, erNoMainProc,
    erOutOfGlobalVarsRange, erOutOfProcRange, erOutOfRange, erOutOfStackRange,
    erTypeMismatch, erUnexpectedEof, erVersionError, erDivideByZero, erMathError,
    erCouldNotCallProc, erOutofRecordRange, erOutOfMemory, erException,
    erNullPointerException, erNullVariantError, erInterfaceNotSupported, erCustomError);

  TPSStatus = (isNotLoaded, isLoaded, isRunning, isPaused);

  TByteArray = array[0..1023] of Byte;
  PByteArray = ^TByteArray;

  TDWordArray = array[0..1023] of Cardinal;
  PDWordArray = ^TDWordArray;

{@link(TPSProcRec)
  PIFProcRec is a pointer to a TIProcRec record}
  TPSProcRec = class;
  TIFProcRec = TPSProcRec;
  TPSExternalProcRec = class;
  TIFPSExternalProcRec = TPSExternalProcRec;
  TIFExternalProcRec = TPSExternalProcRec;
  PIFProcRec = TPSProcRec;
  PProcRec = ^TProcRec;

  TPSProcPtr = function(Caller: TPSExec; p: TPSExternalProcRec; Global, Stack: TPSStack): Boolean;

  TPSFreeProc = procedure (Caller: TPSExec; p: PProcRec);

  TPSProcRec = class
  private
    FAttributes: TPSRuntimeAttributes;
  public
    constructor Create(Owner: TPSExec);
    destructor Destroy; override;

    property Attributes: TPSRuntimeAttributes read FAttributes;
  end;

  TPSExternalProcRec = class(TPSProcRec)
  private
    FExt1: Pointer;
    FExt2: Pointer;
    FName: TbtString;
    FProcPtr: TPSProcPtr;
    FDecl: TbtString;
    {+}
    FRCL: Pointer; // == TPSRuntimeClass;
    FWrap: Boolean;
    {+.}
  public
    property Name: TbtString read FName write FName;
    property Decl: TbtString read FDecl write FDecl;
    property Ext1: Pointer read FExt1 write FExt1;
    property Ext2: Pointer read FExt2 write FExt2;
    property ProcPtr: TPSProcPtr read FProcPtr write FProcPtr;
  end;

  TPSInternalProcRec = class(TPSProcRec)
  private
    FData: PByteArray;
    FLength: Cardinal;
    FExportNameHash: Longint;
    FExportDecl: TbtString;
    FExportName: TbtString;
  public
    destructor Destroy; override;

    property Data: PByteArray read FData;
    property Length: Cardinal read FLength;
    property ExportNameHash: Longint read FExportNameHash;
    property ExportName: TbtString read FExportName write FExportName;
    property ExportDecl: TbtString read FExportDecl write FExportDecl;
  end;

  TProcRec = record
    Name: ShortString;
    Hash: Longint;
    ProcPtr: TPSProcPtr;
    FreeProc: TPSFreeProc;
    Ext1, Ext2: Pointer;
  end;

  PBTReturnAddress = ^TBTReturnAddress;

  TBTReturnAddress = packed record
    ProcNo: TPSInternalProcRec;
    Position, StackBase: Cardinal;
  end;

  TPSTypeRec = class
  private
    FExportNameHash: Longint;
    FExportName: TbtString;
    FBaseType: TPSBaseType;
    FAttributes: TPSRuntimeAttributes;
  protected
    FRealSize: Cardinal;
  public
    constructor Create(Owner: TPSExec);
    destructor Destroy; override;

    {+}
    procedure Clear; virtual;
    {+.}
    procedure CalcSize; virtual;

    property RealSize: Cardinal read FRealSize;
    property BaseType: TPSBaseType read FBaseType write FBaseType;
    property ExportName: TbtString read FExportName write FExportName;
    property ExportNameHash: Longint read FExportNameHash write FExportNameHash;
    property Attributes: TPSRuntimeAttributes read FAttributes write FAttributes;
  end;

  {+}
  TPSTypeRec_Chars = class(TPSTypeRec)
  private
    FPChasrRefCount: Integer;
  public
    property PCharRefCount: Integer read FPChasrRefCount write FPChasrRefCount;
  end;

  TPSTypeRec_PChar = class(TPSTypeRec)
  //private
  //  //FTempBaseType: TPSBaseType;
  //  FTempData: Pointer;
  //  FTypeCharsRec: TPSTypeRec_Chars;
  //public
  //  constructor Create(Owner: TPSExec);
  //  destructor Destroy; override;

  //  procedure Clear; override;

    //property TempBaseType: TPSBaseType read FTempBaseType write FTempBaseType;
  //  property TempData: Pointer read FTempData write FTempData;
  end;

  TPSTypeRec_PAnsiChar = class(TPSTypeRec_PChar)
  private
    fBuffer: TbtString;
  end;

  {$IFNDEF PS_NOWIDESTRING}
    {$if declared(btPWideChar)}
  TPSTypeRec_PWideChar = class(TPSTypeRec_PChar)
  private
    fBuffer: TbtUnicodeString;
  end;
    {$ifend}
  {$ENDIF !PS_NOWIDESTRING}
  {+.}

  TPSTypeRec_ProcPtr = class(TPSTypeRec)
  private
    FParamInfo: TbtString;
  public
    property ParamInfo: TbtString read FParamInfo write FParamInfo;
    procedure CalcSize; override;
  end;
  PIFTypeRec = TPSTypeRec;

  TPSTypeRec_Class = class(TPSTypeRec)
  private
    FCN: TbtString;
  public
    property CN: TbtString read FCN write FCN;
  end;
{$IFNDEF PS_NOINTERFACES}

  TPSTypeRec_Interface = class(TPSTypeRec)
  private
    FGuid: TGUID;
  public
    property Guid: TGUID read FGuid write FGuid;
  end;
{$ENDIF}

  TPSTypeRec_Array = class(TPSTypeRec)
  private
    FArrayType: TPSTypeRec;
  public
    property ArrayType: TPSTypeRec read FArrayType write FArrayType;
    procedure CalcSize; override;
  end;

  TPSTypeRec_StaticArray = class(TPSTypeRec_Array)
  private
    FSize: Longint;
    FStartOffset: LongInt;
  public
    procedure CalcSize; override;

    property Size: Longint read FSize write FSize;
    property StartOffset: LongInt read FStartOffset write FStartOffset;
  end;

  TPSTypeRec_Set = class(TPSTypeRec)
  private
    FBitSize: Longint;
    FByteSize: Longint;
  public
    procedure CalcSize; override;

    {The number of bytes this would require (same as realsize)}
    property aByteSize: Longint read FByteSize write FByteSize;
    property aBitSize: Longint read FBitSize write FBitSize;
  end;

  TPSTypeRec_Record = class(TPSTypeRec)
  private
    FFieldTypes: TPSList;
    FRealFieldOffsets: TPSList;
  public
    constructor Create(Owner: TPSExec);
    destructor Destroy; override;

    procedure CalcSize; override;

    property FieldTypes: TPSList read FFieldTypes;
    property RealFieldOffsets: TPSList read FRealFieldOffsets;
  end;

  PPSVariant = ^TPSVariant;
  PIFVariant = PPSVariant;
  TPSVariant = packed record
    FType: TPSTypeRec;
  end;

  PPSVariantData = ^TPSVariantData;

  TNullRecord = packed record end;
  PNullRecord = ^TNullRecord;

  TPSVarRec = packed record
   case Byte of
    btReturnAddress: (vRet: Pointer);
    btU8           : (vU8: tbtU8);
    btS8           : (vS8: tbtS8);
    btU16          : (vU16: tbtU16);
    btS16          : (vS16: tbtS16);
    btU32          : (vU32: tbtU32);
    btS32          : (vS32: tbtS32);
    btSingle       : (vSingle: tbtSingle);
    btDouble       : (vDouble: tbtDouble);
    btExtended     : (vExtended: tbtExtended);
    btString       : (vString: ^TbtString);
    btRecord       : (vRecord: TNullRecord);
    btArray        : (vArray: array[0..0] of Byte);
    btPointer      : (vPt: Pointer);
    btPChar        : (vPChar: ^tbtChar);
    btResourcePointer : (vResPtr: Pointer);
    btVariant      : (vVar: TVarData);
    {$IFNDEF PS_NOINT64}
    btS64          : (vS64: tbtS64);
    {$ENDIF}
    btChar         : (vChar: AnsiChar);
    {$IFNDEF PS_NOWIDESTRING}
    btWideString   : (vWideString: ^tbtWideString);
    btWideChar     : (vWideChar: TbtWideChar);
    {$ENDIF}
    btProcPtr      : (vProcPtr: Pointer); // PPSVariantProcPtr
    btStaticArray  : (vStaticArray: TPSTypeRec_StaticArray);
    btSet          : (vSet: TPSTypeRec_Set);
    btCurrency     : (vCurrency: tbtCurrency);
    btClass        : (vClass: TObject);
    btInterface    : (vIntf: Pointer);
    btNotificationVariant: (vNotifVar: Pointer);
    btUnicodeString: (vUnicodeString: ^TbtUnicodeString);
    {$if declared(btPWideChar)}
    btPWideChar    : (vPWideChar: ^TbtWideChar);
    {$ifend}
    //btType         : ;
    btEnum         : (vEnum8: tbtU8; vEnum16: tbtU16; vEnum32: tbtU32);
    //btExtClass     : ; // TPSUndefinedClassType
  end;
  PPSVarRec = ^TPSVarRec;

  TPSVariantData = packed record
    VI: TPSVariant;
    //Data: array[0..0] of Byte;
    Data: TNullRecord;
    //Data: array[0..SizeOf(Pointer)-1] of Byte; // TODO: ???
    //Data: TPSVarRec;
  end;

  PPSVariantU8 = ^TPSVariantU8;

  TPSVariantU8 = packed record
    VI: TPSVariant;
    Data: tbtU8;
  end;

  PPSVariantS8 = ^TPSVariantS8;

  TPSVariantS8 = packed record
    VI: TPSVariant;
    Data: tbts8;
  end;

  PPSVariantU16 = ^TPSVariantU16;

  TPSVariantU16 = packed record
    VI: TPSVariant;
    Data: tbtU16;
  end;

  PPSVariantS16 = ^TPSVariantS16;

  TPSVariantS16 = packed record
    VI: TPSVariant;
    Data: tbts16;
  end;

  PPSVariantU32 = ^TPSVariantU32;

  TPSVariantU32 = packed record
    VI: TPSVariant;
    Data: tbtU32;
  end;

  PPSVariantS32 = ^TPSVariantS32;

  TPSVariantS32 = packed record
    VI: TPSVariant;
    Data: tbts32;
  end;
{$IFNDEF PS_NOINT64}

  PPSVariantS64 = ^TPSVariantS64;

  TPSVariantS64 = packed record
    VI: TPSVariant;
    Data: tbts64;
  end;
{$ENDIF}

  PPSVariantAChar = ^TPSVariantAChar;

  TPSVariantAChar = packed record
    VI: TPSVariant;
    Data: tbtChar;
  end;

{$IFNDEF PS_NOWIDESTRING}

  PPSVariantWChar = ^TPSVariantWChar;

  TPSVariantWChar = packed record
    VI: TPSVariant;
    Data: TbtWideChar;
  end;
{$ENDIF}

  PPSVariantAString = ^TPSVariantAString;

  TPSVariantAString = packed record
    VI: TPSVariant;
    Data: TbtString;
  end;

{$IFNDEF PS_NOWIDESTRING}
  TPSVariantWString = {$IFNDEF DELPHI2009UP}packed {$ENDIF}record
    VI: TPSVariant;
    Data: TbtWideString;
  end;
  PPSVariantWString = ^TPSVariantWString;

  TPSVariantUString = {$IFNDEF DELPHI2009UP}packed {$ENDIF}record
    VI: TPSVariant;
    Data: TbtUnicodeString;
  end;
  PPSVariantUString = ^TPSVariantUString;
{$ENDIF !PS_NOWIDESTRING}

  PPSVariantSingle = ^TPSVariantSingle;

  TPSVariantSingle = packed record
    VI: TPSVariant;
    Data: tbtsingle;
  end;

  PPSVariantDouble = ^TPSVariantDouble;

  TPSVariantDouble = packed record
    VI: TPSVariant;
    Data: tbtDouble;
  end;

  PPSVariantExtended = ^TPSVariantExtended;

  TPSVariantExtended = packed record
    VI: TPSVariant;
    Data: tbtExtended;
  end;

  PPSVariantCurrency = ^TPSVariantCurrency;

  TPSVariantCurrency = packed record
    VI: TPSVariant;
    Data: tbtCurrency;
  end;

  PPSVariantSet = ^TPSVariantSet;

  TPSVariantSet = packed record
    VI: TPSVariant;
    Data: array[0..0] of Byte;
  end;

{$IFNDEF PS_NOINTERFACES}

  PPSVariantInterface = ^TPSVariantInterface;

  TPSVariantInterface = packed record
    VI: TPSVariant;
    Data: IUnknown;
  end;
{$ENDIF}

  PPSVariantClass = ^TPSVariantClass;

  TPSVariantClass = packed record
    VI: TPSVariant;
    Data: TObject;
  end;

  PPSVariantRecord = ^TPSVariantRecord;

  TPSVariantRecord = packed record
    VI: TPSVariant;
    Data: array[0..0] of byte;
  end;

  PPSVariantDynamicArray = ^TPSVariantDynamicArray;

  TPSVariantDynamicArray = packed record
    VI: TPSVariant;
    Data: Pointer;
  end;

  PPSVariantStaticArray = ^TPSVariantStaticArray;

  TPSVariantStaticArray = packed record
    VI: TPSVariant;
    Data: array[0..0] of byte;
  end;

  PPSVariantPointer = ^TPSVariantPointer;

  TPSVariantPointer = packed record
    VI: TPSVariant;
    DataDest: Pointer;
    DestType: TPSTypeRec;
    FreeIt: LongBool;
  end;

  PPSVariantReturnAddress = ^TPSVariantReturnAddress;

  TPSVariantReturnAddress = packed record
    VI: TPSVariant;
    Addr: TBTReturnAddress;
  end;

  PPSVariantVariant = ^TPSVariantVariant;

  TPSVariantVariant = packed record
    VI: TPSVariant;
    Data: Variant;
  end;

  PPSVariantProcPtr = ^TPSVariantProcPtr;
  TPSVariantProcPtr = packed record
    VI: TPSVariant;
    ProcNo: Cardinal;
    Self: Pointer;
    Ptr: Pointer;
    {
      ProcNo = 0  means Self/Ptr become active (Ptr = nil means it's nil)
    }
  end;

  TPSVarFreeType = (
    vtNone,
    vtTempVar
    );

  TPSResultData = packed record
    P: Pointer;
    aType: TPSTypeRec;
    FreeType: TPSVarFreeType;
  end;
  {+}
  PPSResultData=^TPSResultData;
  {+.}

  PPSResource = ^TPSResource;

  TPSResource = record
    Proc: Pointer;
    P: Pointer;
  end;

  TPSAttributeUseProc = function (Sender: TPSExec; const AttribType: TbtString; Attr: TPSRuntimeAttribute): Boolean;

  TPSAttributeType = class
  private
    FTypeName: TbtString;
    FUseProc: TPSAttributeUseProc;
    FTypeNameHash: Longint;
  public
    property UseProc: TPSAttributeUseProc read FUseProc write FUseProc;
    property TypeName: TbtString read FTypeName write FTypeName;
    property TypeNameHash: Longint read FTypeNameHash write FTypeNameHash;
  end;

  PClassItem = ^TClassItem;
  TClassItem = record
    FName: TbtString;
    FNameHash: Longint;
    b: byte;
    case byte of
      RTCLRG_METHOD: (
        Ptr: Pointer);
      RTCLRG_METHOD_VIRT: ( // == RTCLRG_METHOD_VIRT_ABSTRACT
        PointerInList, ClassTypeM1, ClassTypeM2: Pointer);
      RTCLRG_PROP_HELPER: (
        FReadFunc, FWriteFunc: Pointer); // Property Helper
      RTCLRG_CONSTRUCTOR: (
        PtrC, ClassTypeC1, ClassTypeC2: Pointer);
      RTCLRG_CONTRUCTOR_VIRT: (
        PtrCV, ClassTypeCV1, ClassTypeCV2: Pointer);
      RTCLRG_EVENT_PROP_HELPER: (); // Property helper, like RTCLRG_PROP_HELPER
      RTCLRG_PROP_HELPER_NAME: ();  // Property helper that will pass it's name
      {+} // https://github.com/remobjects/pascalscript/pull/210
      RTCLRG_METHOD_NAME: (
        ProcPtr: TPSProcPtr;
        Ext1, Ext2: Pointer);
      RTCLRG_PROP_NAME_HELPER: ( // Property Helper
        ReadProcPtr, WriteProcPtr: TPSProcPtr;
        ExtRead1, ExtRead2, ExtWrite1, ExtWrite2: Pointer);
      {RTCLRG_CLASS_METHOD: ( // TODO: implementation
        );}
      {RTCLRG_CLASS_PROP: ( // TODO: implementation
        );}
      {+.}
  end;

  PPSVariantIFC = ^TPSVariantIFC;
  {Temporary variant into record}
  TPSVariantIFC = packed record
    Dta: Pointer;
    aType: TPSTypeRec;
    VarParam: Boolean;
  end;
  PIFPSVariantIFC = PPSVariantIFC;
  TIFPSVariantIFC = TPSVariantIFC;

  TPSRuntimeAttribute = class(TObject)
  private
    FValues: TPSStack;
    FAttribType: TbtString;
    FOwner: TPSRuntimeAttributes;
    FAttribTypeHash: Longint;
    function GetValue(I: Longint): PIFVariant;
    function GetValueCount: Longint;
  public
    constructor Create(Owner: TPSRuntimeAttributes);
    destructor Destroy; override;

    function AddValue(aType: TPSTypeRec): PPSVariant;
    procedure DeleteValue(i: Longint);
    procedure AdjustSize;

    property Owner: TPSRuntimeAttributes read FOwner;
    property AttribType: TbtString read FAttribType write FAttribType;
    property AttribTypeHash: Longint read FAttribTypeHash write FAttribTypeHash;
    property ValueCount: Longint read GetValueCount;
    property Value[I: Longint]: PIFVariant read GetValue;
  end;

  TPSRuntimeAttributes = class(TObject)
  private
    FAttributes: TPSList;
    FOwner: TPSExec;
    function GetCount: Longint;
    function GetItem(I: Longint): TPSRuntimeAttribute;
  public
    constructor Create(AOwner: TPSExec);
    destructor Destroy; override;

    procedure Delete(I: Longint);
    function Add: TPSRuntimeAttribute;
    function FindAttribute(const Name: TbtString): TPSRuntimeAttribute;

    property Owner: TPSExec read FOwner;
    property Count: Longint read GetCount;
    property Items[I: Longint]: TPSRuntimeAttribute read GetItem; default;
  end;
  TPSOnGetNVariant = function (Sender: TPSExec; const Name: TbtString): Variant;
  TPSOnSetNVariant = procedure (Sender: TPSExec; const Name: TbtString; V: Variant);

  TPSOnLineEvent = procedure(Sender: TPSExec);
  TPSOnSpecialProcImport = function (Sender: TPSExec; p: TPSExternalProcRec; Tag: Pointer): Boolean;
  TPSOnException = procedure (Sender: TPSExec; ExError: TPSError; const ExParam: TbtString; ExObject: TObject; ProcNo, Position: Cardinal);

  {+}
  TLoadDebugInfoEvent = procedure(Sender: TPSExec; var OK: Boolean) of object;
  {+.}
  TPSExec = class(TObject)
  private
    FOnGetNVariant: TPSOnGetNVariant;
    FOnSetNVariant: TPSOnSetNVariant;
    FId: Pointer;
    FJumpFlag: Boolean;
    FCallCleanup: Boolean;
    FOnException: TPSOnException;
    {+}
    FOnLoadDebugInfo: TLoadDebugInfoEvent;
    {+.}
    function ReadData(var Data; Len: Cardinal): Boolean;
    function ReadLong(var b: Cardinal): Boolean;
    function DoCalc(var1, Var2: Pointer; var1Type, var2type: TPSTypeRec; CalcType: Cardinal): Boolean;
    function DoBooleanCalc(var1, Var2, into: Pointer; var1Type, var2type, intotype: TPSTypeRec; Cmd: Cardinal): Boolean;
    function SetVariantValue(Dest, Src: Pointer; destType, srcType: TPSTypeRec{+}; {%H-}sd: PPSResultData = nil{+.}): Boolean;
    function ReadVariable(var Dest: TPSResultData; UsePointer: Boolean): Boolean;
    function DoBooleanNot(Dta: Pointer; aType: TPSTypeRec): Boolean;
    function DoMinus(Dta: Pointer; aType: TPSTypeRec): Boolean;
    function DoIntegerNot(Dta: Pointer; aType: TPSTypeRec): Boolean;
    procedure RegisterStandardProcs;
  protected
    FReturnAddressType: TPSTypeRec;
    FVariantType: TPSTypeRec;
    FVariantArrayType: TPSTypeRec;
    FAttributeTypes: TPSList;
    FExceptionStack: TPSList;
    FResources: TPSList;
    FExportedVars: TPSList;
    FTypes: TPSList;
    FProcs: TPSList;
    FGlobalVars: TPSStack;
    FTempVars: TPSStack;
    FStack: TPSStack;
    FMainProc: Cardinal;
    FStatus: TPSStatus;
    FCurrProc: TPSInternalProcRec;
    FData: PByteArray;
    FDataLength: Cardinal;
    FCurrentPosition: Cardinal;
    FCurrStackBase: Cardinal;
    FOnRunLine: TPSOnLineEvent;
    FSpecialProcList: TPSList;
    FRegProcs: TPSList;
    ExObject: TObject;
    ExProc: Cardinal;
    ExPos: Cardinal;
    ExEx: TPSError;
    ExParam: TbtString;
    {+}
    FCurrentRow, FCurrentCol: Cardinal;
    FCurrentFile: TbtString;
    {+.}

    function InvokeExternalMethod(At: TPSTypeRec_ProcPtr; Slf, Ptr: Pointer): Boolean;
    function InnerfuseCall(_Self, Address: Pointer; CallingConv: TPSCallingConvention; Params: TPSList; res: PPSVariantIFC): Boolean;
    procedure RunLine; virtual;
    function ImportProc(const Name: ShortString; proc: TPSExternalProcRec): Boolean; Virtual;
    procedure ExceptionProc(proc, Position: Cardinal; Ex: TPSError; const s: TbtString; NewObject: TObject); Virtual;
    function FindSpecialProcImport(P: TPSOnSpecialProcImport): pointer;
  public
    function LastEx: TPSError;
    function LastExParam: TbtString;
    function LastExProc: Integer;
    function LastExPos: {+}Cardinal{+.};
    function LastExObject: TObject;

    procedure CMD_Err(EC: TPSError); {$ifdef _inline_}inline;{$endif}
    procedure CMD_Err2(EC: TPSError; const Param: TbtString); {$ifdef _inline_}{$ifndef fpc}inline;{$endif}{$endif}
    procedure CMD_Err3(EC: TPSError; const Param: TbtString; ExObject: TObject);

    property Id: Pointer read FID write FID;

    class function About: TbtString;

    function RunProc(Params: TPSList; ProcNo: Cardinal): Boolean;

    function RunProcP(const Params: array of Variant; const Procno: Cardinal): Variant;
    function RunProcPVar(var Params: array of Variant; const Procno: Cardinal): Variant;
    function RunProcPN(const Params: array of Variant; const ProcName: TbtString): Variant;

    function FindType(StartAt: Cardinal; BaseType: TPSBaseType; var l: Cardinal): PIFTypeRec;
    function FindType2(BaseType: TPSBaseType): PIFTypeRec;

    function GetTypeNo(l: Cardinal): PIFTypeRec;
    function GetType(const Name: TbtString): Cardinal;

    function GetProc(const Name: TbtString): Cardinal;
    function GetVar(const Name: TbtString): Cardinal;
    function GetVar2(const Name: TbtString): PIFVariant;
    function GetVarNo(C: Cardinal): PIFVariant;
    function GetProcNo(C: Cardinal): PIFProcRec;
    function GetProcCount: Cardinal;
    function GetVarCount: Longint;
    function GetTypeCount: Longint;

    {+}
    function GetCallStack(var Count: Cardinal): TbtString; virtual;
    {+.}

    constructor Create;
    destructor Destroy; Override;

    function RunScript: Boolean;

    function LoadData(const S: TbtString): Boolean; virtual;

    {+}
    //-function LoadDebugData(const Data: TbtString): Longint; virtual;
    function LoadDebugInfo(): Boolean; // Allows downloading of debugging information dynamically as needed!
    property OnLoadDebugInfo: TLoadDebugInfoEvent read FOnLoadDebugInfo write FOnLoadDebugInfo;
    function TranslatePosition(Proc, Position: Cardinal): Cardinal; //virtual;
    function TranslatePositionEx({%H-}Proc, {%H-}Position: Cardinal; var {%H-}Pos, Row, Col: Cardinal; var Fn: TbtString): Boolean; virtual;
    function GetCurrentPositionDebugInfo(const sPrefix: TbtString = ''): TbtString;
    {+.}

    procedure Clear; virtual;
    procedure Cleanup; virtual;
    procedure Stop; virtual;
    procedure Pause; virtual;

    property CallCleanup: Boolean read FCallCleanup write FCallCleanup;
    property Status: TPSStatus Read FStatus;
    property OnRunLine: TPSOnLineEvent Read FOnRunLine Write FOnRunLine;

    procedure ClearspecialProcImports;
    procedure AddSpecialProcImport(const FName: TbtString; P: TPSOnSpecialProcImport; Tag: Pointer);

    function RegisterFunctionName(const Name: TbtString; ProcPtr: TPSProcPtr; Ext1, Ext2: Pointer): PProcRec;
    procedure RegisterDelphiFunction(ProcPtr: Pointer; const Name: TbtString; CC: TPSCallingConvention);
    procedure RegisterDelphiMethod(ASelf, ProcPtr: Pointer; const Name: TbtString; CC: TPSCallingConvention);
    procedure RegisterAttributeType(UseProc: TPSAttributeUseProc; const TypeName: TbtString);

    function GetProcAsMethod(const ProcNo: Cardinal): TMethod;
    function GetProcAsMethodN(const ProcName: TbtString): TMethod;

    procedure ClearFunctionList;

    property ExceptionProcNo: Cardinal Read ExProc;
    property ExceptionPos: Cardinal Read ExPos;
    property ExceptionCode: TPSError Read ExEx;
    property ExceptionString: TbtString read ExParam;
    property ExceptionObject: TObject read ExObject write ExObject;

    procedure AddResource(Proc, P: Pointer);

    function IsValidResource(Proc, P: Pointer): Boolean;
    procedure DeleteResource(P: Pointer);
    function FindProcResource(Proc: Pointer): Pointer;
    function FindProcResource2(Proc: Pointer; var StartAt: Longint): Pointer;

    procedure RaiseCurrentException;

    property OnException: TPSOnException read FOnException write FOnException;
    property OnGetNVariant: TPSOnGetNVariant read FOnGetNVariant write FOnGetNVariant;
    property OnSetNVariant: TPSOnSetNVariant read FOnSetNVariant write FOnSetNVariant;
  end;

  TPSStack = class(TPSList)
  private
    FDataPtr: Pointer;
    FCapacity,
    FLength: Longint;
    function GetItem(I: Longint): PPSVariant;
    procedure SetCapacity(const Value: Longint);
    procedure AdjustLength;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear; {$IFDEF DELPHI5UP} reintroduce;{$ELSE} override; {$ENDIF}
    function Push(TotalSize: Longint): PPSVariant;
    function PushType(aType: TPSTypeRec): PPSVariant;
    procedure Pop;
    function GetInt(ItemNo: Longint): Longint;
    function GetUInt(ItemNo: Longint): Cardinal;
    {$IFNDEF PS_NOINT64}
    function GetInt64(ItemNo: Longint): Int64;
    {$ENDIF}
    function GetString(ItemNo: Longint): string; // calls the native method
    function GetAnsiString(ItemNo: Longint): TbtString;
    {$IFNDEF PS_NOWIDESTRING}
    function GetWideString(ItemNo: Longint): tbtWideString;
    {$ENDIF}
    function GetUnicodeString(ItemNo: Longint): TbtUnicodeString;
    function GetReal(ItemNo: Longint): Extended;
    function GetCurrency(ItemNo: Longint): Currency;
    function GetBool(ItemNo: Longint): Boolean;
    function GetClass(ItemNo: Longint): TObject;

    procedure SetInt(ItemNo: Longint; const Data: Longint);
    procedure SetUInt(ItemNo: Longint; const Data: Cardinal);
    {$IFNDEF PS_NOINT64}
    procedure SetInt64(ItemNo: Longint; const Data: Int64);
    {$ENDIF}
    procedure SetString(ItemNo: Longint; const Data: string);
    procedure SetAnsiString(ItemNo: Longint; const Data: TbtString);
    {$IFNDEF PS_NOWIDESTRING}
    procedure SetWideString(ItemNo: Longint; const Data: tbtWideString);
    {$ENDIF}
    procedure SetUnicodeString(ItemNo: Longint; const Data: TbtUnicodeString);
    procedure SetReal(ItemNo: Longint; const Data: Extended);
    procedure SetCurrency(ItemNo: Longint; const Data: Currency);
    procedure SetBool(ItemNo: Longint; const Data: Boolean);
    procedure SetClass(ItemNo: Longint; const Data: TObject);

    property DataPtr: Pointer read FDataPtr;
    property Capacity: Longint read FCapacity write SetCapacity;
    property Length: Longint read FLength;
    property Items[I: Longint]: PPSVariant read GetItem; default;
  end;

function PSErrorToString(x: TPSError; const Param: TbtString): TbtString;
function TIFErrorToString(x: TPSError; const Param: TbtString): TbtString;
function CreateHeapVariant(aType: TPSTypeRec): PPSVariant;
procedure DestroyHeapVariant(v: PPSVariant);

procedure FreePIFVariantList(l: TPSList);
procedure FreePSVariantList(l: TPSList);

const
  ENoError = ERNoError;

function PIFVariantToVariant(Src: PIFVariant; var Dest: Variant): Boolean;
function VariantToPIFVariant(Exec: TPSExec; const Src: Variant; Dest: PIFVariant): Boolean;

function PSGetRecField(const avar: TPSVariantIFC; Fieldno: Longint): TPSVariantIFC;
function PSGetArrayField(const avar: TPSVariantIFC; Fieldno: Longint): TPSVariantIFC;
function NewTPSVariantRecordIFC(avar: PPSVariant; Fieldno: Longint): TPSVariantIFC;

function NewTPSVariantIFC(avar: PPSVariant; VarParam: boolean): TPSVariantIFC;

function NewPPSVariantIFC(avar: PPSVariant; VarParam: boolean): PPSVariantIFC;

procedure DisposePPSVariantIFC(aVar: PPSVariantIFC);

procedure DisposePPSVariantIFCList(list: TPSList);

function PSGetObject(Src: Pointer; aType: TPSTypeRec): TObject;
function PSGetUInt(Src: Pointer; aType: TPSTypeRec): Cardinal;
{$IFNDEF PS_NOINT64}
function PSGetInt64(Src: Pointer; aType: TPSTypeRec): Int64;
{$ENDIF}
function PSGetReal(Src: Pointer; aType: TPSTypeRec): Extended;
function PSGetCurrency(Src: Pointer; aType: TPSTypeRec): Currency;
function PSGetInt(Src: Pointer; aType: TPSTypeRec): Longint;
function PSGetString(Src: Pointer; aType: TPSTypeRec): string;
{+}
function PSGetAnsiChar(Src: Pointer; aType: TPSTypeRec): tbtchar;
{+.}
function PSGetAnsiString(Src: Pointer; aType: TPSTypeRec): TbtString;
{$IFNDEF PS_NOWIDESTRING}
{+}
function PSGetWideChar(Src: Pointer; aType: TPSTypeRec): TbtWideChar;
{+.}
function PSGetWideString(Src: Pointer; aType: TPSTypeRec): tbtWideString;
{$ENDIF}
function PSGetUnicodeString(Src: Pointer; aType: TPSTypeRec): TbtUnicodeString;

procedure PSSetObject(Src: Pointer; aType: TPSTypeRec; var {%H-}Ok: Boolean; Const val: TObject);
procedure PSSetUInt(Src: Pointer; aType: TPSTypeRec; var Ok: Boolean; const Val: Cardinal);
{$IFNDEF PS_NOINT64}
procedure PSSetInt64(Src: Pointer; aType: TPSTypeRec; var Ok: Boolean; const Val: Int64);
{$ENDIF}
procedure PSSetReal(Src: Pointer; aType: TPSTypeRec; var Ok: Boolean; const Val: Extended);
procedure PSSetCurrency(Src: Pointer; aType: TPSTypeRec; var Ok: Boolean; const Val: Currency);
procedure PSSetInt(Src: Pointer; aType: TPSTypeRec; var Ok: Boolean; const Val: Longint);
procedure PSSetString(Src: Pointer; aType: TPSTypeRec; var Ok: Boolean; const Val: String);
procedure PSSetAnsiString(Src: Pointer; aType: TPSTypeRec; var Ok: Boolean; const Val: TbtString);
{$IFNDEF PS_NOWIDESTRING}
procedure PSSetWideString(Src: Pointer; aType: TPSTypeRec; var Ok: Boolean; const Val: tbtWideString);
{$ENDIF}
procedure PSSetUnicodeString(Src: Pointer; aType: TPSTypeRec; var Ok: Boolean; const Val: TbtUnicodeString);

procedure VNSetPointerTo(const Src: TPSVariantIFC; Data: Pointer; aType: TPSTypeRec);

function VNGetUInt(const Src: TPSVariantIFC): Cardinal;
{$IFNDEF PS_NOINT64}
function VNGetInt64(const Src: TPSVariantIFC): Int64;
{$ENDIF}
function VNGetReal(const Src: TPSVariantIFC): Extended;
function VNGetCurrency(const Src: TPSVariantIFC): Currency;
function VNGetInt(const Src: TPSVariantIFC): Longint;
function VNGetString(const Src: TPSVariantIFC): String;
function VNGetAnsiString(const Src: TPSVariantIFC): TbtString;
{$IFNDEF PS_NOWIDESTRING}
function VNGetWideString(const Src: TPSVariantIFC): tbtWideString;
function VNGetUnicodeString(const Src: TPSVariantIFC): TbtUnicodeString;
{$ENDIF}

procedure VNSetUInt(const Src: TPSVariantIFC; const Val: Cardinal);
{$IFNDEF PS_NOINT64}
procedure VNSetInt64(const Src: TPSVariantIFC; const Val: Int64);
{$ENDIF}
procedure VNSetReal(const Src: TPSVariantIFC; const Val: Extended);
procedure VNSetCurrency(const Src: TPSVariantIFC; const Val: Currency);
procedure VNSetInt(const Src: TPSVariantIFC; const Val: Longint);
procedure VNSetString(const Src: TPSVariantIFC; const Val: String);
procedure VNSetAnsiString(const Src: TPSVariantIFC; const Val: TbtString);
{$IFNDEF PS_NOWIDESTRING}
procedure VNSetWideString(const Src: TPSVariantIFC; const Val: tbtWideString);
procedure VNSetUnicodeString(const Src: TPSVariantIFC; const Val: TbtUnicodeString);
{$ENDIF}

function VGetUInt(const Src: PIFVariant): Cardinal;
{$IFNDEF PS_NOINT64}
function VGetInt64(const Src: PIFVariant): Int64;
{$ENDIF}
function VGetReal(const Src: PIFVariant): Extended;
function VGetCurrency(const Src: PIFVariant): Currency;
function VGetInt(const Src: PIFVariant): Longint;
function VGetString(const Src: PIFVariant): String;
function VGetAnsiString(const Src: PIFVariant): TbtString;
{$IFNDEF PS_NOWIDESTRING}
function VGetWideString(const Src: PIFVariant): tbtWideString;
function VGetUnicodeString(const Src: PIFVariant): TbtUnicodeString;
{$ENDIF}

procedure VSetPointerTo(const Src: PIFVariant; Data: Pointer; aType: TPSTypeRec);
procedure VSetUInt(const Src: PIFVariant; const Val: Cardinal);
{$IFNDEF PS_NOINT64}
procedure VSetInt64(const Src: PIFVariant; const Val: Int64);
{$ENDIF}
procedure VSetReal(const Src: PIFVariant; const Val: Extended);
procedure VSetCurrency(const Src: PIFVariant; const Val: Currency);
procedure VSetInt(const Src: PIFVariant; const Val: Longint);
procedure VSetString(const Src: PIFVariant; const Val: string);
procedure VSetAnsiString(const Src: PIFVariant; const Val: TbtString);
{$IFNDEF PS_NOWIDESTRING}
procedure VSetWideString(const Src: PIFVariant; const Val: tbtWideString);
procedure VSetUnicodeString(const Src: PIFVariant; const Val: TbtUnicodeString);
{$ENDIF}

type
  {+}
  EPSError = uPSUtils.EPSError;

  EPSException = class(EPSError)
  {+.}
  private
    FProcPos: Cardinal;
    FProcNo: Cardinal;
    FExec: TPSExec;
  public
    constructor Create(const Error: TbtString; Exec: TPSExec; Procno, ProcPos: Cardinal);

    property ProcNo: Cardinal read FProcNo;
    property ProcPos: Cardinal read FProcPos;
    property Exec: TPSExec read FExec;
  end;

  TPSRuntimeClass = class
  protected
    FClassName: TbtString;
    FClassNameHash: Longint;
    FClassItems: TPSList;
    FClass: TClass;
    FEndOfVmt: Longint;
    {+}
    procedure NewPClassItem(var P: PClassItem);
    {+.}
  public
    constructor Create(aClass: TClass; const AName: TbtString);
    destructor Destroy; override;

    procedure RegisterConstructor(ProcPtr: Pointer; const Name: TbtString);
    procedure RegisterVirtualConstructor(ProcPtr: Pointer; const Name: TbtString);
    {+}
    procedure RegisterVirtualConstructorWrapper(ProcPtr, AClassType: Pointer; const Name: TbtString);
    {+.}
    procedure RegisterMethod(ProcPtr: Pointer; const Name: TbtString);
    procedure RegisterMethodName(const Name: TbtString; ProcPtr: TPSProcPtr; Ext1, Ext2: Pointer);
    procedure RegisterVirtualMethod(ProcPtr: Pointer; const Name: TbtString);
    {+}
    procedure RegisterVirtualMethodWrapper(ProcPtr, AClassType: Pointer; const Name: TbtString);
    {+.}
    procedure RegisterVirtualAbstractMethod(ClassDef: TClass; ProcPtr: Pointer; const Name: TbtString);
    procedure RegisterPropertyHelper(ReadFunc, WriteFunc: Pointer; const Name: TbtString);
    procedure RegisterPropertyHelperName(ReadFunc, WriteFunc: Pointer; const Name: TbtString);
    procedure RegisterPropertyNameHelper(const Name: TbtString; ProcPtr: TPSProcPtr;
      ExtRead1, ExtRead2, ExtWrite1, ExtWrite2: Pointer); overload;
    procedure RegisterPropertyNameHelper(const Name: TbtString; ProcReadPtr, ProcWritePtr: TPSProcPtr;
      ExtRead1, ExtRead2, ExtWrite1, ExtWrite2: Pointer); overload;
    procedure RegisterEventPropertyHelper(ReadFunc, WriteFunc: Pointer; const Name: TbtString);
  end;

  TPSRuntimeClassImporter = class
  private
    FClasses: TPSList;
  public
    constructor Create;
    constructor CreateAndRegister(Exec: TPSExec; AutoFree: Boolean);
    destructor Destroy; override;

    function Add(aClass: TClass): TPSRuntimeClass;
    function Add2(aClass: TClass; const Name: TbtString): TPSRuntimeClass;
    procedure Clear;
    function FindClass(const Name: TbtString): TPSRuntimeClass;
  end;
  TIFPSRuntimeClassImporter = TPSRuntimeClassImporter;
  TPSResourceFreeProc = procedure (Sender: TPSExec; P: TPSRuntimeClassImporter);

procedure RegisterClassLibraryRuntime(SE: TPSExec; Importer: TPSRuntimeClassImporter);

procedure SetVariantToClass(V: PIFVariant; Cl: TObject);
{$IFNDEF PS_NOINTERFACES}
procedure SetVariantToInterface(V: PIFVariant; Cl: IUnknown);
{$ENDIF}

procedure MyAllMethodsHandler;
function GetMethodInfoRec(SE: TPSExec; ProcNo: Cardinal): Pointer;
function MkMethod(FSE: TPSExec; No: Cardinal): TMethod;

type
  TIFInternalProcRec = TPSInternalProcRec;
  TIFError = TPSError;
  TIFStatus = TPSStatus;
  TIFPSExec = TPSExec;
  TIFPSStack = TPSStack;
  TIFTypeRec = TPSTypeRec;

  TPSCallingConvention = uPSUtils.TPSCallingConvention;
const
  cdRegister = uPSUtils.cdRegister;
  cdPascal = uPSUtils.cdPascal;
  cdCdecl = uPSUtils.cdCdecl;
  cdStdCall = uPSUtils.cdStdCall;
  InvalidVal = Cardinal(-1);

function  PSDynArrayGetLength(arr: Pointer; aType: TPSTypeRec): Longint;
procedure PSDynArraySetLength(var arr: Pointer; aType: TPSTypeRec; NewLength: Longint);

function  GetPSArrayLength(Arr: PIFVariant): Longint;
procedure SetPSArrayLength(Arr: PIFVariant; NewLength: Longint);

function PSVariantToString(const p: TPSVariantIFC; const ClassProperties: TbtString): TbtString;
function MakeString(const s: TbtString): TbtString;
{$IFNDEF PS_NOWIDESTRING}
function MakeWString(const s: TbtUnicodeString): TbtString; {$ifdef _inline_}inline;{$endif}
{$ENDIF}

{$IFNDEF PS_NOIDISPATCH}
function IDispatchInvoke(Self: IDispatch; PropertySet: Boolean; const Name: string;
  const Par: array of Variant): Variant;
{$ENDIF}

implementation
uses
  {+}
  {$IFNDEF PS_NOINTERFACES}
    {$IFDEF CONDITIONALEXPRESSIONS}
  VarUtils,
    {$ENDIF CONDITIONALEXPRESSIONS}
  {$ENDIF !PS_NOINTERFACES}
  //
  {$IFNDEF PS_NOWIDESTRING}
  {$IFDEF FPC}
  StrUtils, {$DEFINE _STRUTILS_}
  {$ELSE}
  WideStrUtils,
  {$ENDIF !FPC}
  {$ENDIF !PS_NOWIDESTRING}
  TypInfo
  {$IFDEF DELPHI3UP} // or FPC
    {$IFNDEF FPC}
      {$IFDEF MSWINDOWS}
  ,ComObj
      {$ENDIF}
    {$ENDIF !FPC}
  {$ENDIF DELPHI3UP}
  //
  {$IFDEF FPC}{$IFDEF PS_FPC_HAS_COM}
  ,ComObj
  {$ENDIF}{$ENDIF FPC}
  {$IF (NOT DEFINED (NEXTGEN)) AND (NOT DEFINED (MACOS)) AND DEFINED(DELPHI17UP)} // DELPHI17UP == DELPHIXE4UP
  {$DEFINE _ANSISTRINGS_}
  ,AnsiStrings
  {$IFEND}
  //{$IF DEFINED(DELPHI11UP) or DEFINED(FPC)} // TODO: FPC need check
  //{$DEFINE _STRUTILS_}
  //,StrUtils
  //{$IFEND}
  {+.}
  ;

{$if (defined(DELPHI3UP) or defined(FPC))}
resourcestring
{$else}
const
{$ifend}
  RPS_UnknownIdentifier = 'Unknown Identifier';
  RPS_Exception = 'Exception: %s';
  RPS_Invalid = '[Invalid]';

  //- PSErrorToString
  RPS_NoError = 'No Error';
  RPS_CannotImport = 'Cannot Import %s';
  RPS_InvalidType = 'Invalid Type';
  RPS_InternalError = 'Internal error';
  RPS_InvalidHeader = 'Invalid Header';
  RPS_InvalidOpcode = 'Invalid Opcode';
  RPS_InvalidOpcodeParameter = 'Invalid Opcode Parameter';
  RPS_NoMainProc = 'no Main Proc';
  RPS_OutOfGlobalVarsRange = 'Out of Global Vars range';
  RPS_OutOfProcRange = 'Out of Proc Range';
  RPS_OutOfRange = 'Out Of Range';
  {+}
  RPS_OutOfRangeEx = 'Out Of Range! Element index is out of Array range: Element Index is %d, Array length = %d';
  {+.}
  RPS_OutOfStackRange = 'Out Of Stack Range';
  RPS_TypeMismatch = 'Type Mismatch';
  RPS_UnexpectedEof = 'Unexpected End Of File';
  RPS_VersionError = 'Version error';
  RPS_DivideByZero = 'divide by Zero';
  RPS_MathError = 'Math error';
  RPS_CouldNotCallProc = 'Could not call proc';
  RPS_OutofRecordRange = 'Out of Record Fields Range';
  RPS_NullPointerException = 'Null Pointer Exception';
  RPS_NullVariantError = 'Null variant error';
  RPS_OutOfMemory = 'Out Of Memory';
  RPS_InterfaceNotSupported = 'Interface not supported';
  RPS_UnknownError = 'Unknown error';

  RPS_InvalidVariable = 'Invalid variable';
  RPS_InvalidArray = 'Invalid array';
  RPS_OLEError = 'OLE error %.8x';
  RPS_UnknownProcedure = 'Unknown procedure';
  RPS_NotEnoughParameters = 'Not enough parameters';
  RPS_InvalidParameter = 'Invalid parameter';
  RPS_TooManyParameters = 'Too many parameters';
  RPS_OutOfStringRange = 'Out of string range';
  RPS_CannotCastInterface = 'Cannot cast an interface';
  RPS_CannotCastObject = 'Cannot cast an object';
  RPS_CapacityLength = 'Capacity < Length';
  RPS_CanOnlySendLastItem = 'Can only remove last item from stack';
{%H-}RPS_NILInterfaceException = 'Nil interface';
{%H-}RPS_UnknownMethod = 'Unknown method';

{+}
type
  ExceptionBase = Exception;
  Exception = EPSError;
{+.}

type
  TPSExportedVar = record
    FName: TbtString;
    FNameHash: Longint;
    FVarNo: Cardinal;
  end;
  PPSExportedVar = ^TPSExportedVar;

  PRaiseFrame = ^TRaiseFrame;
  TRaiseFrame = record
    NextRaise: PRaiseFrame;
    ExceptAddr: Pointer;
    ExceptObject: TObject;
    ExceptionRecord: Pointer;
  end;

  TPSExceptionHandler = class
    CurrProc: TPSInternalProcRec;
    BasePtr, StackSize: Cardinal;
    FinallyOffset, ExceptOffset, Finally2Offset, EndOfBlock: Cardinal;
    ExceptionData: TPSError;
    ExceptionObject: TObject;
    ExceptionParam: TbtString;

    destructor Destroy; override;
  end;

  TPSType = packed record
    BaseType: TPSBaseType;
  end;

  TPSProc = packed record
    Flags: Byte;
  end;

  TPSVar = packed record
    TypeNo: Cardinal;
    Flags: Byte;
  end;

  TSpecialProc = record
    P: TPSOnSpecialProcImport;
    namehash: Longint;
    Name: TbtString;
    tag: pointer;
  end;
  PSpecialProc = ^TSpecialProc;

{ TPSExceptionHandler }

destructor TPSExceptionHandler.Destroy;
begin
  FreeAndNil(ExceptionObject);
  inherited;
end;

{$IFDEF FPC}
  {$DEFINE FPC_OR_KYLIX}
{$ELSE}
  {$IFDEF KYLIX}
    {$DEFINE FPC_OR_KYLIX}
  {$ENDIF}
{$ENDIF}

{$IFDEF FPC_OR_KYLIX}
function OleErrorMessage(ErrorCode: HResult): TbtString;
begin
  Result := SysErrorMessage(ErrorCode);
  if Result = '' then
    Result := Format(RPS_OLEError, [ErrorCode]);
end;

procedure OleError(ErrorCode: HResult);
begin
  raise Exception.Create(OleErrorMessage(ErrorCode));
end;

procedure OleCheck(Result: HResult);
begin
  if Result < 0 then
    OleError(Result);
end;
{$ENDIF FPC_OR_KYLIX}

{$IFNDEF DELPHI3UP} // and not FPC
function OleErrorMessage(ErrorCode: HResult): TbtString;
begin
  Result := SysErrorMessage(ErrorCode);
  if Result = '' then
    Result := Format(RPS_OLEError, [ErrorCode]);
end;

procedure OleError(ErrorCode: HResult);
begin
  raise Exception.Create(OleErrorMessage(ErrorCode));
end;

procedure OleCheck(Result: HResult);
begin
  if Result < 0 then
    OleError(Result);
end;

procedure AssignInterface(var Dest: IUnknown; const Src: IUnknown);
var OldDest: IUnknown;
begin
  { Like Delphi 3+'s _IntfCopy, reference source before releasing old dest.
    so that self assignment (I := I) works right }
  OldDest := Dest;
  Dest := Src;
  if Src <> nil then
    Src.AddRef;
  if OldDest <> nil then
    OldDest.Release;
end;

procedure AssignVariantFromIDispatch(var Dest: Variant; const Src: IDispatch);
begin
  VarClear(Dest);
  TVarData(Dest).VDispatch := Src;
  TVarData(Dest).VType := varDispatch;
  if Src <> nil then
    Src.AddRef;
end;

procedure AssignIDispatchFromVariant(var Dest: IDispatch; const Src: Variant);
const RPS_InvalidVariantRef = 'Invalid variant ref';
var   NewDest: IDispatch;
begin
  case TVarData(Src).VType of
    varEmpty: NewDest := nil;
    varDispatch: NewDest := TVarData(Src).VDispatch;
    varDispatch or varByRef: NewDest := Pointer(TVarData(Src).VPointer^);
    else
      raise Exception.Create(RPS_InvalidVariantRef);
  end;
  AssignInterface(IUnknown(Dest), NewDest);
end;
{$ENDIF !DELPHI3UP}

{+}
type
  EPSIntf = class(Exception) // TODO: ?EPSIntf = class(EPSError)
    {$IFDEF UNICODE}
    constructor Create(const Msg: AnsiString); overload;
    {$ENDIF}
  end;

{$IFDEF UNICODE}
constructor EPSIntf.Create(const Msg: AnsiString);
begin
  inherited Create(string(Msg));
end;
{$ENDIF}

{.$IFDEF PS_HAVEVARIANT}
function VarIsEmptyOrNull(const Value: TVarData): Boolean; overload;
begin
  with Value do begin
    Result :=
      // var is not valid
      (Integer(VType) < 0) or (Integer(VType) > Integer(High(VType)))
      or (
        // var is empty or null
        (VType = varEmpty) or (VType = varNull)
        // var is unassigned dispatch
        or ((VType = varDispatch) and (VDispatch = nil))
        or ((VType = varUnknown) and (VUnknown = nil))
      );
  end;
end;

function VarIsEmptyOrNull(const Value: Variant): Boolean; overload; inline;
begin
  //Result := {(not VarIsValid(Value)) or} VarIsEmpty(Value) or VarIsNull(Value);
  //if (not Result) and (TVarData(Value).VType = varDispatch) then
  //  Result := TVarData(Value).VDispatch = nil;
  Result := VarIsEmptyOrNull(TVarData(Value));
end;

{.$IFNDEF _VARIANTS_}
function VarToStr(const V: Variant): string;
begin
  //-if not VarIsNull(V) then
  if not VarIsEmptyOrNull(V) then
    Result := V
  else
    Result := '';
end;

{.$ENDIF !_VARIANTS_}
{.$ENDIF PS_HAVEVARIANT}

{$IFNDEF PS_NOINTERFACES}
const
  IUnknown_GUID: TGUID = '{00000000-0000-0000-C000-000000000046}';

{$UNDEF _DEBUG_}
{.$DEFINE _DEBUG_}
{$IFDEF _DEBUG_}
function DbgRefCount(const V: IUnknown): Integer; overload; forward;
function DbgRefCount(const V: Pointer): Integer; overload; forward;
function DbgRefCount(const V: TVarData): Integer; overload; forward;
function DbgRefCount(const V: Variant): Integer; overload; forward;
{$ENDIF _DEBUG_}

{$IFDEF CONDITIONALEXPRESSIONS}
procedure AnyToIntf(var Intf: IUnknown; const V: TVarData);
var LTemp: TVarData;
begin
  Intf := nil;
  {VarUtils.}VariantInit({%H-}LTemp);
  try
    {VarUtils.}VariantCopy(LTemp, V);
    {Variants.}ChangeAnyProc(LTemp);
    case LTemp.VType of
      varUnknown:
        if Assigned(LTemp.VUnknown) then
          Intf := IUnknown(LTemp.VUnknown);
      varUnknown + varByRef:
        if Assigned(LTemp.VPointer) then
          Intf := IUnknown(LTemp.VPointer^);
    end;
  finally
    {VarUtils.}VariantClear(LTemp);
  end;
end;
{$ENDIF CONDITIONALEXPRESSIONS}

function DispIsEmpty(const Value: TVarData): Boolean; overload;
begin
  with Value do begin
    Result :=
      // var is not valid
      (Integer(VType) < 0) or (Integer(VType) > Integer(High(VType)))
      or (
        {// var is empty or null
        (VType = varEmpty) or (VType = varNull)
        // var is unassigned dispatch
        or} ((VType = varDispatch) and (VDispatch = nil))
        or ((VType = varUnknown) and (VUnknown = nil))
      );
  end;
end;

function DispIsEmpty(const Value: Variant): Boolean; overload; inline;
begin
  //Result := {(not VarIsValid(Value)) or} VarIsEmpty(Value) or VarIsNull(Value);
  //if (not Result) and (TVarData(Value).VType = varDispatch) then
  //  Result := TVarData(Value).VDispatch = nil;
  Result := VarIsEmptyOrNull(TVarData(Value));
end;

function VarToIInterface(const Value: TVarData; out Obj): Boolean;
var Intf: IUnknown absolute Obj;
begin
  Pointer(Obj) := nil;
  with Value do begin
    case VType of
      varUnknown, varDispatch:
        if Assigned(VUnknown) then
          Intf := IUnknown(VUnknown);
      varUnknown + varByRef, varDispatch + varByRef:
        if Assigned(VPointer) then
          Intf := IUnknown(VPointer^);
       {$IFDEF CONDITIONALEXPRESSIONS}
       varAny:
         AnyToIntf(Intf, Value);
       {$ENDIF}
    end;
    Result := Assigned(Intf);
  end;
end;

function VarToInterfase(const Value: TVarData; IID: TGUID; out Obj): Boolean;
var AIntf: IUnknown;
begin
  AIntf := nil;
  Pointer(Obj) := nil;
  with Value do begin
    case VType of
      varUnknown, varDispatch:
        if Assigned(VUnknown) then
          AIntf := IUnknown(VUnknown);
      varUnknown + varByRef, varDispatch + varByRef:
        if Assigned(VPointer) then
          AIntf := IUnknown(VPointer^);
       {$IFDEF CONDITIONALEXPRESSIONS}
       varAny:
         AnyToIntf(AIntf, Value);
       {$ENDIF}
    end;
    Result := Assigned(AIntf) and (AIntf.QueryInterface(IID, Obj) =  S_OK) and (Pointer(Obj) <> nil);
  end;
end;

{$IFDEF _DEBUG_}
function DbgRefCount(const V: IUnknown): Integer; overload;
begin
  if Assigned(V) then begin
    V._AddRef;
    Result := V._Release;
  end else
    Result := -1;
end;

function DbgRefCount(const V: Pointer): Integer; overload;
begin
  Result := DbgRefCount(IUnknown(V));
end;

function DbgRefCount(const V: TVarData): Integer; overload;
var Obj: IUnknown;
begin
  if VarToIInterface(TVarData(V), Obj) then
    Result := DbgRefCount(Obj)
  else
    Result := -1;
end;

function DbgRefCount(const V: Variant): Integer; overload;
begin
  Result := DbgRefCount(TVarData(V));
end;
{$ENDIF _DEBUG_}

{$IFDEF PS_NOIDISPATCH}
const
  GUID_NULL: TGUID = '{00000000-0000-0000-0000-000000000000}';
{$ENDIF}

function AssignIInterfaceFromVariant(var Dest: IUnknown; const Src: Variant; const dest_giud: TGUID; AllowEmptyAssign: Boolean = True): Boolean;
var
  pDest: Pointer absolute Dest;
  Obj: Pointer;
  iObj: IUnknown;
  SrcEmpty: Boolean;
begin
  if pDest <> nil then begin
    {$IFDEF Delphi3UP} // or FPC
    Dest._Release;
    {$ELSE}
    Dest.Release;
    {$ENDIF}
  end;
  pDest := nil;
  SrcEmpty := VarIsEmptyOrNull(Src);
  {$IFDEF _DEBUG_}
  if SrcEmpty then
    writeln('Src(0>):', DbgRefCount(Src))
  else
    writeln('Src(0>): Empty')
  writeln('Dst(0>):', DbgRefCount(Dest));
  {$ENDIF _DEBUG_}
  if SrcEmpty then begin
    Result := AllowEmptyAssign and DispIsEmpty(Src);
    Exit; //!!
  end else if VarToIInterface(TVarData(Src), iObj) then begin
    {$IFDEF _DEBUG_}
    writeln('Src(1>):', DbgRefCount(Src));
    writeln('Dst(1>):', DbgRefCount(Dest));
    {$ENDIF _DEBUG_}
    //{$IFDEF _DEBUG_}
    //writeln('Src(2>):', DbgRefCount(Src));
    //{$ENDIF _DEBUG_}
    Obj := nil;
    if (not IsEqualGUID(dest_giud, IUnknown_GUID))
      and (not IsEqualGUID(dest_giud, GUID_NULL)) then begin // debug: GUIDToString(dest_giud)
      if iObj.QueryInterface(dest_giud, Obj) <>  S_OK then
        Obj := nil;
    end else
      Obj := Pointer(iObj);
    pDest := Obj;
  end;
  Result := pDest <> nil;
  if Result then begin
    {$IFDEF Delphi3UP} // or FPC
    Dest._AddRef;
    {$ELSE}
    Dest.AddRef;
    {$ENDIF}
  end;
  {$IFDEF _DEBUG_}
  writeln('Src(E<):', DbgRefCount(Src));
  writeln('Dst(E<):', DbgRefCount(Dest));
  {$ENDIF _DEBUG_}
end; // function AssignIInterfaceFromVariant

function AssignIntfFromIntf(var Dest: IUnknown; const Src: IUnknown; const dest_giud: TGUID; AllowNilAssign: Boolean = True): Boolean;
var
  pDest: Pointer absolute Dest;
  Obj: Pointer;
begin
  if pDest <> nil then begin
    {$IFDEF Delphi3UP} // or FPC
    Dest._Release;
    {$ELSE}
    Dest.Release;
    {$ENDIF}
  end;
  pDest := nil;
  if Assigned(Src) then begin
    Obj := nil;
    if not IsEqualGUID(dest_giud, GUID_NULL) then begin // debug: GUIDToString(dest_giud)
      if Src.QueryInterface(dest_giud, Obj) <>  S_OK then
        Obj := nil;
    end else
      Obj := Pointer(Src);
    pDest := Obj;
    Result := Assigned(pDest);
    if Result then begin
      {$IFDEF Delphi3UP} // or FPC
      Dest._AddRef;
      {$ELSE}
      Dest.AddRef;
      {$ENDIF}
    end;
  end else begin
    Result := AllowNilAssign;
  end;
end; // function AssignIntfFromIntf
{$ENDIF !PS_NOINTERFACES}
{+.}

procedure P_CM_A; begin end;
procedure P_CM_CA; begin end;
procedure P_CM_P; begin end;
procedure P_CM_PV; begin end;
procedure P_CM_PO; begin end;
procedure P_CM_C; begin end;
procedure P_CM_G; begin end;
procedure P_CM_CG; begin end;
procedure P_CM_CNG; begin end;
procedure P_CM_R; begin end;
procedure P_CM_ST; begin end;
procedure P_CM_PT; begin end;
procedure P_CM_CO; begin end;
procedure P_CM_CV; begin end;
procedure P_CM_SP; begin end;
procedure P_CM_BN; begin end;
procedure P_CM_VM; begin end;
procedure P_CM_SF; begin end;
procedure P_CM_FG; begin end;
procedure P_CM_PUEXH; begin end;
procedure P_CM_POEXH; begin end;
procedure P_CM_IN; begin end;
procedure P_CM_SPB; begin end;
procedure P_CM_INC; begin end;
procedure P_CM_DEC; begin end;

function IntPIFVariantToVariant(Src: pointer; aType: TPSTypeRec; var Dest: Variant): Boolean; forward;

procedure Set_Union(Dest, Src: PByteArray; ByteSize: Integer);
var i: Longint;
begin
  for i := ByteSize-1 downto 0 do
    Dest^[i] := Dest^[i] or Src^[i];
end;

procedure Set_Diff(Dest, Src: PByteArray; ByteSize: Integer);
var i: Longint;
begin
  for i := ByteSize-1 downto 0 do
    Dest^[i] := Dest^[i] and not Src^[i];
end;

procedure Set_Intersect(Dest, Src: PByteArray; ByteSize: Integer);
var i: Longint;
begin
  for i := ByteSize-1 downto 0 do
    Dest^[i] := Dest^[i] and Src^[i];
end;

procedure Set_Subset(Dest, Src: PByteArray; ByteSize: Integer; var Val: Boolean);
var i: Integer;
begin
  for i := ByteSize-1 downto 0 do begin
    if not (Src^[i] and Dest^[i] = Dest^[i]) then begin
      Val := False;
      Exit;
    end;
  end;
  Val := True;
end;

procedure Set_Equal(Dest, Src: PByteArray; ByteSize: Integer; var Val: Boolean);
var i: Longint;
begin
  for i := ByteSize-1 downto 0 do begin
    if Dest^[i] <> Src^[i] then begin
      Val := False;
      Exit;
    end;
  end;
  val := True;
end;

procedure Set_membership(Item: Longint; Src: PByteArray; var Val: Boolean);
begin
  Val := (Src^[Item shr 3] and (1 shl (Item and 7))) <> 0;
end;

procedure RCIFreeProc(Sender: TPSExec; P: TPSRuntimeClassImporter);
begin
  p.Free;
end;

function Trim(const s: TbtString): TbtString;
begin
  Result := s;
  while (Length(Result) > 0) and (Result[1] = #32) do
    Delete(Result, 1, 1);
  while (Length(Result) > 0) and (Result[Length(Result)] = #32) do
    Delete(Result, Length(Result), 1);
end;

//function FloatToStr(E: Extended): TbtString;
//begin
//  Result := TbtString(Sysutils.FloatToStr(e));
//end;

function Padl(s: TbtString; i: longInt): TbtString;
begin
  Result := TbtString(StringOfChar({$IFDEF FPC}Char{$ELSE}TbtChar{$ENDIF}(' '), i - length(s))) + s;
end;
//-------------------------------------------------------------------

function Padz(s: TbtString; i: longInt): TbtString;
begin
  Result := TbtString(StringOfChar({$IFDEF FPC}Char{$ELSE}TbtChar{$ENDIF}('0'), i - length(s))) + s;
end;
//-------------------------------------------------------------------

function Padr(s: TbtString; i: longInt): TbtString;
begin
  Result := s + TbtString(StringOfChar({$IFDEF FPC}Char{$ELSE}TbtChar{$ENDIF}(' '), i - Length(s)));
end;
//-------------------------------------------------------------------

{$IFNDEF PS_NOWIDESTRING}
function wPadl(s: TbtWideString; i: longInt): TbtWideString;
begin
  Result := TbtWideString(StringOfChar({$IFDEF FPC}Char{$ELSE}TbtWideChar{$ENDIF}(' '), i - length(s))) + s;
end;
//-------------------------------------------------------------------

function wPadz(s: TbtWideString; i: longInt): TbtWideString;
begin
  Result := TbtWideString(StringOfChar({$IFDEF FPC}Char{$ELSE}TbtWideChar{$ENDIF}('0'), i - length(s))) + s;
end;
//-------------------------------------------------------------------

function wPadr(s: TbtWideString; i: longInt): TbtWideString;
begin
  Result := s + TbtWideString(StringOfChar({$IFDEF FPC}Char{$ELSE}TbtWideChar{$ENDIF}(' '), i - Length(s)));
end;
{$ENDIF !PS_NOWIDESTRING}

function uPadl(s: TbtUnicodeString; i: longInt): TbtUnicodeString;
begin
  Result := TbtUnicodeString(StringOfChar({$IFDEF FPC}Char{$ELSE}TbtWideChar{$ENDIF}(' '), i - length(s))) + s;
end;
//-------------------------------------------------------------------

function uPadz(s: TbtUnicodeString; i: longInt): TbtUnicodeString;
begin
  Result := TbtUnicodeString(StringOfChar({$IFDEF FPC}Char{$ELSE}TbtWideChar{$ENDIF}('0'), i - length(s))) + s;
end;
//-------------------------------------------------------------------

function uPadr(s: TbtUnicodeString; i: longInt): TbtUnicodeString;
begin
  Result := s + TbtUnicodeString(StringOfChar({$IFDEF FPC}Char{$ELSE}TbtWideChar{$ENDIF}(' '), i - Length(s)));
end;

function MakeString(const s: TbtString): TbtString;
var
  i: Longint;
  e: TbtString;
  b: Boolean;
begin
  Result := s;
  i := 1;
  b := False;
  while i <= Length(Result) do begin
    if Result[i] = '''' then begin
      if not b then begin
        b := True;
        Insert('''', Result, i);
        Inc(i);
      end;
      Insert('''', Result, i);
      Inc(i, 2);
    end else if (Result[i] < #32) then begin
      e := '#'+IntToStr(Ord(Result[i]));
      Delete(Result, i, 1);
      if b then begin
        b := False;
        Insert('''', Result, i);
        Inc(i);
      end;
      Insert(e, Result, i);
      Inc(i, length(e));
    end else begin
      if not b then begin
        b := True;
        Insert('''', Result, i);
        Inc(i, 2);
      end else
        Inc(i);
    end;
  end;
  if b then
    Result := Result + '''';
  if Result = '' then
    Result := '''''';
end; // function MakeString

{$IFNDEF PS_NOWIDESTRING}
function MakeWString(const s: TbtUnicodeString): TbtString;
begin
  Result := MakeString(TbtString(s));
end;
(*function MakeWString(const s: TbtUnicodeString): TbtString;
var
  i: Longint;
  e: TbtString;
  b: Boolean;
begin
  Result := TbtString(s);
  i := 1;
  b := False;
  while i <= Length(Result) do begin
    if Result[i] = '''' then begin
      if not b then begin
        b := True;
        Insert('''', Result, i);
        Inc(i);
      end;
      Insert('''', Result, i);
      Inc(i, 2);
    end else if (Result[i] < #32) or (Result[i] > #255) then begin
      e := '#'+IntToStr(Ord(Result[i]));
      Delete(Result, i, 1);
      if b then begin
        b := False;
        Insert('''', Result, i);
        Inc(i);
      end;
      Insert(e, Result, i);
      Inc(i, length(e));
    end else begin
      if not b then begin
        b := True;
        Insert('''', Result, i);
        Inc(i, 2);
      end else
        Inc(i);
    end;
  end;
  if b then
    Result := Result + '''';
  if Result = '' then
    Result := '''''';
end; // function MakeWString *)
{$ENDIF !PS_NOWIDESTRING}

function SafeStr(const S: TbtString): TbtString;
var i: Longint;
begin
  for i := 1 to Length(S) do begin
    {$if declared(CharInSet)}
    if CharInSet(S[i],   [#0..#31]) then
    {$else}
    if (         S[i] in [#0..#31]) then
    {$ifend}
    begin
      Result := Copy(S, 1, i-1);
      Exit;
    end;
  end;
  Result := S;
end;

function PropertyToString(Instance: TObject; PName: TbtString): TbtString;
var
  s: TbtString;
  i: Longint;
  PP: PPropInfo;
begin
  if PName = '' then begin
    Result := TbtString(Instance.ClassName);
    Exit;
  end;
  while Length(PName) > 0 do begin
    i := pos(tbtChar('.'), pname);
    if i = 0 then begin
      s := Trim(PNAme);
      pname := '';
    end else begin
      s := trim(Copy(PName, 1, i-1));
      Delete(PName, 1, i);
    end;
    pp := GetPropInfo(PTypeInfo(Instance.ClassInfo), string(s));
    if pp = nil then begin
      Result := TbtString(RPS_UnknownIdentifier);
      Exit;
    end;

    case pp^.PropType^.Kind of
      tkInteger: begin
        Result := IntToStr(GetOrdProp(Instance, pp));
        Exit;
      end;
      tkChar: begin
        Result := '#'+IntToStr(GetOrdProp(Instance, pp));
        Exit;
      end;
      tkEnumeration: begin
        Result := TbtString(GetEnumName(pp^.PropType{$IFNDEF FPC}{$IFDEF DELPHI3UP}^{$ENDIF}{$ENDIF},
          GetOrdProp(Instance, pp)));
          Exit;
      end;
      tkFloat: begin
        Result := FloatToStr(GetFloatProp(Instance, PP));
        Exit;
      end;
      tkString, tkLString: begin
        Result := ''''+TbtString(GetStrProp(Instance, PP))+'''';
        Exit;
      end;
      tkSet: begin
        Result := '[Set]';
        Exit;
      end;
      tkClass: begin
        Instance := TObject(GetOrdProp(Instance, pp));
      end;
      tkMethod: begin
        Result := '[Method]';
        Exit;
      end;
      tkVariant: begin
        Result := '[Variant]';
        Exit;
      end;
      {$IFDEF DELPHI6UP} // {+} TODO: FPC test {+.}
      {$IFNDEF PS_NOWIDESTRING}
      tkWString: begin
        Result := ''''+TbtString(GetWideStrProp(Instance, pp))+'''';
        Exit;
      end;
      {$IFDEF UNICODE_OR_FPC}
      tkUString: begin
        Result := ''''+TbtString({$IFDEF DELPHI_TOKYO_UP}GetStrProp{$ELSE}GetUnicodeStrProp{$ENDIF}(Instance, pp))
          + '''';
        Exit;
      end;
      {$ENDIF UNICODE_OR_FPC}
      {$ENDIF !PS_NOWIDESTRING}
      {$ENDIF DELPHI6UP}
      else begin
        Result := '[Unknown]';
        Exit;
      end;
    end; // case
    if Instance = nil then begin
      Result := 'nil';
      Exit;
    end;
  end;
  Result := TbtString(Instance.ClassName);
end; // function PropertyToString

function ClassVariantInfo(const pvar: TPSVariantIFC; const PropertyName: TbtString): TbtString;
begin
  if pvar.aType.BaseType = btClass then
  begin
    if TObject(pvar.Dta^) = nil then
      Result := 'nil'
    else
      Result := PropertyToString(TObject(pvar.Dta^), PropertyName);
  end else if pvar.atype.basetype = btInterface then
      Result := 'Interface'
  else Result := TbtString(RPS_InvalidType);
end;

function PSVariantToString(const p: TPSVariantIFC; const ClassProperties: TbtString): TbtString;
var i, n: Longint;
begin
  if p.Dta = nil then begin
    Result := 'nil';
    Exit;
  end;
  if (p.aType.BaseType = btVariant) then begin
    try
      if TVarData(p.Dta^).VType = varDispatch then
        Result := 'Variant(IDispatch)'
      else if TVarData(p.Dta^).VType = varNull then
        Result := 'Null'
      else if (TVarData(p.Dta^).VType = varOleStr) then
      {$IFDEF PS_NOWIDESTRING}
        Result := MakeString(Variant(p.Dta^))
      {$ELSE}
        Result := MakeWString(Variant(p.dta^))
      {$ENDIF}
      else if TVarData(p.Dta^).VType = varString then
        Result := MakeString(TbtString(VarToStr(Variant(p.Dta^))))
      else
        Result := TbtString(VarToStr(Variant(p.Dta^)));
    except
      on e: {+}{Exception}ExceptionBase{+.} do
        Result := TbtString(Format(RPS_Exception, [e.Message]));
    end;
    Exit;
  end;
  case p.aType.BaseType of
    btProcptr:
      Result := 'Proc: '+inttostr(tbtu32(p.Dta^));
    {$IFDEF DELPHI12UP} // "Delphi 2009 Up"
      {$WARN IMPLICIT_STRING_CAST OFF} // -W1057
    {$ENDIF}
    btU8: str(tbtu8(p.dta^), Result);
    btS8: str(tbts8(p.dta^), Result);
    btU16: str(tbtu16(p.dta^), Result);
    btS16: str(tbts16(p.dta^), Result);
    btU32: str(tbtu32(p.dta^), Result);
    btS32: str(tbts32(p.dta^), Result);
    btSingle: str(tbtsingle(p.dta^), Result);
    btDouble: str(tbtdouble(p.dta^), Result);
    btExtended: str(tbtextended(p.dta^), Result);
    {$IFDEF DELPHI12UP} // "Delphi 2009 Up"
      {$WARN IMPLICIT_STRING_CAST ON} // +W1057
    {$ENDIF}
    btString:
      Result := MakeString(TbtString(p.dta^));
    btPChar: begin
      if PTbtChar(p.dta^) = nil then
        Result := 'nil'
      else
        Result := MakeString(PTbtChar(p.dta^));
    end;
    btChar:
      Result := MakeString(TbtChar(p.dta^));
    {$IFNDEF PS_NOWIDESTRING}
    {+}
    {$if declared(btPWideChar)}
    btPWideChar: begin
      if PWideChar(p.dta^) = nil then
        Result := 'nil'
      else
        Result := MakeWString(PWideChar(p.dta^));
    end;
    {$ifend}
    {+.}
    btwidechar:
      Result := MakeWString(TbtWideChar(p.dta^));
    btWideString:
      Result := MakeWString(tbtwidestring(p.dta^));
    btUnicodeString:
      Result := MakeWString(TbtUnicodeString(p.dta^));
    {$ENDIF !PS_NOWIDESTRING}
    {$IFNDEF PS_NOINT64}
    btS64:
      begin
        {$IFDEF DELPHI12UP} // "Delphi 2009 Up"
          {$WARN IMPLICIT_STRING_CAST OFF} // -W1057
        {$ENDIF}
        str(TbtS64(p.dta^), Result);
        {$IFDEF DELPHI12UP} // "Delphi 2009 Up"
          {$WARN IMPLICIT_STRING_CAST ON} // +W1057
        {$ENDIF}
      end;
    {$ENDIF}
    btStaticArray, btArray: begin
      Result := '';
      if p.aType.BaseType = btStaticArray then
        n := TPSTypeRec_StaticArray(p.aType).Size
      else
        n := PSDynArrayGetLength(Pointer(p.dta^), p.aType);
      for i := 0 to n-1 do begin
        if Result <> '' then
          Result := Result + ', ';
        Result := Result + PSVariantToString(PSGetArrayField(p, i), '');
      end;
      Result := '[' + Result + ']';
    end;
    btRecord: begin
      Result := '';
      n := TPSTypeRec_Record(p.aType).FFieldTypes.Count;
      for i := 0 to n-1 do begin
        if Result <> '' then
          Result := Result + ', ';
        Result := Result + PSVariantToString(PSGetRecField(p, i), '');
      end;
      Result := '(' + Result + ')';
    end;
    btPointer:
      Result := 'Nil';
    btClass, btInterface: begin
      Result := ClassVariantInfo(p, ClassProperties)
    end;
    else
      Result := TbtString(RPS_Invalid);
  end;
end; // function PSVariantToString

function TIFErrorToString(x: TPSError; const Param: TbtString): TbtString;
begin
  Result := PSErrorToString(x,param);
end;

function PSErrorToString(x: TPSError; const Param: TbtString): TbtString;
begin {+}{@dbg@:hook.error}{+.} // dbg.cond:
  Result := '';
  case x of
    erNoError:
      Result := TbtString(RPS_NoError);
    erCannotImport:
      Result := TbtString(Format (RPS_CannotImport, [Safestr(Param)]));
    erInvalidType:
      Result := TbtString(RPS_InvalidType);
    erInternalError:
      Result := TbtString(RPS_InternalError);
    erInvalidHeader:
      Result := TbtString(RPS_InvalidHeader);
    erInvalidOpcode:
      Result := TbtString(RPS_InvalidOpcode);
    erInvalidOpcodeParameter:
      Result := TbtString(RPS_InvalidOpcodeParameter);
    erNoMainProc:
      Result := TbtString(RPS_NoMainProc);
    erOutOfGlobalVarsRange:
      Result := TbtString(RPS_OutOfGlobalVarsRange);
    erOutOfProcRange:
      Result := TbtString(RPS_OutOfProcRange);
    erOutOfRange:
      Result := TbtString(RPS_OutOfRange);
    erOutOfStackRange:
      Result := TbtString(RPS_OutOfStackRange);
    erTypeMismatch:
      Result := TbtString(RPS_TypeMismatch);
    erUnexpectedEof:
      Result := TbtString(RPS_UnexpectedEof);
    erVersionError:
      Result := TbtString(RPS_VersionError);
    erDivideByZero:
      Result := TbtString(RPS_DivideByZero);
    erMathError:
      Result := TbtString(RPS_MathError);
    erCouldNotCallProc:
      begin
        Result := TbtString(RPS_CouldNotCallProc);
        if (Param <> '') then
        begin
          if Pos(Result, Param) > 0 then
            Result := Param
          else //if (Param <> Result) then
            Result := Result +' ('+Param+')'
        end;
      end;
    erOutofRecordRange:
      Result := TbtString(RPS_OutofRecordRange);
    erNullPointerException:
      Result := TbtString(RPS_NullPointerException);
    erNullVariantError:
      Result := TbtString(RPS_NullVariantError);
    erOutOfMemory:
      Result := TbtString(RPS_OutOfMemory);
    erException:
      Result := TbtString(Format (RPS_Exception, [Param]));
    erInterfaceNotSupported:
      Result := TbtString(RPS_InterfaceNotSupported);
    erCustomError: Result := Param;
    //else
    //  Result := TbtString(RPS_UnknownError);
  end; // case x
  if Length(Result) = 0 then
    Result := TbtString(RPS_UnknownError);
end;

procedure TPSTypeRec.CalcSize;
begin
  case BaseType of
    btVariant: FRealSize := SizeOf(Variant);
    btChar, bts8, btU8: FrealSize := 1 ;
    {$IFNDEF PS_NOWIDESTRING}btWideChar, {$ENDIF}bts16, btU16: FrealSize := 2;
    {$IFNDEF PS_NOWIDESTRING}btWideString,
    btUnicodeString,
    {$ENDIF}{$IFNDEF PS_NOINTERFACES}btInterface, {$ENDIF}
    btClass, btPChar, {+}{$IFNDEF PS_NOWIDESTRING}{$if declared(btPWideChar)}btPWideChar,{$ifend}{$ENDIF}{+.}btString: FrealSize := PointerSize;
    btSingle, bts32, btU32: FRealSize := 4;
    btProcPtr: FRealSize := 3 * SizeOf(Pointer);
    btCurrency: FrealSize := SizeOf(Currency);
    btPointer: FRealSize := 3 * SizeOf(Pointer); // ptr, type, freewhendone
    btDouble{$IFNDEF PS_NOINT64}, bts64{$ENDIF}: FrealSize := 8;
    btExtended: FrealSize := SizeOf(Extended);
    btReturnAddress: FrealSize := SizeOf(TBTReturnAddress);
  else
    FrealSize := 0;
  end;
end;

{+}
procedure TPSTypeRec.Clear;
begin
  { empty }
end;
{+.}

constructor TPSTypeRec.Create(Owner: TPSExec);
begin
  inherited Create;
  FAttributes := TPSRuntimeAttributes.Create(Owner);
end;

destructor TPSTypeRec.Destroy;
begin
  FreeAndNil(FAttributes);
  inherited;
end;

{+}
{ TPSTypeRec_PChar }

//constructor TPSTypeRec_PChar.Create(Owner: TPSExec);
//begin
//  inherited Create(Owner);
//end;
//
//destructor TPSTypeRec_PChar.Destroy;
//begin
//  Clear;
//  inherited;
//end;
//
//procedure FinalizeVariant(p: Pointer; aType: TPSTypeRec); forward;
//
(*procedure TPSTypeRec_PChar.Clear;
begin
  {
  if Assigned(FTypeCharsRec) then
  begin
    Dec(FTypeCharsRec.FPChasrRefCount);
    if FTypeCharsRec.FPChasrRefCount <= 0 then
    begin
      FinalizeVariant(FTempData, FTypeCharsRec);
      FTypeCharsRec.FPChasrRefCount := 0;
    end;
    //FTempBaseType := 0;
    FTempData := nil;
    FTypeCharsRec := nil;
    //Exit;
  end;
  }
  //
  //case FTempBaseType of
  //  btString:
  //    begin
  //      FTempBaseType := 0;
  //      if Assigned(FTempData) then
  //      begin
  //        TbtString(FTempData^) := '';
  //        FTempData := nil;
  //      end;
  //    end;
  //  {$IFNDEF PS_NOWIDESTRING}
  //  btWideString:
  //    begin
  //      FTempBaseType := 0;
  //      if Assigned(FTempData) then
  //      begin
  //        tbtWideString(FTempData^) := '';
  //        FTempData := nil;
  //      end;
  //    end;
  //  btUnicodeString:
  //    begin
  //      FTempBaseType := 0;
  //      if Assigned(FTempData) then
  //      begin
  //        TbtUnicodeString(FTempData^) := '';
  //        FTempData := nil;
  //      end;
  //    end;
  //  {$ENDIF}
  //end;
  //
end;//*)
{+.}

{ TPSTypeRec_Record }

procedure TPSTypeRec_Record.CalcSize;
begin
  inherited CalcSize;
  FrealSize := TPSTypeRec(FFieldTypes[FFieldTypes.Count-1]).RealSize +
    IPointer(RealFieldOffsets[RealFieldOffsets.Count-1]);
end;

constructor TPSTypeRec_Record.Create(Owner: TPSExec);
begin
  inherited Create(Owner);
  FRealFieldOffsets := TPSList.Create;
  FFieldTypes := TPSList.Create;
end;

destructor TPSTypeRec_Record.Destroy;
begin
  FreeAndNil(FFieldTypes);
  FreeAndNil(FRealFieldOffsets);
  inherited;
end;

const
  RTTISize = SizeOf(TPSVariant);

procedure InitializeVariant(p: Pointer; aType: TPSTypeRec);
var
  t: TPSTypeRec;
  i: Longint;
begin
  {+}if Assigned(p) then{+.}
  case aType.BaseType of
    btChar, bts8, btU8: tbtu8(p^) := 0;
    {$IFNDEF PS_NOWIDESTRING}btWideChar, {$ENDIF}bts16, btU16: tbtu16(p^) := 0;
    btSingle: TbtSingle(P^) := 0;
    bts32, btU32: TbtU32(P^) := 0;
    btPChar, btString, {$IFNDEF PS_NOWIDESTRING}btUnicodeString, btWideString, {+}{$if declared(btPWideChar)}btPWideChar,{$ifend}{+.}{$ENDIF}btClass,
    btInterface, btArray: Pointer(P^) := nil;
    btPointer:
      begin
        Pointer(p^) := nil;
        Pointer(Pointer(IPointer(p)+PointerSize)^) := nil;
        Pointer(Pointer(IPointer(p)+(2*PointerSize))^) := nil;
      end;
    btProcPtr:
      begin
        Longint(p^) := 0;
        Pointer(Pointer(IPointer(p)+PointerSize)^) := nil;
        Pointer(Pointer(IPointer(p)+(2*PointerSize))^) := nil;
      end;
    btCurrency: tbtCurrency(P^) := 0;
    btDouble{$IFNDEF PS_NOINT64}, bts64{$ENDIF}: {$IFNDEF PS_NOINT64}tbtS64(P^) := 0{$ELSE}tbtdouble(p^) := 0 {$ENDIF};
    btExtended: tbtExtended(p^) := 0;
    btVariant: Initialize(Variant(p^));
    btReturnAddress:; // there is no point in initializing a return address
    btRecord:
      begin
        for i := 0 to TPSTypeRec_Record(aType).FFieldTypes.Count-1 do
        begin
          t := TPSTypeRec_Record(aType).FieldTypes[i];
          InitializeVariant(P, t);
          p := Pointer(IPointer(p) + t.FrealSize);
        end;
      end;
    btStaticArray:
      begin
        t := TPSTypeRec_Array(aType).ArrayType;
        for i := 0 to TPSTypeRec_StaticArray(aType).Size-1 do
        begin
          InitializeVariant(p, t);
          p := Pointer(IPointer(p) + t.RealSize);
        end;
      end;
    btSet:
      begin
        FillChar(p^, TPSTypeRec_Set(aType).RealSize, 0);
      end;
  end;
end;

procedure DestroyHeapVariant2(v: Pointer; aType: TPSTypeRec); forward;

const
  NeedFinalization = [btStaticArray, btRecord, btArray, btPointer, btVariant
    {$IFNDEF PS_NOINTERFACES},btInterface{$ENDIF}
    ,btString
    {$IFNDEF PS_NOWIDESTRING},btUnicodestring,btWideString{$ENDIF}
  ];
type
  TDynArrayRecHeader = packed record
    {$ifdef FPC}
    refCnt  : PtrInt;
    high    : tdynarrayindex;
    {$else !FPC}
    {$ifdef CPUX64}
    _Padding: LongInt; // Delphi XE2+ expects 16 byte align
    {$endif}
    // dynamic array reference count (basic garbage memory mechanism)
    refCnt  : Longint;
    // length in element count
    // - size in bytes = length*ElemSize
    length  : IPointer;
    {$endif !FPC}
  end;
  TDynArrayRec = packed record
    header : TDynArrayRecHeader;
    datas  : Pointer;
  end;
  PDynArrayRec = ^TDynArrayRec;

procedure FinalizeVariant(p: Pointer; aType: TPSTypeRec);
var
  OK: Boolean;
  t: TPSTypeRec;
  elsize: Cardinal;
  i, l: {+}NativeInt{+.};
  darr: PDynArrayRec;
begin
  {+}
  if p = nil then
    Exit;
  {+.}
  case aType.BaseType of
    btString:
      TbtString(p^) := '';

    {$IFNDEF PS_NOWIDESTRING}
    btWideString:
      tbtWideString(p^) := '';
    btUnicodeString:
      TbtUnicodeString(p^) := '';
    {$ENDIF !PS_NOWIDESTRING}

    {$IFNDEF PS_NOINTERFACES}
    btInterface: begin
        {$IFNDEF DELPHI3UP}
        if IUnknown(p^) <> nil then
          IUnknown(p^).Release;
        {$ENDIF}
        IUnknown(p^) := nil;
      end;
    {$ENDIF !PS_NOINTERFACES}

    btVariant: begin
      try
        Finalize(Variant(p^));
      except
      end;
    end;

    btPointer: begin
      OK := Pointer(Pointer(IPointer(p)+PointerSize2)^) <> nil;
      if OK then begin
        DestroyHeapVariant2(Pointer(p^), Pointer(Pointer(IPointer(p)+PointerSize)^));
        Pointer(p^) := nil;
      end;
    end;

    btArray: begin
      if IPointer(P^) = 0 then
        Exit;
      {+}
      //darr := PDynArrayRec(IPointer(p^) - SizeOf(TDynArrayRecHeader));
      darr := PDynArrayRec(PointerShift(Pointer(p^),-SizeOf(TDynArrayRecHeader)));
      {+.}
      if darr^.header.refCnt < 0 then
        Exit;// refcount < 0 means don't free
      Dec(darr^.header.refCnt);
      if darr^.header.refCnt <> 0 then
        Exit;
      t := TPSTypeRec_Array(aType).ArrayType;
      elsize := t.RealSize;
      {$IFDEF FPC}
      l := darr^.header.high+1;
      {$ELSE}
      l := darr^.header.length;
      {$ENDIF FPC}
      darr := @darr^.datas;
      case t.BaseType of
        btString,
        {$IFNDEF PS_NOWIDESTRING}
        btUnicodeString, btWideString,
        {$ENDIF}
        {$IFNDEF PS_NOINTERFACES}
        btInterface,
        {$ENDIF}
        btArray, btStaticArray, btRecord, btPointer, btVariant: begin
          for i := 0 to l-1 do begin
            FinalizeVariant(darr, t);
            {+}
            //darr := Pointer(IPointer(darr) + elsize);
            darr := PointerShift(darr, elsize);
            {+.}
          end;
        end;
      end;
      {+}
      //FreeMem(Pointer(IPointer(p^) - SizeOf(TDynArrayRecHeader)), IPointer(Cardinal(l) * elsize) + SizeOf(TDynArrayRecHeader));
      darr := PointerShift(Pointer(p^), -SizeOf(TDynArrayRecHeader));
      l := l * NativeInt(elsize) + NativeInt(SizeOf(TDynArrayRecHeader));
      FreeMem(darr, l);
      {+.}
      Pointer(P^) := nil;
    end; // btArray

    btRecord: begin
      for i := 0 to TPSTypeRec_Record(aType).FFieldTypes.Count-1 do
      begin
        t := TPSTypeRec_Record(aType).FieldTypes[i];
        case t.BaseType of
          btString,
          {$IFNDEF PS_NOWIDESTRING}
          btUnicodeString, btWideString,
          {$ENDIF}
          {$IFNDEF PS_NOINTERFACES}
          btInterface,
          {$ENDIF}
          btArray, btStaticArray, btRecord:
            FinalizeVariant(p, t);
        end;
        {+}
        //p := Pointer(IPointer(p) + t.FrealSize);
        p := PointerShift(p, NativeInt(t.FrealSize));
        {+.}
      end;
    end;

    btStaticArray: begin
      t := TPSTypeRec_Array(aType).ArrayType;
      case t.BaseType of
        btString,
        {$IFNDEF PS_NOWIDESTRING}
        btUnicodeString, btWideString,
        {$ENDIF}
        {$IFNDEF PS_NOINTERFACES}
        btInterface,
        {$ENDIF}
        btArray, btStaticArray,btRecord:
          {empty};
        else
          Exit;
      end;
      for i := 0 to TPSTypeRec_StaticArray(aType).Size-1 do begin
        FinalizeVariant(p, t);
        {+}
        //p := Pointer(IPointer(p) + t.RealSize);
        p := PointerShift(p, NativeInt(t.RealSize));
        {+.}
      end;
    end; // btStaticArray

  end; // case
end; // procedure FinalizeVariant

function CreateHeapVariant2(aType: TPSTypeRec): Pointer;
begin
  GetMem(Result, aType.RealSize);
  InitializeVariant(Result, aType);
end;

procedure DestroyHeapVariant2(v: Pointer; aType: TPSTypeRec);
begin
  if v = nil then
    Exit;
  if atype.BaseType in NeedFinalization then
    FinalizeVariant(v, aType);
  FreeMem(v, aType.RealSize);
end;

function CreateHeapVariant(aType: TPSTypeRec): PPSVariant;
var aSize: Longint;
begin
  aSize := aType.RealSize + RTTISize;
  GetMem(Result, aSize);
  Result.FType := aType;
  InitializeVariant(Pointer(IPointer(Result)+PointerSize), aType);
end;

procedure DestroyHeapVariant(v: PPSVariant);
begin
  if v = nil then
    Exit;
  if v.FType.BaseType in NeedFinalization then begin
    FinalizeVariant(Pointer(IPointer(v)+PointerSize), v.FType);
  end;
  FreeMem(v, v.FType.RealSize + RTTISize);
end;

procedure FreePSVariantList(l: TPSList);
var i: Longint;
begin
  for i:= l.Count-1 downto 0 do
    DestroyHeapVariant(l[i]);
  l.free;
end;

procedure FreePIFVariantList(l: TPSList);
begin
  FreePsVariantList(l);
end;

{ TPSExec }

procedure TPSExec.ClearFunctionList;
var
  x: PProcRec;
  l: Longint;
begin
  for l := FAttributeTypes.Count-1 downto 0 do
  begin
    TPSAttributeType(FAttributeTypes.Data^[l]).Free;
  end;
  FAttributeTypes.Clear;

  for l := 0 to FRegProcs.Count - 1 do
  begin
    x := FRegProcs.Data^[l];
    if @x^.FreeProc <> nil then x^.FreeProc(Self, x);
    Dispose(x);
  end;
  FRegProcs.Clear;
  RegisterStandardProcs;
end;

class function TPSExec.About: TbtString;
begin
  Result := 'RemObjects Pascal Script. Copyright (c) 2004-2010 by RemObjects Software';
end;

procedure TPSExec.Cleanup;
var
  I: Longint;
  p: Pointer;
begin
  if FStatus <> isLoaded then
    Exit;
  FStack.Clear;
  FTempVars.Clear;
  for I := Longint(FGlobalVars.Count) - 1 downto 0 do
  begin
    p := FGlobalVars.Items[i];
    if PIFTypeRec(P^).BaseType in NeedFinalization then begin
      FinalizeVariant(Pointer(IPointer(p)+PointerSize), Pointer(P^));
    end;
    InitializeVariant(Pointer(IPointer(p)+PointerSize), Pointer(P^));
  end;
end;

//function TPSExec.LoadDebugData(const Data: TbtString): Longint;
//begin
//  Result := 0;
//end;

function TPSExec.LoadDebugInfo(): Boolean;
// Allows downloading of debugging information dynamically as needed!
begin
  Result := False;
  if Assigned(FOnLoadDebugInfo) then
    FOnLoadDebugInfo(Self, Result);
end;

function TPSExec.TranslatePosition(Proc, Position: Cardinal): Cardinal;
var D1, D2: Cardinal; s: TbtString;
{$IFDEF FPC}{$push} // FPC: https://wiki.freepascal.org/Turn_warnings_and_hints_on_or_off
  {$warn 5060 off}  // FPC: Hint: Function result variable does not seem to be initialized
  {$warn 5057 off}  // FPC: Hint: Local variable "$1" does not seem to be initialized
{$ENDIF}
begin
  if not TranslatePositionEx(Proc, Position, {%H-}Result, {%H-}D1, {%H-}D2, {%H-}s) then
    Result := 0;
end; {$IFDEF FPC}{$pop}{$ENDIF}

function TPSExec.TranslatePositionEx(Proc, Position: Cardinal; var Pos, Row, Col: Cardinal; var Fn: TbtString): Boolean;
begin
  Row := 0;
  Col := 0;
  Fn := '';
  Result := False;
end;

function TPSExec.GetCurrentPositionDebugInfo(const sPrefix: TbtString): TbtString;
var
  OK: Boolean;
  d1: Cardinal;
  fn: TbtString;
  Row, Col: Cardinal;
begin
  Result := '';
  if FCurrentPosition > 0 then
  try
    OK := LoadDebugInfo();
    if OK then
    begin
      Row := FCurrentRow;
      Col := FCurrentCol;
      OK := Row > 0;
      if not OK then begin
        OK := TranslatePositionEx(ExProc, FCurrentPosition, {%H-}d1, Row, Col, {%H-}fn);
      end;
      if OK then
      begin
        Result := sPrefix + 'RPS';
        if Length(FCurrentFile) > 0 then
          Result := Result + ': "' + TbtString(ExtractFileName(string(FCurrentFile))) + '"';
        if Row > 0 then
          Result := Result + ': ('+TbtString(SysUtils.IntToStr(Row))+','+TbtString(SysUtils.IntToStr(Col))+')';
      end;
    end;
  except
  end;
end;

procedure TPSExec.Clear;
var
  I: Longint;
  Temp: PPSResource;
  Proc: TPSResourceFreeProc;
  pp: TPSExceptionHandler;
  ptr: Pointer;
begin
  {+}
  if Assigned(FExceptionStack) then
    for i := Longint(FExceptionStack.Count)-1 downto 0 do
    begin
      pp := FExceptionStack.Data^[i];
      FExceptionStack.Data^[i] := nil;
      pp.Free;
    end;
  if Assigned(FResources) then
    for i := Longint(FResources.Count)-1 downto 0 do
    begin
      Temp := FResources.Data^[i];
      if Temp = nil then
        Continue;
      Proc := Temp^.Proc;
      Proc(Self, Temp^.P);
      FResources.Data^[i] := nil;
      Dispose(Temp);
    end;
  if Assigned(FExportedVars) then
    for i := Longint(FExportedVars.Count)-1 downto 0 do
    begin
      ptr := FExportedVars.Data^[I];
      if ptr = nil then
        Continue;
      FExportedVars.Data^[I] := nil;
      Dispose(PPSExportedVar(ptr));
    end;
  if Assigned(FProcs) then
  begin
    for I := Longint(FProcs.Count) - 1downto 0 do
    begin
      ptr := FProcs.Data^[i];
      if ptr = nil then
        Continue;
      FProcs.Data^[i] := nil;
      TPSProcRec(ptr).Destroy;
    end;
    FProcs.Clear;
  end;
  if Assigned(FGlobalVars) then
    FGlobalVars.Clear;
  if Assigned(FStack) then
    FStack.Clear;
  if Assigned(FTypes) then
  begin
    for I := Longint(FTypes.Count) - 1downto 0 do
    begin
      ptr := FTypes.Data^[i];
      FTypes.Data^[i] := nil;
      TPSTypeRec(ptr).Free;
    end;
    FTypes.Clear;
  end;
  FStatus := isNotLoaded;
  if Assigned(FResources) then
    FResources.Clear;
  if Assigned(FExportedVars) then
    FExportedVars.Clear;
  if Assigned(FExceptionStack) then
    FExceptionStack.Clear;
  FCurrStackBase := InvalidVal;
  {+.}
end;

constructor TPSExec.Create;
begin
  inherited Create;
  FAttributeTypes := TPSList.Create;
  FExceptionStack := TPSList.Create;
  FCallCleanup := False;
  FResources := TPSList.Create;
  FTypes := TPSList.Create;
  FProcs := TPSList.Create;
  FGlobalVars := TPSStack.Create;
  FTempVars := TPSStack.Create;
  FMainProc := 0;
  FStatus := isNotLoaded;
  FRegProcs := TPSList.Create;
  FExportedVars := TPSList.create;
  FSpecialProcList := TPSList.Create;
  RegisterStandardProcs;
  FReturnAddressType := TPSTypeRec.Create(self);
  FReturnAddressType.BaseType := btReturnAddress;
  FReturnAddressType.CalcSize;
  FVariantType := TPSTypeRec.Create(self);
  FVariantType.BaseType := btVariant;
  FVariantType.CalcSize;
  FVariantArrayType := TPSTypeRec_Array.Create(self);
  FVariantArrayType.BaseType := btArray;
  FVariantArrayType.CalcSize;
  TPSTypeRec_Array(FVariantArrayType).ArrayType := FVariantType;
  FStack := TPSStack.Create;
end;

destructor TPSExec.Destroy;
var
  I: Longint;
  x: PProcRec;
  P: PSpecialProc;
begin
  Clear;
  {+}
  FreeAndNil(FReturnAddressType);
  FreeAndNil(FVariantType);
  FreeAndNil(FVariantArrayType);
  FreeAndNil(ExObject);

  if Assigned(FSpecialProcList) then begin
    for I := FSpecialProcList.Count-1 downto 0 do begin
      P := FSpecialProcList.Data^[I];
      if Assigned(p) then begin
        FSpecialProcList.Data^[I] := nil;
        Dispose(p);
      end;
    end;
  end;

  FreeAndNil(FResources);
  FreeAndNil(FExportedVars);
  FreeAndNil(FTempVars);
  FreeAndNil(FStack);
  FreeAndNil(FGlobalVars);
  FreeAndNil(FProcs);
  FreeAndNil(FTypes);
  FreeAndNil(FSpecialProcList);

  if Assigned(FRegProcs) then begin
    for i := FRegProcs.Count - 1 downto 0 do begin
      x := FRegProcs.Data^[i];
      if Assigned(x) then begin
        FRegProcs.Data^[i] := nil;
        if @x^.FreeProc <> nil then
          x^.FreeProc(Self, x);
        Dispose(x);
      end;
    end;
    FreeAndNil(FRegProcs);
  end;

  FreeAndNil(FExceptionStack);

  if Assigned(FAttributeTypes) then begin
    FAttributeTypes.ClearAsObjects({Destroying:}True);
    FreeAndNil(FAttributeTypes);
  end;
  {+.}
  inherited;
end;

procedure TPSExec.ExceptionProc(proc, Position: Cardinal; Ex: TPSError; const s: TbtString; NewObject: TObject);
var
  d, l: Longint;
  pp: TPSExceptionHandler;
begin
  ExProc := proc;
  ExPos := Position;
  ExEx := Ex;
  ExParam := s;
  if ExObject <> nil then{+}
  begin
    if (Ex<>ErNoError) and (ExParam = '') and (ExObject is ExceptionBase) then
      ExParam := TbtString(ExceptionBase(ExObject).Message);
    ExObject.Free;
  end;{+.}
  ExObject := NewObject; // @dbg: EScriptUserException(ExObject)
  if Ex = eNoError then
    Exit;
  for d := FExceptionStack.Count-1 downto 0 do
  begin
    pp := FExceptionStack[d];
    if Cardinal(FStack.Count) > pp.StackSize then
    begin
      for l := Longint(FStack.Count)-1 downto Longint(pp.StackSize) do
        FStack.Pop;
    end;
    if pp.CurrProc = nil then // no point in continuing
    begin
      pp.Free;
      FExceptionStack.DeleteLast;

      FCurrStackBase := InvalidVal;
      FStatus := isPaused;
      Exit;
    end;
    FCurrProc := pp.CurrProc;
    FData := FCurrProc.Data;
    FDataLength := FCurrProc.Length;

    FCurrStackBase := pp.BasePtr;
    if pp.FinallyOffset <> InvalidVal then
    begin
      FCurrentPosition := pp.FinallyOffset;
      pp.FinallyOffset := InvalidVal;
      Exit;
    end else if (pp.ExceptOffset <> InvalidVal) and (pp.ExceptOffset <> Cardinal(InvalidVal-1)) then
    begin
      FCurrentPosition := pp.ExceptOffset;
      pp.ExceptOffset := Cardinal(InvalidVal-1); // TODO: bad value from access in except handling block and in event OnException for "Sender.ExPos)"
      pp.ExceptionObject := ExObject;
      pp.ExceptionData := ExEx;
      pp.ExceptionParam := ExParam;
      ExObject := nil;
      ExEx := ENoError;
      Exit;
    end else if pp.Finally2Offset <> InvalidVal then
    begin
      FCurrentPosition := pp.Finally2Offset;
      pp.Finally2Offset := InvalidVal;
      Exit;
    end;
    pp.Free;
    FExceptionStack.DeleteLast;
  end;
  if FStatus <> isNotLoaded then
    FStatus := isPaused;
end;

function LookupProc(List: TPSList; const Name: ShortString): PProcRec;
var
  h, l: Longint;
  p: PProcRec;
begin
  h := MakeHash(Name);
  for l := List.Count-1 downto 0 do begin
    p := List.Data^[l];
    if (p^.Hash = h) and (p^.Name = Name) then begin
      Result := List[l];
      Exit;
    end;
  end;
  Result := nil;
end;

function TPSExec.ImportProc(const Name: ShortString; proc: TPSExternalProcRec): Boolean;
var
  u: PProcRec;
  fname: TbtString;
  I, fnh: Longint;
  P: PSpecialProc;
begin
  if name = '' then begin
    fname := proc.Decl;
    fname := copy(fname, 1, pos(tbtChar(':'), fname)-1);
    fnh := MakeHash(fname);
    for I := FSpecialProcList.Count-1 downto 0 do begin
      p := FSpecialProcList[I];
      if (p^.name = '') or ((p^.namehash = fnh) and (p^.name = fname)) then begin
        if p^.P(Self, Proc, p^.tag) then begin
          Result := True;
          Exit;
        end;
      end;
    end;
    Result := False;
    Exit;
  end;
  u := LookupProc(FRegProcs, Name);
  if u = nil then begin
    Result := False;
    Exit;
  end;
  proc.ProcPtr := u^.ProcPtr;
  proc.Ext1 := u^.Ext1;
  proc.Ext2 := u^.Ext2;
  Result := True;
end;

function TPSExec.RegisterFunctionName(const Name: TbtString; ProcPtr: TPSProcPtr; Ext1, Ext2: Pointer): PProcRec;
var
  p: PProcRec;
  S: ShortString;
begin
  S := ShortString(FastUpperCase(Name));
  New(p);
  p^.Name := S;
  p^.Hash := MakeHash(S);
  p^.ProcPtr := ProcPtr;
  p^.FreeProc := nil;
  p^.Ext1 := Ext1;
  p^.Ext2 := Ext2;
  FRegProcs.Add(p);
  Result := P;
end;

function TPSExec.LoadData(const S: TbtString): Boolean;
var
  HDR: TPSHeader;
  Pos: Cardinal;

  function Read(var Data; Len: Cardinal): Boolean;
  begin
    Result := Longint(Pos + Len) <= Length(s);
    if Result then begin
      Move(S[Pos + 1], Data, Len);
      {$if btCharIsWide}
      Len := (Len+1) div 2;
      {$ifend}
      Pos := Pos + Len;
    end;
  end;

  function ReadAttributes(Dest: TPSRuntimeAttributes): Boolean;
  var
    Count: Cardinal;
    i: Integer;

    function ReadAttrib: Boolean;
    var
      NameLen: Longint;
      Name: TbtString;
      TypeNo: Cardinal;
      i, h, FieldCount: Longint;
      att: TPSRuntimeAttribute;
      varp: PIFVariant;
    begin
      Result := Read({%H-}NameLen, 4) and (NameLen <= Length(s) - Longint(Pos));
      if not Result then begin
        CMD_Err2(erOutOfRange, TbtString(RPS_OutOfRange));
        Exit;
      end;
      SetLength({%H-}Name, NameLen);
      Result := Read(Name[1], NameLen*btCharSize);
      if not Result then begin
        CMD_Err2(erOutOfRange, TbtString(RPS_OutOfRange));
        Exit;
      end;
      Result := Read({%H-}FieldCount, 4);
      if not Result then begin
        CMD_Err2(erOutOfRange, TbtString(RPS_OutOfRange));
        Exit;
      end;
      att := Dest.Add;
      att.AttribType := Name;
      att.AttribTypeHash := MakeHash(att.AttribType);
      for i := 0 to FieldCount-1 do begin
        Result := Read({%H-}TypeNo, 4) and (TypeNo < Cardinal(FTypes.Count));
        if not Result then begin
          CMD_Err2(erOutOfRange, TbtString(RPS_OutOfRange));
          Exit;
        end;
        //
        VarP := att.AddValue(FTypes[TypeNo]);
        case VarP^.FType.BaseType of
          btSet: begin
            Result := Read(PPSVariantSet(varp).Data, TPSTypeRec_Set(varp.FType).aByteSize);
            if not Result then begin
              CMD_Err2(erOutOfRange, TbtString(RPS_OutOfRange));
              DestroyHeapVariant(VarP);
              Exit;
            end;
          end;
          bts8 {$if btCharIsAnsi},btChar{$ifend}, btU8: begin
            Result := Read(PPSVariantU8(VarP)^.Data, 1);
            if not Result then begin
              CMD_Err2(erOutOfRange, TbtString(RPS_OutOfRange));
              DestroyHeapVariant(VarP);
              Exit;
            end;
          end;
          {$if btCharIsWide}
          btChar,
          {$ifend}
          bts16,
          {$IFNDEF PS_NOWIDESTRING}
          btWideChar,
          {$ENDIF}
          btU16: begin
            Result := Read(PPSVariantU16(Varp)^.Data, SizeOf(TbtU16));
            if not Result then begin
              CMD_Err2(erOutOfRange, TbtString(RPS_OutOfRange));
              DestroyHeapVariant(VarP);
              Exit;
            end;
          end;
          btS32, btU32: begin
            Result := FCurrentPosition + 3 < FDataLength;
            if not Result then begin
              CMD_Err2(erOutOfRange, TbtString(RPS_OutOfRange));
              DestroyHeapVariant(VarP);
              Exit;
            end;
            PPSVariantU32(VarP)^.Data := {$ifdef FPC_REQUIRES_PROPER_ALIGNMENT}unaligned{$endif}(
              {+}tbtU32{+.}((@FData[FCurrentPosition])^)
            );
            Inc(FCurrentPosition, 4);
          end;
          btProcPtr: begin
            Result := FCurrentPosition + 3 < FDataLength;
            if not Result then begin
              CMD_Err2(erOutOfRange, TbtString(RPS_OutOfRange));
              DestroyHeapVariant(VarP);
              Exit;
            end;
            PPSVariantU32(VarP)^.Data := {$ifdef FPC_REQUIRES_PROPER_ALIGNMENT}unaligned{$endif}(
              {+}tbtU32{+.}((@FData[FCurrentPosition])^)
            );
            if PPSVariantU32(VarP)^.Data = 0 then begin
              PPSVariantProcPtr(VarP)^.Ptr := nil;
              PPSVariantProcPtr(VarP)^.Self := nil;
            end;
            Inc(FCurrentPosition, 4);
          end;
          {$IFNDEF PS_NOINT64}
          bts64: begin
            Result := Read(PPSVariantS64(VarP)^.Data, SizeOf(TbtS64));
            if not Result then begin
              CMD_Err2(erOutOfRange, TbtString(RPS_OutOfRange));
              DestroyHeapVariant(VarP);
              Exit;
            end;
          end;
          {$ENDIF PS_NOINT64}
          btSingle: begin
            Result := Read(PPSVariantSingle(VarP)^.Data, SizeOf(TbtSingle));
            if not Result then begin
              CMD_Err2(erOutOfRange, TbtString(RPS_OutOfRange));
              DestroyHeapVariant(VarP);
              Exit;
            end;
          end;
          btDouble: begin
            Result := Read(PPSVariantDouble(VarP)^.Data, SizeOf(TbtDouble));
            if not Result then begin
              CMD_Err2(erOutOfRange, TbtString(RPS_OutOfRange));
              DestroyHeapVariant(VarP);
              Exit;
            end;
          end;
          btExtended: begin
            Result := Read(PPSVariantExtended(VarP)^.Data, SizeOf(TbtExtended));
            if not Result then begin
              CMD_Err2(erOutOfRange, TbtString(RPS_OutOfRange));
              DestroyHeapVariant(VarP);
              Exit;
            end;
          end;
          btCurrency: begin
            Result := Read(PPSVariantExtended(VarP)^.Data, SizeOf(tbtCurrency));
            if not Result then begin
              CMD_Err2(erOutOfRange, TbtString(RPS_OutOfRange));
              DestroyHeapVariant(VarP);
              Exit;
            end;
          end;
          btPChar, btString: begin
            Result := Read(NameLen, 4);
            if not Result then begin
              CMD_Err2(erOutOfRange, TbtString(RPS_OutOfRange));
              DestroyHeapVariant(VarP);
              Exit;
            end;
            Inc(FCurrentPosition, 4);
            SetLength(PPSVariantAString(VarP)^.Data, NameLen);
            Result := Read(PPSVariantAString(VarP)^.Data[1], NameLen*btCharSize);
            if not Result then begin
              CMD_Err2(erOutOfRange, TbtString(RPS_OutOfRange));
              DestroyHeapVariant(VarP);
              Exit;
            end;
          end;
          {$IFNDEF PS_NOWIDESTRING}
          {+}
          {$if declared(btPWideChar)}
          btPWideChar,
          {$ifend}
          {+.}
          btWidestring: begin
            Result := Read(NameLen, 4);
            if not Result then begin
              CMD_Err2(erOutOfRange, TbtString(RPS_OutOfRange));
              DestroyHeapVariant(VarP);
              Exit;
            end;
            Inc(FCurrentPosition, 4);
            SetLength(PPSVariantWString(VarP).Data, NameLen);
            Result := Read(PPSVariantWString(VarP).Data[1], NameLen*2);
            if not Result then begin
              CMD_Err2(erOutOfRange, TbtString(RPS_OutOfRange));
              DestroyHeapVariant(VarP);
              Exit;
            end;
          end;
          {$ENDIF !PS_NOWIDESTRING}
          btUnicodeString: begin
            Result := Read(NameLen, 4);
            if not Result then begin
              CMD_Err2(erOutOfRange, TbtString(RPS_OutOfRange));
              DestroyHeapVariant(VarP);
              Exit;
            end;
            Inc(FCurrentPosition, 4);
            SetLength(PPSVariantUString(VarP).Data, NameLen);
            Result := Read(PPSVariantUString(VarP).Data[1], NameLen*2);
            if not Result then begin
              CMD_Err2(erOutOfRange, TbtString(RPS_OutOfRange));
              DestroyHeapVariant(VarP);
              Exit;
            end;
          end;
          else begin
            CMD_Err2(erInvalidType, TbtString(RPS_InvalidType));
            DestroyHeapVariant(VarP);
            Result := False;
            Exit;
          end;
        end; // case VarP^.FType.BaseType
      end; // for i
      h := MakeHash(att.AttribType);
      for i := FAttributeTypes.Count-1 downto 0 do begin
        Result := (TPSAttributeType(FAttributeTypes.Data^[i]).TypeNameHash = h) and
          (TPSAttributeType(FAttributeTypes.Data^[i]).TypeName = att.AttribType);
        if Result then begin
          Result := TPSAttributeType(FAttributeTypes.Data^[i]).UseProc(Self, att.AttribType, Att);
          if not Result then
            Exit;
        end;
      end;
      //
      Result := True;
    end; // function ReadAttrib

  begin // function ReadAttributes
    Result := Read({%H-}Count, 4);
    if not Result then
    begin
      CMD_Err2(erOutOfRange, TbtString(RPS_OutOfRange));
      Exit;
    end;
    for i := 0 to Count-1 do
    begin
      Result := ReadAttrib();
      if not Result then
        Exit;
    end;
    //
    Result := True;
  end; // function ReadAttributes

  {$WARNINGS OFF}
  function LoadTypes: Boolean;
  var
    currf: TPSType;
    Curr: PIFTypeRec;
    fe: Boolean;
    l2, l: Longint;
    d: Cardinal;

    function Resolve(Dta: TPSTypeRec_Record): Boolean;
    var
      offs, l: Longint;
    begin
      offs := 0;
      for l := 0 to Dta.FieldTypes.Count-1 do begin
        Dta.RealFieldOffsets.Add(Pointer(offs));
        offs := offs + TPSTypeRec(Dta.FieldTypes[l]).RealSize;
      end;
      Result := True;
    end;

  begin
    {+} // remove warning
    {$IFNDEF FPC}{$IFDEF CPUX86}{$IFNDEF DELPHI_TOKYO_UP}
    Curr := nil;
    {$ENDIF}{$ENDIF}{$ENDIF}
    {+.}
    for l := 0 to HDR.TypeCount - 1 do begin
      Result := Read({%H-}currf, SizeOf(currf));
      if not Result then begin
        CMD_Err2(erUnexpectedEof, TbtString(RPS_UnexpectedEof));
        Exit;
      end;
      if (currf.BaseType and 128) <> 0 then begin
        fe := True;
        currf.BaseType := currf.BaseType - 128;
      end else
        fe := False;
      case currf.BaseType of
        {$IFNDEF PS_NOINT64}
        bts64,
        {$ENDIF}
        btU8, btS8, btU16, btS16, btU32, btS32, btSingle, btDouble, btCurrency,
        btExtended, btPointer,
        {+}
        //btString, btChar, //btPChar,
        //{$IFNDEF PS_NOWIDESTRING}
        //btUnicodeString, btWideString, btWideChar, //btPWideChar,
        //{$ENDIF}
        btVariant: begin
        {+.}
          curr := TPSTypeRec.Create(self);
          Curr.BaseType := currf.BaseType;
          FTypes.Add(Curr);
        end;
          {+}
        {+}
        btString, btChar
        //,btPChar,
        {$IFNDEF PS_NOWIDESTRING}
        ,btUnicodeString, btWideString, btWideChar
        //,btPWideChar
        {+.}
        {$ENDIF}: begin
          curr := TPSTypeRec_Chars.Create(self);
          Curr.BaseType := currf.BaseType;
          FTypes.Add(Curr);
        end;
        {+.}
        {+}
        btPChar(*{$IFNDEF PS_NOWIDESTRING}, btPWideChar{$ENDIF}*): begin
          //curr := TPSTypeRec_PChar.Create(self);
          curr := TPSTypeRec_PAnsiChar.Create(self);
          Curr.BaseType := currf.BaseType;
          FTypes.Add(Curr);
        end;
        {$IFNDEF PS_NOWIDESTRING}
           {$if declared(btPWideChar)}
        btPWideChar: begin
          curr := TPSTypeRec_PWideChar.Create(self);
          Curr.BaseType := currf.BaseType;
          FTypes.Add(Curr);
        end;
           {$ifend}
        {$ENDIF}
        {+.}
        btClass: begin
          Curr := TPSTypeRec_Class.Create(Self);
          Result := Read({%H-}d, 4) and (d <= 255);
          if not Result then begin
            curr.Free;
            CMD_Err2(erUnexpectedEof, TbtString(RPS_UnexpectedEof));
            Exit;
          end;
          SetLength(TPSTypeRec_Class(Curr).FCN, d);
          Result := Read(TPSTypeRec_Class(Curr).FCN[1], d*btCharSize);
          if not Result then begin
            curr.Free;
            CMD_Err2(erUnexpectedEof, TbtString(RPS_UnexpectedEof));
            Exit;
          end;
          Curr.BaseType := currf.BaseType;
          FTypes.Add(Curr);
        end;
        btProcPtr: begin
          Curr := TPSTypeRec_ProcPtr.Create(Self);
          Result := Read({%H-}d, 4) and (d <= 255);
          if not Result then begin
            curr.Free;
            CMD_Err2(erUnexpectedEof, TbtString(RPS_UnexpectedEof));
            Exit;
          end;
          Setlength(TPSTypeRec_ProcPtr(Curr).FParamInfo, d);
          Result := Read(TPSTypeRec_ProcPtr(Curr).FParamInfo[1], d*btCharSize);
          if not Result then begin
            curr.Free;
            CMD_Err2(erUnexpectedEof, TbtString(RPS_UnexpectedEof));
            Exit;
          end;
          Curr.BaseType := currf.BaseType;
          FTypes.Add(Curr);
        end;
        {$IFNDEF PS_NOINTERFACES}
        btInterface: begin
          Curr := TPSTypeRec_Interface.Create(self);
          Result := Read(TPSTypeRec_Interface(Curr).FGUID, SizeOf(TGuid));
          if not Result then begin
            curr.Free;
            CMD_Err2(erUnexpectedEof, TbtString(RPS_UnexpectedEof));
            Exit;
          end;
          Curr.BaseType := currf.BaseType;
          FTypes.Add(Curr);
        end;
        {$ENDIF !PS_NOINTERFACES}
        btSet: begin
          Curr := TPSTypeRec_Set.Create(Self);
          Result := Read({%H-}d, 4);
          if not Result then begin
            curr.Free;
            CMD_Err2(erUnexpectedEof, TbtString(RPS_UnexpectedEof));
            Exit;
          end;
          Result := d <= 256;
          if not Result then begin
            curr.Free;
            CMD_Err2(erTypeMismatch, TbtString(RPS_TypeMismatch));
            Exit;
          end;
          TPSTypeRec_Set(curr).aBitSize := d;
          TPSTypeRec_Set(curr).aByteSize := TPSTypeRec_Set(curr).aBitSize shr 3;
          if (TPSTypeRec_Set(curr).aBitSize and 7) <> 0 then
            Inc(TPSTypeRec_Set(curr).fByteSize);
          Curr.BaseType := currf.BaseType;
          FTypes.Add(Curr);
        end;
        btStaticArray: begin
          curr := TPSTypeRec_StaticArray.Create(self);
          Result := Read({%H-}d, 4);
          if not Result then begin
            curr.Free;
            CMD_Err2(erUnexpectedEof, TbtString(RPS_UnexpectedEof));
            Exit;
          end;
          Result := d < FTypes.Count;
          if not Result then begin
            curr.Free;
            CMD_Err2(erTypeMismatch, TbtString(RPS_TypeMismatch));
            Exit;
          end;
          TPSTypeRec_StaticArray(curr).ArrayType := FTypes[d];
          Result := Read(d, 4);
          if not Result then begin
            curr.Free;
            CMD_Err2(erUnexpectedEof, TbtString(RPS_UnexpectedEof));
            Exit;
          end;
          Result := d <= (MaxInt div 4);
          if not Result then begin
            curr.Free;
            CMD_Err2(erUnexpectedEof, TbtString(RPS_UnexpectedEof));
            Exit;
          end;
          TPSTypeRec_StaticArray(curr).Size := d;
          Result := Read(d,4); //<-additional StartOffset
          if not Result then begin
            curr.Free;
            CMD_Err2(erUnexpectedEof, TbtString(RPS_UnexpectedEof));
            Exit;
          end;
          TPSTypeRec_StaticArray(curr).StartOffset := d;
          Curr.BaseType := currf.BaseType;
          FTypes.Add(Curr);
        end; // btStaticArray
        btArray: begin
          Curr := TPSTypeRec_Array.Create(self);
          Result := Read({%H-}d, 4); // Read type
          if not Result then begin
            curr.Free;
            CMD_Err2(erUnexpectedEof, TbtString(RPS_UnexpectedEof));
            Exit;
          end;
          Result := d < FTypes.Count;
          if not Result then begin
            curr.Free;
            CMD_Err2(erTypeMismatch, TbtString(RPS_TypeMismatch));
            Exit;
          end;
          Curr.BaseType := currf.BaseType;
          TPSTypeRec_Array(curr).ArrayType := FTypes[d];
          FTypes.Add(Curr);
        end;
        btRecord: begin
          curr := TPSTypeRec_Record.Create(self);
          Result := Read({%H-}d, 4) and (d <> 0);
          if not Result then begin
            curr.Free;
            CMD_Err2(erUnexpectedEof, TbtString(RPS_UnexpectedEof));
            Exit;
          end;
          while d > 0 do begin
            Result := Read({%H-}l2, 4);
            if not Result then begin
              curr.Free;
              CMD_Err2(erUnexpectedEof, TbtString(RPS_UnexpectedEof));
              Exit;
            end;
            Result := Cardinal(l2) < FTypes.Count;
            if not Result then begin
              curr.Free;
              CMD_Err2(erOutOfRange, TbtString(RPS_OutOfRange));
              Exit;
            end;
            TPSTypeRec_Record(curR).FFieldTypes.Add(FTypes[l2]);
            Dec(D);
          end;
          Result := Resolve(TPSTypeRec_Record(curr));
          if not Result then begin
            curr.Free;
            CMD_Err2(erInvalidType, TbtString(RPS_InvalidType));
            Exit;
          end;
          Curr.BaseType := currf.BaseType;
          FTypes.Add(Curr);
        end; // btRecord
        else begin
          Result := False; //!!
          CMD_Err2(erInvalidType, TbtString(RPS_InvalidType));
          Exit;
        end;
      end; // case currf.BaseType
      //
      if fe then begin
        Result := Read({%H-}d, 4);
        if not Result then begin
          CMD_Err2(erUnexpectedEof, TbtString(RPS_UnexpectedEof));
          Exit;
        end;
        Result := d <= PSAddrNegativeStackStart;
        if not Result then begin
          CMD_Err2(erInvalidType, TbtString(RPS_InvalidType));
          Exit;
        end;
        SetLength(Curr.FExportName, d);
        Result := Read(Curr.fExportName[1], d*btCharSize);
        if not Result then begin
          CMD_Err2(erUnexpectedEof, TbtString(RPS_UnexpectedEof));
          Exit;
        end;
        Curr.ExportNameHash := MakeHash(Curr.ExportName);
      end; // if fe
      //
      curr.CalcSize();
      if HDR.PSBuildNo >= 21 then begin // since build 21 we support attributes
        Result := ReadAttributes(Curr.Attributes);
        if not Result then
          Exit;
      end;
    end; // for l
    //
    Result := True; //!!
  end; // function LoadTypes

  function LoadProcs: Boolean;
  var
    OK: Boolean;
    Rec: TPSProc;
    n: TbtString;
    b: Byte;
    l, L2, L3: Longint;
    Curr: TPSProcRec;
  begin
    Result := False;
    for l := 0 to HDR.ProcCount - 1 do begin
      OK := Read({%H-}Rec, SizeOf(Rec));
      if not OK then begin
        CMD_Err2(erUnexpectedEof, TbtString(RPS_UnexpectedEof));
        Exit;
      end;
      OK := (Rec.Flags and 1) <> 0;
      if OK then
      begin
        Curr := TPSExternalProcRec.Create(Self);
        OK := Read({%H-}b, 1);
        if not OK then begin
          Curr.Free;
          CMD_Err2(erUnexpectedEof, TbtString(RPS_UnexpectedEof));
          Exit;
        end;
        SetLength({%H-}n, b);
        OK := Read(n[1], b*btCharSize);
        if not OK then begin
          Curr.Free;
          CMD_Err2(erUnexpectedEof, TbtString(RPS_UnexpectedEof));
          Exit;
        end;
        TPSExternalProcRec(Curr).Name := n;
        OK := (Rec.Flags and 3 = 3);
        if OK then
        begin
          OK := Read({%H-}L2, 4) and (L2 <= Length(s) - Pos);
          if not OK then begin
            Curr.Free;
            CMD_Err2(erUnexpectedEof, TbtString(RPS_UnexpectedEof));
            Exit;
          end;
          SetLength(n, L2);
          Read(n[1], L2*btCharSize); // no check is needed
          TPSExternalProcRec(Curr).FDecl := n;
        end;
        OK := ImportProc(TPSExternalProcRec(Curr).Name, TPSExternalProcRec(Curr));
        if not OK then begin
          if TPSExternalProcRec(Curr).Name <> '' then
            CMD_Err2(erCannotImport, TPSExternalProcRec(Curr).Name)
          else
            CMD_Err2(erCannotImport, TPSExternalProcRec(curr).Decl);
          Curr.Free;
          Exit;
        end;
      end else begin
        Curr := TPSInternalProcRec.Create(Self);
        OK := Read({%H-}L2, 4);
        if not OK then begin
          Curr.Free;
          CMD_Err2(erUnexpectedEof, TbtString(RPS_UnexpectedEof));
          Exit;
        end;
        OK := Read({%H-}L3, 4);
        if not OK then begin
          Curr.Free;
          CMD_Err2(erUnexpectedEof, TbtString(RPS_UnexpectedEof));
          Exit;
        end;
        OK := (L2 < 0) or (L2 >= Length(S)) or (L2 + L3 > Length(S)) or (L3 = 0);
        if OK then begin
          Curr.Free;
          CMD_Err2(erUnexpectedEof, TbtString(RPS_UnexpectedEof));
          Exit;
        end;
        //
        GetMem(TPSInternalProcRec(Curr).FData, L3);
        Move(S[L2 + 1], TPSInternalProcRec(Curr).FData^, L3);
        TPSInternalProcRec(Curr).FLength := L3;
        OK := (Rec.Flags and 2) <> 0;
        if OK then begin // exported
          OK := Read(L3, 4);
          if not OK then begin
            Curr.Free;
            CMD_Err2(erUnexpectedEof, TbtString(RPS_UnexpectedEof));
            Exit;
          end;
          OK := L3 > PSAddrNegativeStackStart;
          if OK then begin
            Curr.Free;
            CMD_Err2(erUnexpectedEof, TbtString(RPS_UnexpectedEof));
            Exit;
          end;
          SetLength(TPSInternalProcRec(Curr).FExportName, L3);
          OK := Read(TPSInternalProcRec(Curr).FExportName[1], L3*btCharSize);
          if not OK then begin
            Curr.Free;
            CMD_Err2(erUnexpectedEof, TbtString(RPS_UnexpectedEof));
            Exit;
          end;
          OK := Read(L3, 4);
          if not OK then begin
            Curr.Free;
            CMD_Err2(erUnexpectedEof, TbtString(RPS_UnexpectedEof));
            Exit;
          end;
          OK := L3 > PSAddrNegativeStackStart;
          if OK then begin
            Curr.Free;
            CMD_Err2(erUnexpectedEof, TbtString(RPS_UnexpectedEof));
            Exit;
          end;
          SetLength(TPSInternalProcRec(Curr).FExportDecl, L3);
          OK := Read(TPSInternalProcRec(Curr).FExportDecl[1], L3*btCharSize);
          if not OK then begin
            Curr.Free;
            CMD_Err2(erUnexpectedEof, TbtString(RPS_UnexpectedEof));
            Exit;
          end;
          TPSInternalProcRec(Curr).FExportNameHash := MakeHash(TPSInternalProcRec(Curr).ExportName);
        end;
      end;
      OK := (Rec.Flags and 4) <> 0;
      if OK then
      begin
        OK := ReadAttributes(Curr.Attributes);
        if not OK then
        begin
          Curr.Free;
          {+}
          CMD_Err2(erCustomError, TbtString('Failed LoadProcs, ReadAttributes'));
          {+.}
          Exit;
        end;
      end;
      FProcs.Add(Curr);
    end; // for l
    //
    Result := True;//!!
  end; // function LoadProcs
  {$WARNINGS ON}

  function LoadVars: Boolean;
  var
    l, n: Longint;
    e: PPSExportedVar;
    Rec: TPSVar;
    Curr: PIfVariant;
  begin
    for l := 0 to HDR.VarCount - 1 do begin
      Result := Read({%H-}Rec, SizeOf(Rec));
      if not Result then begin
        CMD_Err2(erUnexpectedEof, TbtString(RPS_UnexpectedEof));
        Exit;
      end;
      Result := Rec.TypeNo < HDR.TypeCount;
      if not Result then begin
        CMD_Err2(erInvalidType, TbtString(RPS_InvalidType));
        Exit;
      end;
      Curr := FGlobalVars.PushType(FTypes.Data^[Rec.TypeNo]);
      Result := Assigned(Curr);
      if not Result then begin
        CMD_Err2(erInvalidType, TbtString(RPS_InvalidType));
        Exit;
      end;
      Result := (Rec.Flags and 1) <> 0;
      if Result then begin
        Result := Read({%H-}n, 4);
        if not Result then begin
          CMD_Err2(erUnexpectedEof, TbtString(RPS_UnexpectedEof));
          Exit;
        end;
        New(e);
        try
          SetLength(e^.FName, n);
          Result := Read(e^.FName[1], n*btCharSize);
          if not Result then begin
            Dispose(e);
            CMD_Err2(erUnexpectedEof, TbtString(RPS_UnexpectedEof));
            Exit;
          end;
          e^.FNameHash := MakeHash(e^.FName);
          e^.FVarNo := FGlobalVars.Count;
          FExportedVars.Add(e);
        except
          Dispose(e);
          CMD_Err2(erInvalidType, TbtString(RPS_InvalidType));
          Result := False;
          Exit;
        end;
      end; // if (Rec.Flags and 1) <> 0
    end; // for l
    //
    Result := True; //!!
  end; // function LoadVars

begin // function TPSExec.LoadData
  Clear();
  Pos := 0;
  Result := Read({%H-}HDR, SizeOf(HDR)); // @dbg: (HDR:1397769801; PSBuildNo:23; TypeCount:42; ProcCount:12; VarCount:8; MainProcNo:0; ImportTableSize:0)
  if not Result then begin
    CMD_Err2(erInvalidHeader, TbtString(RPS_InvalidHeader));
    Exit;
  end;
  Result := (HDR.HDR = PSValidHeader);
  if not Result then begin
    CMD_Err2(erInvalidHeader, TbtString(RPS_InvalidHeader));
    Exit;
  end;
  Result := (HDR.PSBuildNo <= PSCurrentBuildNo) and (HDR.PSBuildNo >= PSLowBuildSupport);
  if not Result then begin
    CMD_Err2(erInvalidHeader, TbtString(RPS_InvalidHeader));
    Exit;
  end;
  Result := LoadTypes();
  if not Result then begin
    Clear();
    {+}
    CMD_Err2(erCustomError, TbtString('Failed LoadTypes'));
    {+.}
    Exit;
  end;
  Result := LoadProcs();
  if not Result then begin
    Clear();
    {+}
    CMD_Err2(erCustomError, TbtString('Failed Define/Load Procs'));
    {+.}
    Exit;
  end;
  Result := LoadVars();
  if not Result then begin
    Clear();
    {+}
    CMD_Err2(erCustomError, TbtString('Failed LoadVars'));
    {+.}
    Exit;
  end;
  Result := (HDR.MainProcNo < FProcs.Count) or (HDR.MainProcNo = InvalidVal);
  if not Result then begin
    CMD_Err2(erNoMainProc, TbtString(RPS_NoMainProc));
    Clear();
    Exit;
  end;
  // Load Import Table
  FMainProc := HDR.MainProcNo;
  FStatus := isLoaded;
  Result := True;
end; // function TPSExec.LoadData

procedure TPSExec.Pause;
begin
  if FStatus = isRunning then
    FStatus := isPaused;
end;

function TPSExec.ReadData(var Data; Len: Cardinal): Boolean;
begin
  if FCurrentPosition + Len <= FDataLength then begin
    Move(FData[FCurrentPosition], Data, Len);
    FCurrentPosition := FCurrentPosition + Len;
    Result := True;
  end
  else
    Result := False;
end;

procedure TPSExec.CMD_Err(EC: TPSError); // Error
begin
  //CMD_Err3(ec, '', nil);
  CMD_Err3(ec, PSErrorToString(ec, ''), nil);
end;

procedure VNSetPointerTo(const Src: TPSVariantIFC; Data: Pointer; aType: TPSTypeRec);
begin
  if Src.aType.BaseType = btPointer then
  begin
    if atype.BaseType in NeedFinalization then
      FinalizeVariant(src.Dta, Src.aType);
    Pointer(Src.Dta^) := Data;
    Pointer(Pointer(IPointer(Src.Dta)+PointerSize)^) := aType;
    Pointer(Pointer(IPointer(Src.Dta)+(2*PointerSize))^) := nil;
  end;
end;

function VNGetUInt(const Src: TPSVariantIFC): Cardinal;
begin
  Result := PSGetUInt(Src.Dta, Src.aType);
end;

{$IFNDEF PS_NOINT64}
function VNGetInt64(const Src: TPSVariantIFC): Int64;
begin
  Result := PSGetInt64(Src.Dta, Src.aType);
end;
{$ENDIF}

function VNGetReal(const Src: TPSVariantIFC): Extended;
begin
  Result := PSGetReal(Src.Dta, Src.aType);
end;

function VNGetCurrency(const Src: TPSVariantIFC): Currency;
begin
  Result := PSGetCurrency(Src.Dta, Src.aType);
end;

function VNGetInt(const Src: TPSVariantIFC): Longint;
begin
  Result := PSGetInt(Src.Dta, Src.aType);
end;

function VNGetAnsiString(const Src: TPSVariantIFC): TbtString;
begin
  Result := PSGetAnsiString(Src.Dta, Src.aType);
end;

{$IFNDEF PS_NOWIDESTRING}
function VNGetWideString(const Src: TPSVariantIFC): tbtWideString;
begin
  Result := PSGetWideString(Src.Dta, Src.aType);
end;

function VNGetUnicodeString(const Src: TPSVariantIFC): TbtUnicodeString;
begin
  Result := PSGetUnicodeString(Src.Dta, Src.aType);
end;
{$ENDIF}

procedure VNSetUInt(const Src: TPSVariantIFC; const Val: Cardinal);
var Dummy: Boolean;
begin
  PSSetUInt(Src.Dta, Src.aType, {%H-}Dummy, Val);
end;

{$IFNDEF PS_NOINT64}
procedure VNSetInt64(const Src: TPSVariantIFC; const Val: Int64);
var Dummy: Boolean;
begin
  PSSetInt64(Src.Dta, Src.aType, {%H-}Dummy, Val);
end;
{$ENDIF}

procedure VNSetReal(const Src: TPSVariantIFC; const Val: Extended);
var Dummy: Boolean;
begin
  PSSetReal(Src.Dta, Src.aType, {%H-}Dummy, Val);
end;

procedure VNSetCurrency(const Src: TPSVariantIFC; const Val: Currency);
var Dummy: Boolean;
begin
  PSSetCurrency(Src.Dta, Src.aType, {%H-}Dummy, Val);
end;

procedure VNSetInt(const Src: TPSVariantIFC; const Val: Longint);
var Dummy: Boolean;
begin
  PSSetInt(Src.Dta, Src.aType, {%H-}Dummy, Val);
end;

procedure VNSetAnsiString(const Src: TPSVariantIFC; const Val: TbtString);
var Dummy: Boolean;
begin
  PSSetAnsiString(Src.Dta, Src.aType, {%H-}Dummy, Val);
end;

function VNGetString(const Src: TPSVariantIFC): string;
begin
  {$IFNDEF PS_NOWIDESTRING}
    {$IFDEF FPC}
      {$if btCharSize=1}
      Result := VNGetAnsiString(Src);
      {$else}
      Result := VNGetUnicodeString(Src);
      {$ifend}
    {$ELSE}
      {$IFDEF UNICODE}
      Result := VNGetUnicodeString(Src);
      {$ELSE}
      Result := VNGetAnsiString(Src);
      {$ENDIF}
    {$ENDIF}
  {$ELSE}
  Result := VNGetAnsiString(Src);
  {$ENDIF}
end;

procedure VNSetString(const Src: TPSVariantIFC; const Val: String);
begin
  {$IFNDEF PS_NOWIDESTRING}
    {$IFDEF FPC}
      {$if btCharSize=1}
      VNSetAnsiString(Src, Val);
      {$else}
      VNSetUnicodeString(Src, Val);
      {$ifend}
    {$ELSE}
      {$IFDEF UNICODE}
      VNSetUnicodeString(Src, Val);
      {$ELSE}
      VNSetAnsiString(Src, Val);
      {$ENDIF}
    {$ENDIF}
  {$ELSE}
  VNSetAnsiString(Src, Val);
  {$ENDIF}
end;

{$IFNDEF PS_NOWIDESTRING}
procedure VNSetWideString(const Src: TPSVariantIFC; const Val: tbtWideString);
var Dummy: Boolean;
begin
  PSSetWideString(Src.Dta, Src.aType, {%H-}Dummy, Val);
end;

procedure VNSetUnicodeString(const Src: TPSVariantIFC; const Val: TbtUnicodeString);
var Dummy: Boolean;
begin
  PSSetUnicodeString(Src.Dta, Src.aType, {%H-}Dummy, Val);
end;
{$ENDIF !PS_NOWIDESTRING}

function VGetUInt(const Src: PIFVariant): Cardinal;
begin
  Result := PSGetUInt(@PPSVariantData(src).Data, src.FType);
end;

{$IFNDEF PS_NOINT64}
function VGetInt64(const Src: PIFVariant): Int64;
begin
  Result := PSGetInt64(@PPSVariantData(src).Data, src.FType);
end;
{$ENDIF}

function VGetReal(const Src: PIFVariant): Extended;
begin
  Result := PSGetReal(@PPSVariantData(src).Data, src.FType);
end;

function VGetCurrency(const Src: PIFVariant): Currency;
begin
  Result := PSGetCurrency(@PPSVariantData(src).Data, src.FType);
end;

function VGetInt(const Src: PIFVariant): Longint;
begin
  Result := PSGetInt(@PPSVariantData(src).Data, src.FType);
end;

function VGetAnsiString(const Src: PIFVariant): TbtString;
begin
  Result := PSGetAnsiString(@PPSVariantData(src).Data, src.FType);
end;

{$IFNDEF PS_NOWIDESTRING}
function VGetWideString(const Src: PIFVariant): tbtWideString;
begin
  Result := PSGetWideString(@PPSVariantData(src).Data, src.FType);
end;

function VGetUnicodeString(const Src: PIFVariant): TbtUnicodeString;
begin
  Result := PSGetUnicodeString(@PPSVariantData(src).Data, src.FType);
end;
{$ENDIF !PS_NOWIDESTRING}

procedure VSetPointerTo(const Src: PIFVariant; Data: Pointer; aType: TPSTypeRec);
var
  temp: TPSVariantIFC;
begin
  if (Atype = nil) or (Data = nil) or (Src = nil) then
    raise Exception.Create(RPS_InvalidVariable);
  temp.Dta := @PPSVariantData(Src).Data;
  temp.aType := Src.FType;
  temp.VarParam := False;
  VNSetPointerTo(temp, Data, AType);
end;

procedure VSetUInt(const Src: PIFVariant; const Val: Cardinal);
var Dummy: Boolean;
begin
  PSSetUInt(@PPSVariantData(src).Data, src.FType, {%H-}Dummy, Val);
end;

{$IFNDEF PS_NOINT64}
procedure VSetInt64(const Src: PIFVariant; const Val: Int64);
var Dummy: Boolean;
begin
  PSSetInt64(@PPSVariantData(src).Data, src.FType, {%H-}Dummy, Val);
end;
{$ENDIF !PS_NOINT64}

procedure VSetReal(const Src: PIFVariant; const Val: Extended);
var Dummy: Boolean;
begin
  PSSetReal(@PPSVariantData(src).Data, src.FType, {%H-}Dummy, Val);
end;

procedure VSetCurrency(const Src: PIFVariant; const Val: Currency);
var Dummy: Boolean;
begin
  PSSetCurrency(@PPSVariantData(src).Data, src.FType, {%H-}Dummy, Val);
end;

procedure VSetInt(const Src: PIFVariant; const Val: Longint);
var Dummy: Boolean;
begin
  PSSetInt(@PPSVariantData(src).Data, src.FType, {%H-}Dummy, Val);
end;

procedure VSetAnsiString(const Src: PIFVariant; const Val: TbtString);
var Dummy: Boolean;
begin
  PSSetAnsiString(@PPSVariantData(src).Data, src.FType, {%H-}Dummy, Val);
end;

function VGetString(const Src: PIFVariant): String;
begin
  {$IFNDEF PS_NOWIDESTRING}
    {$IFDEF FPC}
      {$if btCharSize=1}
      Result := PSGetAnsiString(@PPSVariantData(src).Data, src.FType);
      {$else}
      Result := PSGetUnicodeString(@PPSVariantData(src).Data, src.FType);
      {$ifend}
    {$ELSE}
      {$IFDEF UNICODE}
      Result := PSGetUnicodeString(@PPSVariantData(src).Data, src.FType);
      {$ELSE}
      Result := PSGetAnsiString(@PPSVariantData(src).Data, src.FType);
      {$ENDIF}
    {$ENDIF}
  {$ELSE PS_NOWIDESTRING}
  Result := PSGetAnsiString(@PPSVariantData(src).Data, src.FType);
  {$ENDIF PS_NOWIDESTRING}
end;

procedure VSetString(const Src: PIFVariant; const Val: string);
var
  Dummy: Boolean;
begin
  {$IFNDEF PS_NOWIDESTRING}
    {$IFDEF FPC}
      {$if btCharSize=1}
      PSSetAnsiString(@PPSVariantData(src).Data, src.FType, {%H-}Dummy, Val);
      {$else}
      PSSetUnicodeString(@PPSVariantData(src).Data, src.FType, Dummy, Val);
      {$ifend}
    {$ELSE}
      {$IFDEF UNICODE}
      PSSetUnicodeString(@PPSVariantData(src).Data, src.FType, Dummy, Val);
      {$ELSE}
      PSSetAnsiString(@PPSVariantData(src).Data, src.FType, {%H-}Dummy, Val);
      {$ENDIF}
    {$ENDIF}
  {$ELSE PS_NOWIDESTRING}
  PSSetAnsiString(@PPSVariantData(src).Data, src.FType, {%H-}Dummy, Val);
  {$ENDIF PS_NOWIDESTRING}
end;

{$IFNDEF PS_NOWIDESTRING}
procedure VSetWideString(const Src: PIFVariant; const Val: tbtWideString);
var
  Dummy: Boolean;
begin
  PSSetWideString(@PPSVariantData(src).Data, src.FType, {%H-}Dummy, Val);
end;

procedure VSetUnicodeString(const Src: PIFVariant; const Val: TbtUnicodeString);
var
  Dummy: Boolean;
begin
  PSSetUnicodeString(@PPSVariantData(src).Data, src.FType, {%H-}Dummy, Val);
end;

function VarToWideStr(const Data: Variant): TbtUnicodeString;
begin
  {+}
  if not (VarIsEmpty(Data) or VarIsNull(Data)) then
  {+.}
    Result := Data
  else
    Result := '';
end;
{$ENDIF !PS_NOWIDESTRING}

function PSGetUInt(Src: Pointer; aType: TPSTypeRec): Cardinal;
begin
  if aType.BaseType = btPointer then
  begin
    aType := PIFTypeRec(Pointer(IPointer(Src)+PointerSize)^);
    Src := Pointer(Src^);
    if (Src = nil) or (aType = nil) then
      raise Exception.Create(RPS_TypeMismatch);
  end;
  case aType.BaseType of
    btU8: Result := tbtU8(Src^);
    btS8: Result := tbtS8(Src^);
    btU16: Result := tbtU16(Src^);
    btS16: Result := tbtS16(Src^);
    btU32: Result := tbtU32(Src^);
    btS32: Result := tbtS32(Src^);
    {$IFNDEF PS_NOINT64}
    btS64: Result := tbtS64(src^);
    {$ENDIF !PS_NOINT64}
    btChar: Result := Ord(tbtChar(Src^));
    {$IFNDEF PS_NOWIDESTRING}
    btWideChar: Result := Ord(TbtWideChar(Src^));
    {$ENDIF !PS_NOWIDESTRING}
    btVariant:
      {+}
      //case VarType(Variant(Src^)) of
      case TVarData(Variant(Src^)).VType of
      {+.}
        varString:
          if Length(VarToStr(Variant(Src^))) = 1 then
            Result := Ord(VarToStr(Variant(Src^))[1])
          else
            raise Exception.Create(RPS_TypeMismatch);
        {$IFNDEF PS_NOWIDESTRING}
        varOleStr:
          if Length(VarToWideStr(Variant(Src^))) = 1 then
            Result := Ord(VarToWideStr(Variant(Src^))[1])
          else
            raise Exception.Create(RPS_TypeMismatch);
        {$ENDIF !PS_NOWIDESTRING}
       else
        Result := Variant(src^);
       end;
    {+}
    btSet: Result := tbtU8(Src^);
    {+.}
    else raise Exception.Create(RPS_TypeMismatch);
  end;
end;

function PSGetObject(Src: Pointer; aType: TPSTypeRec): TObject;
begin
  if aType.BaseType = btPointer then
  begin
    aType := PIFTypeRec(Pointer(IPointer(Src)+PointerSize)^);
    Src := Pointer(Src^);
    if (Src = nil) or (aType = nil) then
      raise Exception.Create(RPS_TypeMismatch);
  end;
  case aType.BaseType of
    btClass: Result := TObject(Src^);
    else raise Exception.Create(RPS_TypeMismatch);
  end;
end;

procedure PSSetObject(Src: Pointer; aType: TPSTypeRec; var Ok: Boolean; Const val: TObject);
begin
  if aType.BaseType = btPointer then
  begin
    aType := PIFTypeRec(Pointer(IPointer(Src)+PointerSize)^);
    Src := Pointer(Src^);
    if (Src = nil) or (aType = nil) then
      raise Exception.Create(RPS_TypeMismatch);
  end;
  case aType.BaseType of
    btClass: TObject(Src^) := Val;
    else raise Exception.Create(RPS_TypeMismatch);
  end;
end;

{$IFNDEF PS_NOINT64}
function PSGetInt64(Src: Pointer; aType: TPSTypeRec): Int64;
begin
  if aType.BaseType = btPointer then
  begin
    aType := PIFTypeRec(Pointer(IPointer(Src)+PointerSize)^);
    Src := Pointer(Src^);
    if (Src = nil) or (aType = nil) then
      raise Exception.Create(RPS_TypeMismatch);
  end;
  case aType.BaseType of
    btU8: Result := tbtu8(Src^);
    btS8: Result := tbtS8(Src^);
    btU16: Result := tbtU16(Src^);
    btS16: Result := tbtS16(Src^);
    btU32: Result := tbtU32(Src^);
    btS32: Result := tbtS32(Src^);
    btS64: Result := tbtS64(Src^);
    btChar: Result := Ord(tbtChar(Src^));
    {$IFNDEF PS_NOWIDESTRING}
    btWideChar: Result := Ord(TbtWideChar(Src^));
    {$ENDIF !PS_NOWIDESTRING}
    btVariant:   Result := Variant(Src^);
    {+}
    {$IFDEF CPU64}
    {?}btPointer, btPChar, btClass, btInterface {$IFNDEF PS_NOWIDESTRING}{$if declared(btPWideChar)},btPWideChar{$ifend}{$ENDIF}:
    Result := tbtS64(Src^);
    {$ENDIF CPU64}
    {+.}
    else raise Exception.Create(RPS_TypeMismatch);
  end;
end;
{$ENDIF !PS_NOINT64}

function PSGetReal(Src: Pointer; aType: TPSTypeRec): Extended;
begin
  if aType.BaseType = btPointer then
  begin
    aType := PIFTypeRec(Pointer(IPointer(Src)+PointerSize)^);
    Src := Pointer(Src^);
    if (Src = nil) or (aType = nil) then
      raise Exception.Create(RPS_TypeMismatch);
  end;
  case aType.BaseType of
    btU8: Result := tbtU8(Src^);
    btS8: Result := tbtS8(Src^);
    btU16: Result := tbtU16(Src^);
    btS16: Result := tbtS16(Src^);
    btU32: Result := tbtU32(Src^);
    btS32: Result := tbtS32(Src^);
    {$IFNDEF PS_NOINT64}
    btS64: Result := tbtS64(Src^);
    {$ENDIF}
    btSingle: Result := tbtSingle(Src^);
    btDouble: Result := tbtDouble(Src^);
    btExtended: Result := tbtExtended(Src^);
    btCurrency: Result := tbtCurrency(Src^);
    btVariant:  Result := Variant(Src^);
    else raise Exception.Create(RPS_TypeMismatch);
  end;
end;

function PSGetCurrency(Src: Pointer; aType: TPSTypeRec): Currency;
begin
  if aType.BaseType = btPointer then
  begin
    aType := PIFTypeRec(Pointer(IPointer(Src)+PointerSize)^);
    Src := Pointer(Src^);
    if (Src = nil) or (aType = nil) then
      raise Exception.Create(RPS_TypeMismatch);
  end;
  case aType.BaseType of
    btU8:
      Result := tbtU8(Src^);
    btS8:
      Result := tbtS8(Src^);
    btU16:
      Result := tbtU16(Src^);
    btS16:
      Result := tbtS16(Src^);
    btU32:
      Result := tbtU32(Src^);
    btS32:
      Result := tbtS32(Src^);
    {$IFNDEF PS_NOINT64}
    btS64:
      Result := tbtS64(Src^);
    {$ENDIF}
    btSingle:
      Result := tbtSingle(Src^);
    btDouble:
      Result := tbtDouble(Src^);
    btExtended:
      Result := tbtExtended(Src^);
    btCurrency:
      Result := tbtCurrency(Src^);
    btVariant:
      Result := Variant(Src^);
    else
      raise Exception.Create(RPS_TypeMismatch);
  end;
end;

function PSGetInt(Src: Pointer; aType: TPSTypeRec): Longint;
begin
  if aType.BaseType = btPointer then
  begin
    aType := PIFTypeRec(Pointer(IPointer(Src)+PointerSize)^);
    Src := Pointer(Src^);
    if (Src = nil) or (aType = nil) then
      raise Exception.Create(RPS_TypeMismatch);
  end;
  case aType.BaseType of
    btU8:
      Result := tbtU8(Src^);
    btS8:
      Result := tbtS8(Src^);
    btU16:
      Result := tbtU16(Src^);
    btS16:
      Result := tbtS16(Src^);
    btU32:
      Result := tbtU32(Src^);
    btS32:
      Result := tbtS32(Src^);
    {$IFNDEF PS_NOINT64}
    btS64:
      Result := tbtS64(Src^);
    {$ENDIF}
    btChar:
      Result := Ord(tbtChar(Src^));
    {$IFNDEF PS_NOWIDESTRING}
    btWideChar:
      Result := Ord(TbtWideChar(Src^));
    {$ENDIF}
    btVariant:
      Result := Variant(Src^);
    else
      raise Exception.Create(RPS_TypeMismatch);
  end;
end;

{+}
function IsIntType(b: TPSBaseType): Boolean;
begin
  case b of
    btU8, btS8, btU16, btS16, btU32, btS32{$IFNDEF PS_NOINT64}, btS64{$ENDIF}: Result := True;
  else
    Result := False;
  end;
end;

function IsStrType(b: TPSBaseType): Boolean;
begin
  case b of
    btChar, btPChar, btString{$IFNDEF PS_NOWIDESTRING},btWideChar,{$if declared(btPWideChar)}btPWideChar,{$ifend}btWideString,btUnicodeString{$ENDIF}:
      Result := True;
  else
    Result := False;
  end;
end;
{+.}

function PSGetAnsiChar(Src: Pointer; aType: TPSTypeRec): tbtChar;
var Res : TbtString;
begin
  Res := PSGetAnsiString(Src,aType);
  if Length(Res) > 0 then
    Result := Res[{$IFDEF UNICODE}Low(Res){$ELSE}1{$ENDIF}]
  else
    Result := #0;
end;

function PSGetAnsiString(Src: Pointer; aType: TPSTypeRec): TbtString;
var n: integer;
begin
  if aType.BaseType = btPointer then
  begin
    aType := PIFTypeRec(Pointer(IPointer(Src)+PointerSize)^);
    Src := Pointer(Src^);
    if (Src = nil) or (aType = nil) then
      raise Exception.Create(RPS_TypeMismatch);
  end;
  case aType.BaseType of
    btU8: Result := tbtChar(tbtU8(Src^));
    {+}
    {$IFNDEF PS_NOWIDESTRING}
    btU16: Result := TbtString(WideString( WideChar(Src^) )); //1086
    {$ENDIF}
    btU32, btS32: begin
      {$IFNDEF PS_NOWIDESTRING}
        Result := TbtString(WideString(WideChar(Word(tbtU32(Src^)))));
      {$ELSE}
        {$if btCharIsWide}
        Result := TbtString(WideString(WideChar(Word(tbtU32(Src^)))));
        {$else}
        Result := TbtString(AnsiChar(Word(tbtU32(Src^))));
        {$ifend}
      {$ENDIF}
    end;
   {$IFNDEF PS_NOINT64}
    btS64: begin
      {$IFNDEF PS_NOWIDESTRING}
        Result := TbtString(WideString(WideChar(Word(tbtS64(src^)))));
      {$ELSE}
        {$if btCharIsWide}
        Result := TbtString(WideString(WideChar(Word(tbtS64(src^)))));
        {$else}
        Result := TbtString(AnsiChar(Word(tbtS64(Src^))));
        {$ifend}
      {$ENDIF}
    end;
    {$ENDIF !PS_NOINT64}
    {+.}
    btChar: Result := tbtChar(Src^);
    {+}
    btPChar: begin
      Result := TbtString(PTbtChar(Src^)); //todo: detect memory AV
    end;
    {+.}
    {+} // debug: pansichar(src^)    pwidechar(src^)
    {$IFNDEF PS_NOWIDESTRING}
      {$if declared(btPWideChar)}
    btPWideChar: begin
      Result := TbtString(WideString(PWideChar(src^)));
    end;
      {$ifend}
    {$ENDIF}
    {+.}
    {$IFNDEF PS_NOWIDESTRING}
    btWideChar: Result := TbtString(TbtWideChar(Src^));
    {$ENDIF}
    btString: Result := TbtString(Src^);
    {$IFNDEF PS_NOWIDESTRING}
    btUnicodeString: Result := TbtString(TbtUnicodeString(Src^));
    btWideString: Result := TbtString(tbtWideString(Src^));
    {$ENDIF}
    btVariant:  Result := TbtString(VarToStr(Variant(Src^)));
    {+}
    btArray: begin // dbg: TPSTypeRec_Array(aType),r; TPSTypeRec_Array(aType).FArrayType,r
      if IsIntType(TPSTypeRec_Array(aType).FArrayType.FBaseType) then
      begin // autocopy Array Of IntType into AnsiString
        n := PSDynArrayGetLength(Pointer(Src^), aType);
        n := n * {elSize:}LongInt(TPSTypeRec_Array(aType).FArrayType.FRealSize);
        {$if btCharIsWide}
        SetLength(Result, (n+1) div 2);
        if n > 0 then
          Result[(n+1) div 2] := TbtChar(#0);
        {$else}
        SetLength(Result, n);
        {$ifend}
        if n > 0 then
        begin // dbg: ptbtchar(@(PDynArrayRec(PointerShift(Pointer(src^),-SizeOf(TDynArrayRecHeader))).datas))
          Move({src:}Pointer(@(PDynArrayRec(PointerShift(Pointer(Src^),-SizeOf(TDynArrayRecHeader))).datas))^,
            {dest:}Pointer(Result)^, {size:}n);
        end;
      end
      else
      begin
        raise Exception.Create(RPS_TypeMismatch);
      end;
    end;
    btStaticArray: begin
      //Result := ''; //TODO:...
      raise Exception.Create(RPS_TypeMismatch);
    end;
    {+.}
    else raise Exception.Create(RPS_TypeMismatch);
  end;
end;

{$IFNDEF PS_NOWIDESTRING}
function PSGetWideChar(Src: Pointer; aType: TPSTypeRec): TbtWideChar;
var Res : tbtWideString;
begin
  Res := PSGetWideString(Src,aType);
  if Length(Res) > 0 then
    Result := Res[{$IFDEF UNICODE}Low(Res){$ELSE}1{$ENDIF}]
  else
    Result := #0;
end;

function PSGetWideString(Src: Pointer; aType: TPSTypeRec): tbtWideString;
var
  n: integer;
begin
  if aType.BaseType = btPointer then
  begin
    aType := PIFTypeRec(Pointer(IPointer(Src)+PointerSize)^);
    Src := Pointer(Src^);
    if (Src = nil) or (aType = nil) then
      raise Exception.Create(RPS_TypeMismatch);
  end;
  case aType.BaseType of
    btU8: Result := WideChar(TbtU8(Src^));
    btU16: Result := WideChar(Src^);
    {+}
    btU32, btS32: Result := tbtWideString(WideChar(Word(tbtU32(Src^))));
    {$IFNDEF PS_NOINT64}
    btS64: Result := tbtWideString(WideChar(Word(tbtS64(Src^))));
    {$ENDIF !PS_NOINT64}
    {+.}
    btChar: Result := tbtWideString(tbtChar(Src^));
    btPChar: Result := tbtWideString(PAnsiChar(Src^));
    {+}
    {$if declared(btPWideChar)}
    btPWideChar: Result := tbtWideString(WideString(PWideChar(Src^)));
    {$ifend}
    {+.}
    btWideChar: Result := TbtWideChar(Src^);
    btString: Result := tbtWideString(TbtString(Src^));
    btWideString: Result := tbtWideString(Src^);
    btVariant:   Result := Variant(Src^);
    btUnicodeString: Result := TbtUnicodeString(Src^);
    {+}
    btArray: begin // dbg: TPSTypeRec_Array(aType),r; TPSTypeRec_Array(aType).FArrayType,r
      if IsIntType(TPSTypeRec_Array(aType).FArrayType.FBaseType) then
      begin // autocopy Array Of IntType into WideString
        n := PSDynArrayGetLength(Pointer(Src^), aType);
        n := n * {elSize:}LongInt(TPSTypeRec_Array(aType).FArrayType.FRealSize);
        n := n div 2;
        SetLength(Result, n);
        if n > 0 then
        begin
          n := n * 2;
          // TODO: ? analyze/remove first BOM: {#$FEFF, "UNICODE MARKER (BOM)"}
          Move({src:}Pointer(@(PDynArrayRec(PointerShift(Pointer(Src^),-SizeOf(TDynArrayRecHeader))).datas))^, Pointer(Result)^, n);
        end;
      end
      else
      begin
        raise Exception.Create(RPS_TypeMismatch);
      end;
    end;
    btStaticArray: begin
      //Result := ''; //TODO:...
      raise Exception.Create(RPS_TypeMismatch);
    end;
    {+.}
    else raise Exception.Create(RPS_TypeMismatch);
  end;
end;
{$ENDIF !PS_NOWIDESTRING}

function PSGetUnicodeString(Src: Pointer; aType: TPSTypeRec): TbtUnicodeString;
var
  n: integer;
begin
  if aType.BaseType = btPointer then
  begin
    aType := PIFTypeRec(Pointer(IPointer(Src)+PointerSize)^);
    Src := Pointer(Src^);
    if (Src = nil) or (aType = nil) then
      raise Exception.Create(RPS_TypeMismatch);
  end;
  case aType.BaseType of
    btU8: Result := TbtUnicodeString(Chr(tbtU8(Src^)));
    btU16: Result := TbtUnicodeString(WideChar(Src^));
    {+}
    btU32, btS32: Result := TbtUnicodeString(WideChar(Word(tbtU32(Src^))));
    {$IFNDEF PS_NOINT64}
    btS64: Result := TbtUnicodeString(WideChar(Word(tbtS64(Src^))));
    {$ENDIF !PS_NOINT64}
    {+.}
    btChar: Result := TbtUnicodeString(tbtChar(Src^));
    {+}
    btPChar: Result := TbtUnicodeString(TbtString(PTbtChar(Src^)));
    {+.}
    {+}
    {$if declared(btPWideChar)}
    btPWideChar: Result := TbtUnicodeString(WideString(PWideChar(src^)));
    {$ifend}
    {+.}
    btString: Result := TbtUnicodeString(TbtString(Src^));
    {$IFNDEF PS_NOWIDESTRING}
    btWideChar: Result := TbtUnicodeString(WideChar(Src^));
    btWideString: Result := TbtUnicodeString(tbtWideString(Src^));
    {$ENDIF}
    btVariant:   Result := TbtUnicodeString(Variant(Src^));
    btUnicodeString: Result := TbtUnicodeString(Src^);
    {+}
    btArray: begin // dbg: TPSTypeRec_Array(aType),r; TPSTypeRec_Array(aType).FArrayType,r
      if IsIntType(TPSTypeRec_Array(aType).FArrayType.FBaseType) then
      begin // autocopy Array Of IntType into WideString
        n := PSDynArrayGetLength(Pointer(Src^), aType);
        n := n * {elSize:}LongInt(TPSTypeRec_Array(aType).FArrayType.FRealSize);
        n := n div 2; // elSize := TPSTypeRec_Array(aType).ArrayType.RealSize;
        SetLength(Result, n);
        if n > 0 then
        begin // dbg: PWideChar(@(PDynArrayRec(PointerShift(Pointer(Src^),-SizeOf(TDynArrayRecHeader))).datas))
          n := n * 2;
          // TODO: ? analyze/remove first BOM: {#$FEFF, "UNICODE MARKER (BOM)"}
          Move({src:}Pointer(@(PDynArrayRec(PointerShift(Pointer(Src^),-SizeOf(TDynArrayRecHeader))).datas))^, Pointer(Result)^, n);
        end;
      end
      else
      begin
        raise Exception.Create(RPS_TypeMismatch);
      end;
    end;
    btStaticArray: begin
      //Result := ''; //TODO:...
      raise Exception.Create(RPS_TypeMismatch);
    end;
    {+.}
    else raise Exception.Create(RPS_TypeMismatch);
  end;
end;

procedure PSSetUInt(Src: Pointer; aType: TPSTypeRec; var Ok: Boolean; const Val: Cardinal);
begin
  if (Src = nil) or (aType = nil) then begin
    Ok := False;
    Exit;
  end;
  if aType.BaseType = btPointer then
  begin
    aType := PIFTypeRec(Pointer(IPointer(Src)+PointerSize)^);
    Src := Pointer(Src^);
    if (Src = nil) or (aType = nil) then begin
      Ok := False;
      Exit;
    end;
  end;
  case aType.BaseType of
    btU8: tbtU8(Src^) := Val;
    btS8: tbtS8(Src^) := Val;
    btU16: tbtU16(Src^) := Val;
    btS16: tbtS16(Src^) := Val;
    btProcPtr:
      begin
        tbtU32(Src^) := Val;
        Pointer(Pointer(IPointer(Src)+PointerSize)^) := nil;
        Pointer(Pointer(IPointer(Src)+PointerSize2)^) := nil;
      end;
    btU32: tbtU32(Src^) := Val;
    btS32: tbtS32(Src^) := Val;
    {$IFNDEF PS_NOINT64}
    btS64: tbtS64(Src^) := Val;
    {$ENDIF}
    btChar: tbtChar(Src^) := tbtChar(Val);
    {$IFNDEF PS_NOWIDESTRING}
    btWideChar: TbtWideChar(Src^) := TbtWideChar(Val);
    {$ENDIF}
    btSingle: tbtSingle(Src^) := Val;
    btDouble: tbtDouble(Src^) := Val;
    btCurrency: tbtCurrency(Src^) := Val;
    btExtended: tbtExtended(Src^) := Val;
    btVariant:
      begin
        try
          Variant(Src^) := {$IFDEF DELPHI6UP}Val{$ELSE}tbtS32(Val){$ENDIF};
        except
          Ok := False;
        end;
      end;
    else Ok := False;
  end;
end;

{$IFNDEF PS_NOINT64}
procedure PSSetInt64(Src: Pointer; aType: TPSTypeRec; var Ok: Boolean; const Val: Int64);
begin
  if (Src = nil) or (aType = nil) then begin
    Ok := False;
    Exit;
  end;
  if aType.BaseType = btPointer then
  begin
    aType := PIFTypeRec(Pointer(IPointer(Src)+PointerSize)^);
    Src := Pointer(Src^);
    if (Src = nil) or (aType = nil) then begin
      Ok := False;
      Exit;
    end;
  end;
  case aType.BaseType of
    btU8: tbtU8(Src^) := Val;
    btS8: tbtS8(Src^) := Val;
    btU16: tbtU16(Src^) := Val;
    btS16: tbtS16(Src^) := Val;
    btU32: tbtU32(Src^) := Val;
    btS32: tbtS32(Src^) := Val;
    btS64: tbtS64(Src^) := Val;
    btChar: tbtChar(Src^) := tbtChar(Val);
    {$IFNDEF PS_NOWIDESTRING}
    btWideChar: TbtWideChar(Src^) := TbtWideChar(Val);
    {$ENDIF}
    btSingle: tbtSingle(Src^) := Val;
    btDouble: tbtDouble(Src^) := Val;
    btCurrency: tbtCurrency(Src^) := Val;
    btExtended: tbtExtended(Src^) := Val;
    {$IFDEF DELPHI6UP}
    btVariant:
      begin
        try
          Variant(Src^) := Val;
        except
          Ok := False;
        end;
      end;
    {$ENDIF}
    else
      Ok := False;
  end;
end;
{$ENDIF !PS_NOINT64}

procedure PSSetReal(Src: Pointer; aType: TPSTypeRec; var Ok: Boolean; const Val: Extended);
begin
  if (Src = nil) or (aType = nil) then begin
    Ok := False;
    Exit;
  end;
  if aType.BaseType = btPointer then
  begin
    aType := PIFTypeRec(Pointer(IPointer(Src)+PointerSize)^);
    Src := Pointer(Src^);
    if (Src = nil) or (aType = nil) then begin
      Ok := False;
      Exit;
    end;
  end;
  case aType.BaseType of
    btSingle: tbtSingle(Src^) := Val;
    btDouble: tbtDouble(Src^) := Val;
    btCurrency: tbtCurrency(Src^) := Val;
    btExtended: tbtExtended(Src^) := Val;
    btVariant:
      begin
        try
          Variant(Src^) := Val;
        except
          Ok := False;
        end;
      end;
    else
      Ok := False;
  end;
end;

procedure PSSetCurrency(Src: Pointer; aType: TPSTypeRec; var Ok: Boolean; const Val: Currency);
begin
  if (Src = nil) or (aType = nil) then begin
    Ok := False;
    Exit;
  end;
  if aType.BaseType = btPointer then
  begin
    aType := PIFTypeRec(Pointer(IPointer(Src)+PointerSize)^);
    Src := Pointer(Src^);
    if (Src = nil) or (aType = nil) then begin
      Ok := False;
      Exit;
    end;
  end;
  case aType.BaseType of
    btSingle: tbtSingle(Src^) := Val;
    btDouble: tbtDouble(Src^) := Val;
    btCurrency: tbtCurrency(Src^) := Val;
    btExtended: tbtExtended(Src^) := Val;
    btVariant:
      begin
        try
          Variant(Src^) := Val;
        except
          Ok := False;
        end;
      end;
    else
      Ok := False;
  end;
end;

procedure PSSetInt(Src: Pointer; aType: TPSTypeRec; var Ok: Boolean; const Val: Longint);
begin
  if (Src = nil) or (aType = nil) then begin
    Ok := False;
    Exit;
  end;
  if aType.BaseType = btPointer then
  begin
    aType := PIFTypeRec(Pointer(IPointer(Src)+PointerSize)^);
    Src := Pointer(Src^);
    if (Src = nil) or (aType = nil) then begin
      Ok := False;
      Exit;
    end;
  end;
  case aType.BaseType of
    btU8: tbtU8(Src^) := Val;
    btS8: tbtS8(Src^) := Val;
    btU16: tbtU16(Src^) := Val;
    btS16: tbtS16(Src^) := Val;
    btProcPtr:
      begin
        tbtU32(Src^) := Val;
        Pointer(Pointer(IPointer(Src)+PointerSize)^) := nil;
        Pointer(Pointer(IPointer(Src)+PointerSize2)^) := nil;
      end;
    btU32: tbtU32(Src^) := Val;
    btS32: tbtS32(Src^) := Val;
    {$IFNDEF PS_NOINT64}
    btS64: tbtS64(Src^) := Val;
    {$ENDIF}
    btChar: tbtchar(Src^) := tbtChar(Val);
    {$IFNDEF PS_NOWIDESTRING}
    btWideChar: TbtWideChar(Src^) := TbtWideChar(Val);
    {$ENDIF}
    btSingle: tbtSingle(Src^) := Val;
    btDouble: tbtDouble(Src^) := Val;
    btCurrency: tbtCurrency(Src^) := Val;
    btExtended: tbtExtended(Src^) := Val;
    btVariant:
      begin
        try
          Variant(Src^) := Val;
        except
          Ok := False;
        end;
      end;
    else
      Ok := False;
  end;
end;

procedure PSSetAnsiString(Src: Pointer; aType: TPSTypeRec; var Ok: Boolean; const Val: TbtString);
begin
  if (Src = nil) or (aType = nil) then begin
    Ok := False;
    Exit;
  end;
  if aType.BaseType = btPointer then
  begin
    aType := PIFTypeRec(Pointer(IPointer(Src)+PointerSize)^);
    Src := Pointer(Src^);
    if (Src = nil) or (aType = nil) then begin
      Ok := False;
      Exit;
    end;
  end;
  case aType.BaseType of
    btString: TbtString(Src^) := Val;
    btChar:
      if (Length(Val) > 0) then
        TbtChar(Src^) := Val[1];
    {$IFNDEF PS_NOWIDESTRING}
    btUnicodeString: TbtUnicodeString(Src^) := TbtUnicodeString(Val);
    btWideString: TbtWideString(Src^) := TbtWideString(Val);
    btWideChar: if (Val <> '') then TbtWideChar(Src^) := TbtWideChar(WideString(TbtString(Val[1]))[1]);
    {$ENDIF !PS_NOWIDESTRING}
    btVariant:
      begin
        try
          Variant(Src^) := Val;
        except
          Ok := False;
        end;
      end;
    else
      Ok := False;
  end;
end;

{$IFNDEF PS_NOWIDESTRING}
procedure PSSetWideString(Src: Pointer; aType: TPSTypeRec; var Ok: Boolean; const Val: tbtWideString);
begin
  if (Src = nil) or (aType = nil) then begin
    Ok := False;
    Exit;
  end;
  if aType.BaseType = btPointer then
  begin
    aType := PIFTypeRec(Pointer(IPointer(Src)+PointerSize)^);
    Src := Pointer(Src^);
    if (Src = nil) or (aType = nil) then begin
      Ok := False;
      Exit;
    end;
  end;
  case aType.BaseType of
    btChar: if Val <> '' then tbtChar(Src^) := tbtChar(Val[1]);
    btWideChar: if Val <> '' then TbtWideChar(Src^) := Val[1];
    btString: TbtString(Src^) := TbtString(Val);
    btWideString: tbtwideString(Src^) := Val;
    btUnicodeString: TbtUnicodeString(Src^) := Val;
    btVariant:
      begin
        try
          Variant(Src^) := Val;
        except
          Ok := False;
        end;
      end;
    else
      Ok := False;
  end;
end;
{$ENDIF !PS_NOWIDESTRING}

procedure PSSetUnicodeString(Src: Pointer; aType: TPSTypeRec; var Ok: Boolean; const Val: TbtUnicodeString);
begin
  if (Src = nil) or (aType = nil) then begin
    Ok := False;
    Exit;
  end;
  if aType.BaseType = btPointer then
  begin
    aType := PIFTypeRec(Pointer(IPointer(Src)+PointerSize)^);
    Src := Pointer(Src^);
    if (Src = nil) or (aType = nil) then begin
      OK := False;
      Exit;
    end;
  end;
  case aType.BaseType of
    btString:
      TbtString(Src^) := TbtString(Val);
    {$IFNDEF PS_NOWIDESTRING}
    btWideString:
      TbtWideString(Src^) := Val;
    {$ENDIF}
    btUnicodeString:
      TbtUnicodeString(Src^) := Val;
    btVariant: begin
      try
        Variant(Src^) := Val;
      except
        Ok := False;
      end;
    end;
    else Ok := False;
  end; // case
end;

function PSGetString(Src: Pointer; aType: TPSTypeRec): string;
begin
  {$IFNDEF PS_NOWIDESTRING}
    {$IFDEF UNICODE}
    Result := {+}string(PSGetUnicodeString(Src, aType)){+.};
    {$ELSE}
    Result := {+}string(PSGetAnsiString(Src, aType)){+.};
    {$ENDIF}
  {$ELSE}
  Result := {+}string(PSGetAnsiString(Src, aType)){+.};
  {$ENDIF}
end;

procedure PSSetString(Src: Pointer; aType: TPSTypeRec; var Ok: Boolean; const Val: String);
begin
  {$IFNDEF PS_NOWIDESTRING}
    {$IFDEF FPC}
      {$if btCharSize=1}
      PSSetAnsiString(Src, aType, Ok, Val);
      {$else}
      PSSetUnicodeString(Src, aType, Ok, Val);
      {$ifend}
    {$ELSE}
      {$IFDEF UNICODE}
      PSSetUnicodeString(Src, aType, Ok, Val);
      {$ELSE}
      PSSetAnsiString(Src, aType, Ok, Val);
      {$ENDIF}
    {$ENDIF}
  {$ELSE}
  PSSetAnsiString(Src, aType, Ok, Val);
  {$ENDIF}
end;

function CopyArrayContents(dest, src: Pointer; Len: Longint; aType: TPSTypeRec): Boolean; forward;

function CopyRecordContents(dest, src: Pointer; aType: TPSTypeRec_Record): Boolean;
var
  i: Longint;
  o: Cardinal;
begin
  for i := 0 to aType.FieldTypes.Count-1 do
  begin
    o := Cardinal(atype.RealFieldOffsets[i]);
    CopyArrayContents(Pointer(IPointer(Dest)+o), Pointer(IPointer(Src)+o), 1, aType.FieldTypes[i]);
  end;
  Result := True;
end;

function CreateArrayFromVariant(Exec: TPSExec; dest: Pointer; src: Variant; DestType: TPSTypeRec): Boolean;
var
  i: Integer;
  r: Pointer;
  lVarType: TPSTypeRec;
  v: Variant;
begin
  lVarType := Exec.FindType2(btVariant);
  Result := Assigned(lVarType);
  if not Result then
    Exit;
  PSDynArraySetLength(Pointer(dest^), DestType, VarArrayHighBound(src, 1) - VarArrayLowBound(src, 1) + 1);
  r := Pointer(Dest^);
  DestType := TPSTypeRec_Array(DestType).ArrayType;
  for i := 0 to VarArrayHighBound(src, 1) - VarArrayLowBound(src, 1) do begin
    v := src[i + VarArrayLowBound(src, 1)];
    Result := Exec.SetVariantValue(r, @v, DestType, lVarType);
    if not Result then
      Exit;
    r := Pointer(IPointer(r) + DestType.RealSize);
  end;
  Result := True;
end;

function CopyArrayContents(Dest, Src: Pointer; Len: Longint; aType: TPSTypeRec): Boolean;
var
  OK: Boolean;
  elSize: Cardinal;
  i: Longint;
begin
  try
    case aType.BaseType of
      btU8, btS8, btChar:
        for i := 0 to Len-1 do begin
          tbtU8(Dest^) := tbtU8(Src^);
          Dest := Pointer(IPointer(Dest) + 1);
          Src := Pointer(IPointer(Src) + 1);
        end;
      btU16, btS16 {$IFNDEF PS_NOWIDESTRING},btWideChar{$ENDIF}:
        for i := 0 to Len-1 do begin
          tbtU16(Dest^) := tbtU16(Src^);
          Dest := Pointer(IPointer(Dest) + 2);
          Src := Pointer(IPointer(Src) + 2);
        end;
      btProcPtr:
        for i := 0 to Len-1 do begin
          tbtU32(Dest^) := tbtU32(Src^);
          Dest := Pointer(IPointer(Dest) + PointerSize);
          Src := Pointer(IPointer(Src) + PointerSize);
          Pointer(Dest^) := Pointer(Src^);
          Dest := Pointer(IPointer(Dest) + PointerSize);
          Src := Pointer(IPointer(Src) + PointerSize);
          Pointer(Dest^) := Pointer(Src^);
          Dest := Pointer(IPointer(Dest) + PointerSize);
          Src := Pointer(IPointer(Src) + PointerSize);
        end;
      btClass, btPChar {+}{$IFNDEF PS_NOWIDESTRING}{$if declared(btPWideChar)},btPWideChar{$ifend}{$ENDIF}{+.}:
        for i := 0 to Len-1 do begin
          Pointer(Dest^) := Pointer(Src^);
          Dest := Pointer(IPointer(Dest) + PointerSize);
          Src := Pointer(IPointer(Src) + PointerSize);
        end;
      btU32, btS32, btSingle:
        for i := 0 to Len-1 do begin
          tbtU32(Dest^) := tbtU32(Src^);
          Dest := Pointer(IPointer(Dest) + 4);
          Src := Pointer(IPointer(Src) + 4);
        end;
      btDouble:
        for i := 0 to Len-1 do begin
          tbtDouble(Dest^) := tbtDouble(Src^);
          Dest := Pointer(IPointer(Dest) + 8);
          Src := Pointer(IPointer(Src) + 8);
        end;
      {$IFNDEF PS_NOINT64}btS64:
        for i := 0 to Len-1 do begin
          tbtS64(Dest^) := tbtS64(Src^);
          Dest := Pointer(IPointer(Dest) + 8);
          Src := Pointer(IPointer(Src) + 8);
        end;
      {$ENDIF}
      btExtended:
        for i := 0 to Len-1 do begin
          tbtExtended(Dest^) := tbtExtended(Src^);
          Dest := Pointer(IPointer(Dest) + SizeOf(Extended));
          Src := Pointer(IPointer(Src) + SizeOf(Extended));
        end;
      btCurrency:
        for i := 0 to Len-1 do begin
          tbtCurrency(Dest^) := tbtCurrency(Src^);
          Dest := Pointer(IPointer(Dest) + SizeOf(Currency));
          Src := Pointer(IPointer(Src) + SizeOf(Currency));
        end;
      btVariant:
        for i := 0 to Len-1 do begin
          Variant(Dest^) := Variant(Src^);
          Dest := Pointer(IPointer(Dest) + SizeOf(Variant));
          Src := Pointer(IPointer(Src) + SizeOf(Variant));
        end;
      btString:
        for i := 0 to Len-1 do begin
          TbtString(Dest^) := TbtString(Src^);
          Dest := Pointer(IPointer(Dest) + PointerSize);
          Src := Pointer(IPointer(Src) + PointerSize);
        end;
      {$IFNDEF PS_NOWIDESTRING}
      btUnicodeString:
        for i := 0 to Len-1 do begin
          TbtUnicodeString(Dest^) := TbtUnicodeString(Src^);
          Dest := Pointer(IPointer(Dest) + PointerSize);
          Src := Pointer(IPointer(Src) + PointerSize);
        end;
      btWideString:
        for i := 0 to Len-1 do begin
          tbtWideString(Dest^) := tbtWideString(Src^);
          Dest := Pointer(IPointer(Dest) + PointerSize);
          Src := Pointer(IPointer(Src) + PointerSize);
        end;
      {$ENDIF}
      btStaticArray:
        begin
          elSize := aType.RealSize;
          for i := 0 to Len-1 do begin
            OK := CopyArrayContents(Dest, Src,
              TPSTypeRec_StaticArray(aType).Size, TPSTypeRec_StaticArray(aType).ArrayType);
            if not OK then begin
              Result := False;
              Exit;
            end;
            Dest := Pointer(IPointer(Dest) + elSize);
            Src := Pointer(IPointer(Src) + elSize);
          end;
        end;
      btArray:
        begin
          for i := 0 to Len-1 do begin
            if Pointer(Dest^) <> nil then
              PSDynArraySetLength(Pointer(Dest^), aType, 0);
            Pointer(Dest^) := Pointer(Src^);
            if Pointer(Dest^) <> nil then begin
              {+}
              //Inc(PDynArrayRec(PAnsiChar(Dest^) - SizeOf(TDynArrayRecHeader))^.header.refCnt);
              Inc(PDynArrayRec(PointerShift(Pointer(Dest^), -SizeOf(TDynArrayRecHeader)))^.header.refCnt);
              {+.}
            end;
            Dest := Pointer(IPointer(Dest) + PointerSize);
            Src := Pointer(IPointer(Src) + PointerSize);
          end;
        end;
      btRecord:
        begin
          elSize := aType.RealSize;
          for i := 0 to Len-1 do begin
            OK := CopyRecordContents(Dest, Src, TPSTypeRec_Record(aType));
            if not OK then begin
              Result := False;
              Exit;
            end;
            Dest := Pointer(IPointer(Dest) + elSize);
            Src := Pointer(IPointer(Src) + elSize);
          end;
        end;
      btSet:
        begin
          elSize := aType.RealSize;
          for i := 0 to Len-1 do begin
            Move(Src^, Dest^, elSize);
            Dest := Pointer(IPointer(Dest) + elSize);
            Src := Pointer(IPointer(Src) + elSize);
          end;
        end;
      {$IFNDEF PS_NOINTERFACES}
      btInterface:
        begin
          for i := 0 to Len-1 do begin
            {$IFNDEF DELPHI3UP}
            if IUnknown(Dest^) <> nil then begin
              IUnknown(Dest^).Release;
              IUnknown(Dest^) := nil;
            end;
            {$ENDIF !DELPHI3UP}
            IUnknown(Dest^) := IUnknown(Src^);
            {$IFNDEF DELPHI3UP}
            if IUnknown(Dest^) <> nil then
              IUnknown(Dest^).AddRef;
            {$ENDIF !DELPHI3UP}
            Dest := Pointer(IPointer(Dest) + PointerSize);
            Src := Pointer(IPointer(Src) + PointerSize);
          end;
        end;
      {$ENDIF !PS_NOINTERFACES}
      btPointer:
        begin
          OK := (Pointer(Pointer(IPointer(Dest)+PointerSize2)^) = nil)
            and (Pointer(Pointer(IPointer(Src)+PointerSize2)^) = nil);
          if OK then begin
            for i := 0 to Len-1 do begin
              Pointer(Dest^) := Pointer(Src^);
              Dest := Pointer(IPointer(Dest) + PointerSize);
              Src := Pointer(IPointer(Src) + PointerSize);
              Pointer(Dest^) := Pointer(Src^);
              Dest := Pointer(IPointer(Dest) + PointerSize);
              Src := Pointer(IPointer(Src) + PointerSize);
              Pointer(Dest^) := nil;
              Dest := Pointer(IPointer(Dest) + PointerSize);
              Src := Pointer(IPointer(Src) + PointerSize);
            end;
          end else begin
            for i := 0 to Len-1 do begin
              OK := Pointer(Pointer(IPointer(Dest)+PointerSize2)^) <> nil;
              if OK then
                DestroyHeapVariant2(Pointer(Dest^), Pointer(Pointer(IPointer(Dest)+PointerSize)^));
              if Pointer(Src^) <> nil then begin
                OK := LongBool(Pointer(IPointer(Src) + PointerSize2)^);
                if not OK then begin
                  Pointer(Dest^) := Pointer(Src^);
                  Pointer(Pointer(IPointer(Dest) + PointerSize)^) := Pointer(Pointer(IPointer(Src) + PointerSize)^);
                  Pointer(Pointer(IPointer(Dest) + PointerSize2)^) := Pointer(Pointer(IPointer(Src) + PointerSize2)^);
                end else begin
                  Pointer(Dest^) := CreateHeapVariant2(Pointer(Pointer(IPointer(Src) + PointerSize)^));
                  Pointer(Pointer(IPointer(Dest) + PointerSize)^) := Pointer(Pointer(IPointer(Src) + PointerSize)^);
                  LongBool(Pointer(IPointer(Dest) + PointerSize2)^) := True;
                  OK := CopyArrayContents(Pointer(Dest^), Pointer(Src^), 1,
                    Pointer(Pointer(IPointer(Dest) + PointerSize)^));
                  if not OK then begin
                    Result := False;
                    Exit;
                  end;
                end;
              end else begin
                Pointer(Dest^) := nil;
                Pointer(Pointer(IPointer(Dest) + PointerSize)^) := nil;
                Pointer(Pointer(IPointer(Dest) + PointerSize2)^) := nil;
              end;
              Dest := Pointer(IPointer(Dest) + PointerSize*3);
              Src := Pointer(IPointer(Src) + PointerSize*3);
            end; // for i
          end;
        end;
        //btResourcePointer = 15;
        //btVariant = 16;
      else begin
        Result := False;
        Exit;
      end;
    end; // case aType.BaseType
  except
    Result := False;
    Exit;
  end;
  Result := True;
end; // function CopyArrayContents

function  GetPSArrayLength(Arr: PIFVariant): Longint;
begin
  Result := PSDynArrayGetLength(PPSVariantDynamicArray(arr).Data, arr.FType);
end;

procedure SetPSArrayLength(Arr: PIFVariant; NewLength: Longint);
begin
  PSDynArraySetLength(PPSVariantDynamicArray(arr).Data, arr.FType, NewLength);
end;

function PSDynArrayGetLength(arr: Pointer; aType: TPSTypeRec): Longint;
begin
  if aType.BaseType <> btArray then
    raise Exception.Create(RPS_InvalidArray);
  if arr = nil then Result := 0 else begin
    {+}
    //Result := PDynArrayRec(PAnsiChar(arr)  -SizeOf(TDynArrayRecHeader))^.header. {$IFDEF FPC}high+1{$ELSE}length{$ENDIF FPC};
    Result := PDynArrayRec(PointerShift(arr, -SizeOf(TDynArrayRecHeader)))^.header.{$IFDEF FPC}high+1{$ELSE}length{$ENDIF FPC};
    {+.}
  end;
end;

procedure PSDynArraySetLength(var arr: Pointer; aType: TPSTypeRec; NewLength: Longint);
var
  elSize, i, OldLen: Longint;
  darr : PDynArrayRec;
begin
  if aType.BaseType <> btArray then
    raise Exception.Create(RPS_InvalidArray);
  OldLen := PSDynArrayGetLength(arr, aType);
  elSize := TPSTypeRec_Array(aType).ArrayType.RealSize;
  if NewLength<0 then
     NewLength:=0;
  if (OldLen = 0) and (NewLength = 0) then
    Exit; // already are both 0
  if (OldLen = NewLength) then
    Exit; // already same size, noop
  {+}
  //darr := PDynArrayRec(PAnsiChar(Arr) - SizeOf(TDynArrayRecHeader));
  darr := PDynArrayRec(PointerShift(arr, -SizeOf(TDynArrayRecHeader)));
  {+.}
  if (OldLen <> 0) and (darr^.header.refCnt = 1) then begin // unique copy of this dynamic array
    for i := NewLength to OldLen-1 do begin
      if TPSTypeRec_Array(aType).ArrayType.BaseType in NeedFinalization then begin
        FinalizeVariant(Pointer(IPointer(arr) + Cardinal(elsize * i)), TPSTypeRec_Array(aType).ArrayType);
      end;
    end;
    if NewLength <= 0 then begin
      FreeMem(darr);
      arr := nil;
      Exit;
    end;
    ReallocMem(darr, Longint(NewLength * elSize) + SizeOf(TDynArrayRecHeader));
    {$IFDEF FPC}
    darr^.header.high := NewLength-1;
    {$ELSE}
    darr^.header.length := NewLength;
    {$ENDIF}
    arr := @darr^.datas;
    for i := OldLen to NewLength-1 do begin
      InitializeVariant(Pointer(IPointer(arr) + Cardinal(elsize * i)), TPSTypeRec_Array(aType).ArrayType);
    end;
  end else begin
    if NewLength = 0 then begin
      FinalizeVariant(@arr, aType);
      arr := nil;
      Exit;
    end;
    GetMem(darr, Longint(NewLength * elSize) + SizeOf(TDynArrayRecHeader));
    darr^.header.refCnt := 1;
    {$IFDEF FPC}
    darr^.header.high := NewLength-1;
    {$ELSE}
    {$IFDEF CPUX64}
    darr^.header._Padding := 0;
    {$ENDIF CPUX64}
    darr^.header.length := NewLength;
    {$ENDIF FPC}
    for i := 0 to NewLength-1 do begin
      InitializeVariant(Pointer(IPointer(@darr^.datas) + Cardinal(elsize * i)), TPSTypeRec_Array(aType).ArrayType);
    end;
    if OldLen <> 0 then begin
      if OldLen > NewLength then
        CopyArrayContents(@darr^.datas, arr, NewLength, TPSTypeRec_Array(aType).ArrayType)
      else
        CopyArrayContents(@darr^.datas, arr, OldLen, TPSTypeRec_Array(aType).ArrayType);
      FinalizeVariant(@arr, aType);
    end;
    arr := @darr^.datas;
  end;
end; // procedure PSDynArraySetLength

function TPSExec.SetVariantValue(Dest, Src: Pointer; destType, srcType: TPSTypeRec{+}; sd: PPSResultData{+.}): Boolean;
var
  Tmp: TObject;
  tt: TPSVariantPointer;
  {+}
  S: TbtString;
  n, elSize, len: integer;
  OK: Boolean;
  src_ArrayType, dest_ArrayType: TPSTypeRec;
  {+.} // IsStrType(srctype.BaseType) and ((desttype.FBaseType in [btArray, btStaticArray]) and IsIntType(TPSTypeRec_Array(desttype).FArrayType.FBaseType))
begin // {+}{@dbg@:hook.variant.set}{+.} // dbg.cond: srctype.BaseType = btUnicodeString; IsStrType(srctype.BaseType)
  Result := True;
  try
    case destType.BaseType of
      btSet: begin
        if destType = srcType then
          Move(Src^, Dest^, TPSTypeRec_Set(destType).aByteSize)
        else
          Result := False;
      end;
      btU8: tbtU8(Dest^) := PSGetUInt(Src, srctype);
      btS8: tbtS8(Dest^) := PSGetInt(Src, srctype);
      btU16: tbtU16(Dest^) := PSGetUInt(Src, srctype);
      btS16: tbtS16(Dest^) := PSGetInt(Src, srctype);
      btProcPtr: begin
        if srcType.BaseType = btPointer then begin
          srcType := PIFTypeRec(Pointer(IPointer(Src)+PointerSize)^);
          Src := Pointer(Src^);
          if (src = nil) or (srcType = nil) then
            raise Exception.Create(RPS_TypeMismatch);
        end;
        case srcType.BaseType of
          btu32{+},bts32{+.}: begin
            Pointer(Dest^) := Pointer(Src^); // TODO: ?: Pointer(Dest^) := Pointer(btu32(Src^));
          end;
          {+}
          {$IFDEF CPU64}
          bts64: begin
            Pointer(Dest^) := Pointer(Src^); // TODO: ?: Pointer(Dest^) := Pointer(bts64(Src^));
          end;
          {$ENDIF CPU32}
          btPointer: begin
            Pointer(Dest^) := Pointer(Src^);
          end;
          {+.}
          btProcPtr: begin
            Pointer(Dest^) := Pointer(Src^);
            Pointer(Pointer(IPointer(Dest)+PointerSize)^) := Pointer(Pointer(IPointer(Src)+PointerSize)^);
            Pointer(Pointer(IPointer(Dest)+PointerSize2)^) := Pointer(Pointer(IPointer(Src)+PointerSize2)^);
          end;
          else
            raise Exception.Create(RPS_TypeMismatch);
        end; // case
      end; // btProcPtr
      btU32: begin
        if srcType.BaseType = btPointer then begin
          srcType := PIFTypeRec(Pointer(IPointer(Src)+PointerSize)^);
          Src := Pointer(Src^);
          if (src = nil) or (srctype = nil) then
            raise Exception.Create(RPS_TypeMismatch);
        end;
        case srctype.BaseType of
          btU8: tbtU32(Dest^) := tbtU8(src^);
          btS8: tbtU32(Dest^) := tbtS8(src^);
          btU16: tbtU32(Dest^) := tbtU16(src^);
          btS16: tbtU32(Dest^) := tbtS16(src^);
          btU32: tbtU32(Dest^) := tbtU32(src^);
          btS32: tbtU32(Dest^) := tbtS32(src^);
          {$IFNDEF PS_NOINT64}
          btS64: tbtU32(Dest^) := TbtS64(src^);
          {$ENDIF !PS_NOINT64}
          btChar: tbtU32(Dest^) := Ord(TbtChar(Src^));
          {$IFNDEF PS_NOWIDESTRING}
          btWideChar: tbtu32(Dest^) := Ord(TbtWideChar(Src^));
          {$ENDIF !PS_NOWIDESTRING}
          btVariant: tbtU32(Dest^) := Variant(src^);
          {+}
          {$IFDEF CPU32} {+}{@dbg@:hook.variant.set}{+.} // dbg.cond:
          {?}btPointer, btPChar, btClass, btInterface
          {$IFNDEF PS_NOWIDESTRING}{$if declared(btPWideChar)},btPWideChar{$ifend}{$ENDIF}:
            tbtU32(Dest^) := tbtU32(src^);
          {$ENDIF CPU32}
          {+.}
          else
            raise Exception.Create(RPS_TypeMismatch);
        end; // case
      end; // btU32
      btS32: begin
        if srctype.BaseType = btPointer then begin
          srcType := PIFTypeRec(Pointer(IPointer(Src)+PointerSize)^);
          Src := Pointer(Src^);
          if (src = nil) or (srctype = nil) then
            raise Exception.Create(RPS_TypeMismatch);
        end;
        case srcType.BaseType of
          btU8: tbtS32(Dest^) := tbtU8(src^);
          btS8: tbtS32(Dest^) := tbtS8(src^);
          btU16: tbtS32(Dest^) := tbtU16(src^);
          btS16: tbtS32(Dest^) := tbtS16(src^);
          btU32: tbtS32(Dest^) := tbtU32(src^);
          btS32: tbtS32(Dest^) := tbtS32(src^);
          {$IFNDEF PS_NOINT64}
          btS64: tbtS32(Dest^) := tbtS64(src^);
          {$ENDIF}
          btChar: tbtS32(Dest^) := Ord(tbtchar(Src^));
          {$IFNDEF PS_NOWIDESTRING}
          btWideChar: tbtS32(Dest^) := Ord(TbtWideChar(Src^));
          {$ENDIF}
          btVariant: tbtS32(Dest^) := Variant(src^);
          // nx change start - allow assignment of class
          {+}
          //btClass: tbtU32(Dest^) := tbtu32(Src^);
          {$IFDEF CPU32}
          {?}btPointer, btPChar, btClass, btInterface
          {$IFNDEF PS_NOWIDESTRING}
            {$if declared(btPWideChar)}
          ,btPWideChar
            {$ifend}
          {$ENDIF}:
            tbtU32(Dest^) := tbtU32(Src^);
          {$ENDIF}
          {+.}
          // nx change start
          else
            raise Exception.Create(RPS_TypeMismatch);
        end;
      end; // btS32
      {$IFNDEF PS_NOINT64}
      btS64:
        TbtS64(Dest^) := PSGetInt64(Src, srcType);
      {$ENDIF}
      btSingle: begin
        if srcType.BaseType = btPointer then begin
          srcType := PIFTypeRec(Pointer(IPointer(Src)+PointerSize)^);
          Src := Pointer(Src^);
          if (src = nil) or (srctype = nil) then
            raise Exception.Create(RPS_TypeMismatch);
        end;
        case srctype.BaseType of
          btU8: tbtsingle(Dest^) := tbtu8(src^);
          btS8: tbtsingle(Dest^) := tbts8(src^);
          btU16: tbtsingle(Dest^) := tbtu16(src^);
          btS16: tbtsingle(Dest^) := tbts16(src^);
          btU32: tbtsingle(Dest^) := tbtu32(src^);
          btS32: tbtsingle(Dest^) := tbts32(src^);
          {$IFNDEF PS_NOINT64}
          btS64: tbtsingle(Dest^) := tbts64(src^);
          {$ENDIF}
          btSingle: tbtsingle(Dest^) := tbtsingle(Src^);
          btDouble: tbtsingle(Dest^) := tbtdouble(Src^);
          btExtended: tbtsingle(Dest^) := tbtextended(Src^);
          btCurrency: tbtsingle(Dest^) := tbtcurrency(Src^);
          btVariant:  tbtsingle(Dest^) := Variant(src^);
          else
            raise Exception.Create(RPS_TypeMismatch);
        end; // case
      end; // btSingle
      btDouble: begin
        if srcType.BaseType = btPointer then begin
          srcType := PIFTypeRec(Pointer(IPointer(Src)+PointerSize)^);
          Src := Pointer(Src^);
          if (src = nil) or (srcType = nil) then
            raise Exception.Create(RPS_TypeMismatch);
        end;
        case srcType.BaseType of
          btU8: TbtDouble(Dest^) := TbtU8(Src^);
          btS8: TbtDouble(Dest^) := TbtS8(Src^);
          btU16: TbtDouble(Dest^) := TbtU16(Src^);
          btS16: TbtDouble(Dest^) := TbtS16(Src^);
          btU32: TbtDouble(Dest^) := TbtU32(Src^);
          btS32: TbtDouble(Dest^) := TbtS32(Src^);
          {$IFNDEF PS_NOINT64}
          btS64: TbtDouble(Dest^) := TbtS64(Src^);
          {$ENDIF}
          btSingle: TbtDouble(Dest^) := TbtSingle(Src^);
          btDouble: TbtDouble(Dest^) := TbtDouble(Src^);
          btExtended: TbtDouble(Dest^) := TbtExtended(Src^);
          btCurrency: TbtDouble(Dest^) := TbtCurrency(Src^);
          btVariant:  TbtDouble(Dest^) := Variant(Src^);
          {+}
          {$IFDEF CPU32}
          {?}btPointer, btPChar, btClass, btInterface
          {$IFNDEF PS_NOWIDESTRING}
            {$if declared(btPWideChar)}
          ,btPWideChar
            {$ifend}
          {$ENDIF}:
            TbtDouble(Dest^) := TbtU32(Src^);
          {$ENDIF}
          {$IFDEF CPU64}
          {?}btPointer, btPChar, btClass, btInterface
          {$IFNDEF PS_NOWIDESTRING}
            {$if declared(btPWideChar)}
          ,btPWideChar
            {$ifend}
          {$ENDIF}:
            TbtDouble(Dest^) := NativeUInt(TbtS64(Src^));
          {$ENDIF CPU64}
          {+.}
          else
            raise Exception.Create(RPS_TypeMismatch);
        end; // case
      end; // btDouble
      btExtended: begin
        if srcType.BaseType = btPointer then begin
          srcType := PIFTypeRec(Pointer(IPointer(Src)+PointerSize)^);
          Src := Pointer(Src^);
          if (src = nil) or (srcType = nil) then
            raise Exception.Create(RPS_TypeMismatch);
        end;
        case srctype.BaseType of
          btU8: tbtExtended(Dest^) := tbtU8(src^);
          btS8: tbtExtended(Dest^) := tbtS8(src^);
          btU16: tbtExtended(Dest^) := tbtU16(src^);
          btS16: tbtExtended(Dest^) := tbtS16(src^);
          btU32: tbtExtended(Dest^) := tbtU32(src^);
          btS32: tbtExtended(Dest^) := tbtS32(src^);
          {$IFNDEF PS_NOINT64}
          btS64: tbtExtended(Dest^) := tbtS64(src^);
          {$ENDIF}
          btSingle: tbtExtended(Dest^) := tbtSingle(Src^);
          btDouble: tbtExtended(Dest^) := tbtDouble(Src^);
          btExtended: tbtExtended(Dest^) := tbtExtended(Src^);
          btCurrency: tbtExtended(Dest^) := tbtcurrency(Src^);
          btVariant:  tbtExtended(Dest^) := Variant(Src^);
          {+}
          {$IFDEF CPU32}
          {?}btPointer, btPChar, btClass, btInterface
          {$IFNDEF PS_NOWIDESTRING}
            {$if declared(btPWideChar)}
          ,btPWideChar
            {$ifend}
          {$ENDIF}:
            TbtExtended(Dest^) := TbtU32(Src^);
          {$ENDIF CPU32}
          {$IFDEF CPU64}
          {?}btPointer, btPChar, btClass, btInterface
          {$IFNDEF PS_NOWIDESTRING}
            {$if declared(btPWideChar)}
          ,btPWideChar
            {$ifend}
          {$ENDIF}:
            tbtextended(Dest^) := NativeUInt(TbtS64(Src^));
          {$ENDIF CPU64}
          {+.}
          else
            raise Exception.Create(RPS_TypeMismatch);
        end; // case
      end; // btExtended
      btCurrency:
        TbtCurrency(Dest^) := PSGetCurrency(Src, srcType);
      {+}
      btPChar: begin
        //{$if btCharIsWide}
        //destType.Clear;
        //PWideChar(Dest^) := PWideChar(PSGetWideString(Src, srcType));
        //{$else}
        TPSTypeRec_PAnsiChar(destType).fBuffer := PSGetAnsiString(Src, srcType); // lock pansichar by reference to ansistring
        PTbtChar(dest^) := PTbtChar(TPSTypeRec_PAnsiChar(destType).fBuffer);
        (*
        if (sd=nil) or (srctype.BaseType <> btPChar) or (dest <> Src) then
        begin
          desttype.Clear;
          if (srctype.BaseType <> btPChar) or (TPSTypeRec_PChar(srctype).FTypeCharsRec = nil) then
            pansichar(dest^) := pansichar(PSGetAnsiString(Src, srctype))
          else
          begin
            //dest := Src;
            sd.P := Src;
            with TPSTypeRec_PChar(desttype) do
            begin
              FTypeCharsRec := TPSTypeRec_PChar(srctype).FTypeCharsRec;
              //FTempBaseType := TPSTypeRec_PChar(srctype).FTempBaseType;
              FTempData := TPSTypeRec_PChar(srctype).FTempData;
              //if Assigned(FTypeCharsRec) then
              Inc(FTypeCharsRec.FPChasrRefCount);
            end;
          end;
        end;
        //*)
        //{$ifend}
      end;
      {+.}
      btString:
        TbtString(Dest^) := PSGetAnsiString(Src, srcType);
      btChar: TbtChar(Dest^) := PSGetAnsiChar(Src, srcType); // OLD: TbtChar(PSGetUInt(Src, srcType));
      {$IFNDEF PS_NOWIDESTRING}
      {+}
      {$if declared(btPWideChar)}
      btPWideChar: begin
        {
        destType.Clear;
        PWideChar(Dest^) := PWideChar(PSGetWideString(Src, srcType));
        }
        TPSTypeRec_PWideChar(destType).fBuffer := PSGetWideString(Src, srcType); // lock pwidechar by reference to widestring
        PWideChar(Dest^) := PWideChar(TPSTypeRec_PWideChar(destType).fBuffer);
      end;
      {$ifend}
      {+.}
      btWideString: TbtWideString(dest^) := PSGetWideString(Src, srcType);
      btUnicodeString: TbtUnicodeString(dest^) := PSGetUnicodeString(Src, srcType); // {+}{@dbg@:hook.setvar}{+.} // dbg.cond: srctype.FExportName='TBYTES' ; PSDynArrayGetLength(Pointer(Src^), srctype)=36
      {+}
      btWideChar: TbtWideChar(dest^) := PSGetWideChar(Src, srcType); // OLD: widechar(PSGetUInt(Src, srctype));
      {+.}
      {$ENDIF !PS_NOWIDESTRING}
      btStaticArray: begin
        {+} // TODO: autocopy StrType into Array Of IntType
         // ...
        {+.}
        if destType <> srcType then
          Result := False
        else
          CopyArrayContents(dest, Src,
            TPSTypeRec_StaticArray(destType).Size, TPSTypeRec_StaticArray(desTtype).ArrayType);
      end;
      btArray: begin
        {+} // autocopy StrType into Array Of IntType
        src_ArrayType := TPSTypeRec_Array(srctype).ArrayType;
        dest_ArrayType := TPSTypeRec_Array(desttype).ArrayType;
        OK := IsStrType(srctype.BaseType);
        if OK then
          OK := IsIntType(dest_ArrayType.FBaseType);
        if OK then begin
          len := 0;
          if src <> nil then
          case srctype.BaseType of
            btChar:
              len := 1;
            btPChar:
              {$if btCharIsWide}
              //len := uPSUtils.StrLenW(PWideChar(src^));
              len := StrLen(PWideChar(src^));
              {$else}
              len := uPSUtils.StrLenA(PAnsiChar(src^));
              {$ifend}
            btString:
              len := Length(TbtString(src^));
            {$IFNDEF PS_NOWIDESTRING}
            btWideChar:
              len := 2;
            {$if declared(btPWideChar)}
            btPWideChar:
              len := 2*{$IFDEF FPC}strlen{$ELSE}wstrlen{$ENDIF}((PWideChar(src^)));
            {$ifend}
            btWideString:
              len := 2*length(tbtWideString(src^));
            btUnicodeString:
              len := 2*length(TbtUnicodeString(src^));
            {$ENDIF !PS_NOWIDESTRING}
          end; // case
          elSize := TPSTypeRec_Array(desttype).ArrayType.RealSize;
          n := len div elSize;
          //PSDynArraySetLength(Pointer(Dest^), desttype, 0); // for relocation always
          PSDynArraySetLength(Pointer(Dest^), desttype, n); // dbg: PSDynArrayGetLength(Pointer(dest^), desttype)
          if n > 0 then // dbg: pwidechar(src^) ; pwidechar(dest^) ; pwidechar(@(PDynArrayRec(PointerShift(Pointer(dest^),-SizeOf(TDynArrayRecHeader))).datas))
            Move(PPointer(src)^^,
            Pointer(@(PDynArrayRec(PointerShift(Pointer(dest^),-SizeOf(TDynArrayRecHeader))).datas))^,
            n * elSize // ~len
          );
        end else if (srctype.BaseType = btStaticArray) and (dest_ArrayType = src_ArrayType) then begin
          PSDynArraySetLength(Pointer(Dest^), desttype, TPSTypeRec_StaticArray(srctype).Size);
          CopyArrayContents(Pointer(dest^), Src, TPSTypeRec_StaticArray(srctype).Size, TPSTypeRec_StaticArray(srctype).ArrayType);
        end else if (srctype.BaseType = btVariant) and VarIsArray(Variant(src^)) then begin
          Result := CreateArrayFromVariant(Self, dest, Variant(src^), destType)
        end else begin
          //OLD:
          {if (destType <> srcType) and not ((destType.BaseType = btArray) and (srcType.BaseType = btArray)
            and (dest_ArrayType = src_ArrayType)) then
            Result := False
          else
            CopyArrayContents(dest, src, 1, destType);}
          //NEW:
          //src_ArrayType.FBaseType
          OK := (destType = srcType);
          if OK then begin
            OK := (destType.BaseType = btArray) and (srcType.BaseType = btArray);
            if OK then begin
              OK := (dest_ArrayType = src_ArrayType)
                or (
                  (src_ArrayType.FBaseType = btClass) and
                  (dest_ArrayType.FBaseType = src_ArrayType.FBaseType)
                )
              ;
            end;
          end else begin
            OK := (destType.BaseType = btArray) and (srcType.BaseType = btArray);
            if OK then begin
              OK := (dest_ArrayType = src_ArrayType)
                or (
                  (src_ArrayType.FBaseType = btClass) // TODO: check other types
                  and (dest_ArrayType.FBaseType = src_ArrayType.FBaseType)
                )
              ;
            end;
          end;
          if OK then
            Result := CopyArrayContents(dest, src, {Len:}1, desttype)
          else
            Result := False; // Error: TypeMismatch
        end;
        {+.}
      end; // btArray
      btRecord: begin
        if destType <> srcType then
          Result := False
        else
          CopyArrayContents(dest, Src, 1, destType);
      end;
      btVariant: begin
        {$IFNDEF PS_NOINTERFACES}
        if srcType.ExportName = 'IDISPATCH' then begin
          {$IFDEF DELPHI3UP}
          Variant(Dest^) := IDispatch(Src^); {+}{@dbg@:hook.IDispatch}{+.} // dbg.cond:
          {$ELSE}
          AssignVariantFromIDispatch(Variant(Dest^), IDispatch(Src^));
          {$ENDIF !DELPHI3UP}
        end else
        {$ENDIF !PS_NOINTERFACES}
        if srcType.BaseType = btVariant then
          Variant(Dest^) := Variant(src^)
        else begin
          tt.VI.FType := FindType2(btPointer);
          tt.DestType := srcType;
          tt.DataDest := src;
          tt.FreeIt := False;
          Result := PIFVariantToVariant(@tt, Variant(dest^));
        end;
      end; // btVariant
      btClass: begin
        if srcType.BaseType = btClass then
          TObject(Dest^) := TObject(Src^) // @dbg: TObject(Src^).ClassName  ;  PPSVarRec(src)^.vClass.ClassName
        else if srcType.BaseType = btVariant then begin
          {+}
          //TbtU32(Dest^) := Variant(Src^) // {+} TODO: CPU64 test needed {+.}
          Pointer(Dest^) := Pointer({$IFDEF FPC}UIntPtr{$ELSE}NativeUInt{$ENDIF}(Variant(Src^)))
          {+.}
        end
        // nx change start
        {+}
        {$IFDEF CPU32}
        else if (srcType.BaseType in [btS32, btU32, btPointer, btPChar, btClass, btInterface
          {$IFNDEF PS_NOWIDESTRING}{$if declared(btPWideChar)},btPWideChar{$ifend}{$ENDIF}])
        then begin
          Pointer(Dest^) := Pointer(TbtU32(Src^));
        end
        {$ENDIF CPU32}
        {$IFDEF CPU64}
        else if (srctype.BaseType in [btS64, btPointer, btPChar, btClass, btInterface
          {$IFNDEF PS_NOWIDESTRING}{$if declared(btPWideChar)},btPWideChar{$ifend}{$ENDIF}])
        then begin
          Pointer(Dest^) := Pointer(tbts64(Src^));
        end
        {$ENDIF CPU64}
        {+.}
        else
        // nx change end
          Result := False;
      end; // btClass
      {$IFNDEF PS_NOINTERFACES}
      btInterface: begin
        if Srctype.BaseType = btVariant then begin
          if destType.ExportName = 'IDISPATCH' then begin
            {$IFDEF Delphi3UP}
            IDispatch(Dest^) := IDispatch(Variant(Src^));
            {$ELSE}
            AssignIDispatchFromVariant(IDispatch(Dest^), Variant(Src^));
            {$ENDIF}
          end else
          {+}   // debug: dest:   GUIDToString(TPSTypeRec_Interface(desttype).Guid)
          begin // debug: source: GUIDToString(TPSTypeRec_Interface(srctype).Guid)
            //Result := False;
            OK := AssignIInterfaceFromVariant(IUnknown(Dest^), Variant(Src^), TPSTypeRec_Interface(desttype).Guid,
              {AllowEmptyAssign:}True);
            if not OK then begin
              //CMD_Err2(erInterfaceNotSupported, TbtString(RPS_InterfaceNotSupported));
              S := TbtString(RPS_CannotCastInterface);
              if destType.FExportName <> '' then
                S := S + TbtString(' ') + destType.FExportName
              else //if TPSTypeRec_Interface(desttype).Guid <> GUID_NULL then
                S := S + TbtString(' ') + TbtString(GUIDToString(TPSTypeRec_Interface(desttype).Guid));
              //
              raise EPSIntf.Create(S);
              //
              {CMD_Err2(erCustomError, S);
              Result := False;
              Exit;//}
            end;
          end;
          {+.}
        {$IFDEF Delphi3UP} // or FPC
        end else if srcType.BaseType = btClass then begin
          OK := (TObject(Src^) = nil)
            or (not TObject(Src^).GetInterface(TPSTypeRec_Interface(desTtype).Guid, IUnknown(Dest^)));
          if OK then
          begin
            {+}
            //Result := False;
            //CMD_Err2(erInterfaceNotSupported, TbtString(RPS_InterfaceNotSupported));
            S := TbtString(RPS_InterfaceNotSupported);
            if desttype.FExportName <> '' then
              S := S + TbtString(' ') + desttype.FExportName
            else
              S := S + TbtString(' ') + TbtString(GUIDToString(TPSTypeRec_Interface(desttype).Guid));
            //
            raise EPSIntf.Create(S);
            //
            {CMD_Err2(erCustomError, S);
            Result := False;//}
            {+.}
            Exit;
          end;
        {$ENDIF Delphi3UP}
        end else if srcType.BaseType = btInterface then begin
          {$IFNDEF Delphi3UP}
          if IUnknown(Dest^) <> nil then begin
            IUnknown(Dest^).Release;
            IUnknown(Dest^) := nil;
          end;
          {$ENDIF !Delphi3UP}
          {+}
          //IUnknown(Dest^) := IUnknown(Src^);
          //{$IFNDEF Delphi3UP}
          //if IUnknown(Dest^) <> nil then
          //  IUnknown(Dest^).AddRef;
          //{$ENDIF}
          OK := Pointer(Src^) = nil;
          if OK then begin
            {S := TbtString(RPS_NullPointerException) + TbtString(': ') + TbtString(RPS_CannotCastInterface);
            if destType.FExportName <> '' then
              S := S + TbtString(' ') + destType.FExportName
            else //if TPSTypeRec_Interface(destType).Guid <> GUID_NULL then
              S := S + TbtString(' ') + TbtString(GUIDToString(TPSTypeRec_Interface(destType).Guid));
            //
            raise EPSIntf.Create(S);//}
          end else if IsEqualGUID(TPSTypeRec_Interface(srctype).Guid, TPSTypeRec_Interface(desttype).Guid) then begin
            IUnknown(Dest^) := IUnknown(Src^);
            {$IFNDEF Delphi3UP}
            if Pointer(Dest^) <> nil then begin
              if IUnknown(Dest^) <> nil then
                IUnknown(Dest^).AddRef;
            end;
            {$ENDIF !Delphi3UP}
          end else if not AssignIntfFromIntf(IUnknown(Dest^), IUnknown(Src^), TPSTypeRec_Interface(desttype).Guid,
            {AllowNilAssign:}False) then
          begin
            //CMD_Err2(erInterfaceNotSupported, TbtString(RPS_InterfaceNotSupported));
            S := TbtString(RPS_CannotCastInterface);
            if destType.FExportName <> '' then
              S := S + TbtString(' ') + destType.FExportName
            else //if TPSTypeRec_Interface(destType).Guid <> GUID_NULL then
              S := S + TbtString(' ') + TbtString(GUIDToString(TPSTypeRec_Interface(destType).Guid));
            //
            raise EPSIntf.Create(S);
            //
            {CMD_Err2(erCustomError, S);
            Result := False;
            Exit;//}
          end;
          {+.}
        end else
          Result := False;
      end; // btInterface
      {$ENDIF !PS_NOINTERFACES}
      else begin
        Result := False;
      end;
    end; // case destType.BaseType
    if Result = False then
      CMD_Err2(erTypeMismatch, TbtString(RPS_TypeMismatch)); {+}{@dbg@:hook.variant.set}{+.} // dbg.cond:
  except
    {$IFDEF DELPHI6UP}
    Tmp := AcquireExceptionObject;
    {$ELSE !DELPHI6UP}
    if RaiseList <> nil then begin
      Tmp := Exception(PRaiseFrame(RaiseList)^.ExceptObject);
      PRaiseFrame(RaiseList)^.ExceptObject := nil;
    end else
      Tmp := nil;
    {$ENDIF !DELPHI6UP}
    if Tmp <> nil then begin
      if Tmp is EPSException then begin
        Result := False;
        ExceptionProc(EPSException(tmp).ProcNo, EPSException(tmp).ProcPos, erCustomError,
          TbtString(EPSException(tmp).Message), nil);
        Exit;
      end else if Tmp is EDivByZero then begin
        Result := False;
        CMD_Err3(erDivideByZero, TbtString(Exception(Tmp).Message), Tmp);
        Exit;
      end;
      if Tmp is EZeroDivide then begin
        Result := False;
        CMD_Err3(erDivideByZero, TbtString(Exception(Tmp).Message), Tmp);
        Exit;
      end;
      if Tmp is EMathError then begin
        Result := False;
        CMD_Err3(erMathError, TbtString(Exception(Tmp).Message), Tmp);
        Exit;
      end;
    end;
    if (tmp <> nil) and (Tmp is {+}ExceptionBase{+.}) then
      CMD_Err3(erException, TbtString(Exception(Tmp).Message), Tmp)
    else
      CMD_Err3(erException, '', Tmp);
    Result := False;
  end;
end; // function TPSExec.SetVariantValue

function SpecImport(Sender: TPSExec; p: TPSExternalProcRec; Tag: Pointer): Boolean; forward;

function Class_IS(Self: TPSExec; Obj: TObject; var2type: TPSTypeRec): Boolean;
var
  R: TPSRuntimeClassImporter;
  cc: TPSRuntimeClass;
begin
  if {+}(Obj = nil) or (Obj.ClassType = nil){+.} then begin
    Result := False;
    Exit;
  end;
  r := Self.FindSpecialProcImport(SpecImport);
  if R = nil then begin
    Result := False;
    Exit;
  end;
  cc := r.FindClass(var2type.ExportName);
  if cc = nil then begin
    Result := False;
    Exit;
  end;
  try
    Result := Obj is cc.FClass;
  except
    Result := False;
  end;
end;

type
  TVariantArray = array of Variant;
  PVariantArray = ^TVariantArray;
function VariantInArray(var1: Pointer; var1Type: TPSTypeRec; var2: PVariantArray): Boolean;
var
  lDest: Variant;
  i: Integer;
begin
  IntPIFVariantToVariant(var1, var1Type, {%H-}lDest);
  for i := 0 to Length(var2^)-1 do begin
    Result := var2^[i] = lDest;
    if Result then
      Exit;
  end;
  Result := False;
end;

function TPSExec.DoBooleanCalc(var1, Var2, into: Pointer; var1Type, var2type, intotype: TPSTypeRec; Cmd: Cardinal): Boolean;
var
  b: Boolean;
  Tmp: TObject;
  tvar: Variant;

  procedure SetBoolean(b: Boolean; var Ok: Boolean);
  begin
    Ok := True;
    case IntoType.BaseType of
      btU8: tbtu8(Into^):= Cardinal(b);
      btS8: tbts8(Into^) := Longint(b);
      btU16: tbtu16(Into^) := Cardinal(b);
      btS16: tbts16(Into^) := Longint(b);
      btU32: tbtu32(Into^) := Cardinal(b);
      btS32: tbts32(Into^) := Longint(b);
      btVariant: Variant(Into^) := b;
      else begin
        CMD_Err2(erTypeMismatch, TbtString(RPS_TypeMismatch));
        Ok := False;
      end;
    end;
  end;

begin
  Result := True;
  try
    case Cmd of
      0: begin { >= }
          case var1Type.BaseType of
            btU8:
            if (var2Type.BaseType = btString) or (Var2Type.BaseType = btPChar) then
              b := tbtchar(tbtu8(var1^)) >= PSGetAnsiString(Var2, var2type)
            {+}
            {$IFNDEF PS_NOWIDESTRING}
            else if (var2Type.BaseType = btWideString)
              {$if declared(btPWideChar)} or (Var2Type.BaseType = btPWideChar){$ifend} then
              b := TbtWideChar(tbtu16(var1^)) >= PSGetWideString(Var2, var2type)
            {$ENDIF !PS_NOWIDESTRING}
            {+.}
            else
              b := tbtu8(var1^) >= PSGetUInt(Var2, var2type);
            btS8: b := tbts8(var1^) >= PSGetInt(Var2, var2type);
            btU16: b := tbtu16(var1^) >= PSGetUInt(Var2, var2type);
            btS16: b := tbts16(var1^) >= PSGetInt(Var2, var2type);
            btU32: b := tbtu32(var1^) >= PSGetUInt(Var2, var2type);
            btS32:
              begin
                if var2type.BaseType = btPointer then
                begin
                  var2type := PIFTypeRec(Pointer(IPointer(var2)+PointerSize)^);
                  var2 := Pointer(var2^);
                  if (var2 = nil) or (var2type = nil) then
                    raise Exception.Create(RPS_TypeMismatch);
                end;
                case var2type.BaseType of
                  btU8: b := tbts32(var1^) >= tbtu8(Var2^);
                  btS8: b := tbts32(var1^) >= tbts8(Var2^);
                  btU16: b := tbts32(var1^) >= tbtu16(Var2^);
                  btS16: b := tbts32(var1^) >= tbts16(Var2^);
                  btU32: b := tbts32(var1^) >= Longint(tbtu32(Var2^));
                  btS32: b := tbts32(var1^) >= tbts32(Var2^);
                  btDouble: b := PSGetReal(Var1, var1type) >= tbtdouble(var2^);
                  btSingle: B := psGetReal(Var1, var1Type) >= tbtsingle(var2^);
                  btExtended: B := psGetReal(Var1, var1Type) >= tbtExtended(var2^);
                  {$IFNDEF PS_NOINT64}
                  btS64: b := tbts32(var1^) >= tbts64(Var2^);
                  {$ENDIF !PS_NOINT64}
                  btChar: b := tbts32(var1^) >= Ord(tbtchar(Var2^));
                  {$IFNDEF PS_NOWIDESTRING}
                  btWideChar: b := tbts32(var1^) >= Ord(TbtWideChar(Var2^));
                  {$ENDIF !PS_NOWIDESTRING}
                  btVariant: b := tbts32(var1^) >= Variant(Var2^);
                  else raise Exception.Create(RPS_TypeMismatch);
                end;
              end;
            btSingle: b := tbtsingle(var1^) >= PSGetReal(Var2, var2type);
            btDouble: b := tbtdouble(var1^) >= PSGetReal(Var2, var2type);
            btCurrency: b := tbtcurrency(var1^) >= PSGetCurrency(Var2, var2type);
            btExtended: b := tbtextended(var1^) >= PSGetReal(Var2, var2type);
            {$IFNDEF PS_NOINT64}
            btS64: b := tbts64(var1^) >= PSGetInt64(Var2, var2type);
            {$ENDIF !PS_NOINT64}
            btPChar,btString: b := TbtString(var1^) >= PSGetAnsiString(Var2, var2type);
            btChar: b := tbtchar(var1^) >= PSGetAnsiString(Var2, var2type);
            {$IFNDEF PS_NOWIDESTRING}
            btWideChar: b := TbtWideChar(var1^) >= PSGetWideString(Var2, var2type);
            {+}{$if declared(btPWideChar)}btPWideChar,{$ifend}{+.}
            btWideString: b := tbtwidestring(Var1^) >= PSGetWideString(Var2, var2type);
            btUnicodeString: b := TbtUnicodeString(Var1^) >= PSGetUnicodeString(Var2, var2type);
            {$ENDIF !PS_NOWIDESTRING}
            btVariant:
              begin
                if not IntPIFVariantToVariant(var2, var2type, {%H-}tvar) then begin
                  Result := False;
                end else
                  b := Variant(Var1^) >= tvar;
              end;
            btSet:
              begin
                if var1Type = var2Type then
                begin
                  Set_Subset(var2, var1, TPSTypeRec_Set(var1Type).aByteSize, b);
                end else Result := False;
              end;
            else begin
              CMD_Err2(erTypeMismatch, TbtString(RPS_TypeMismatch));
              Exit;
            end;
          end;
          if not Result then begin
            CMD_Err2(erTypeMismatch, TbtString(RPS_TypeMismatch));
            Exit;
          end;
          SetBoolean(b, Result);
        end;
      1: begin { <= }
          case var1Type.BaseType of
            btU8:
            if (var2Type.BaseType = btString) or (Var2Type.BaseType = btPChar) then
              b := tbtchar(tbtu8(var1^)) <= PSGetAnsiString(Var2, var2type)
            {+}
            {$IFNDEF PS_NOWIDESTRING}
            else if (var2Type.BaseType = btWideString) {$if declared(btPWideChar)} or (Var2Type.BaseType = btPWideChar){$ifend} then
              b := TbtWideChar(tbtu16(var1^)) <= PSGetWideString(Var2, var2type)
            {$ENDIF !PS_NOWIDESTRING}
            {+.}
            else
              b := tbtu8(var1^) <= PSGetUInt(Var2, var2type);
            btS8: b := tbts8(var1^) <= PSGetInt(Var2, var2type);
            btU16: b := tbtu16(var1^) <= PSGetUInt(Var2, var2type);
            btS16: b := tbts16(var1^) <= PSGetInt(Var2, var2type);
            btU32: b := tbtu32(var1^) <= PSGetUInt(Var2, var2type);
            btS32:
              begin
                if var2type.BaseType = btPointer then
                begin
                  var2type := PIFTypeRec(Pointer(IPointer(var2)+PointerSize)^);
                  var2 := Pointer(var2^);
                  if (var2 = nil) or (var2type = nil) then
                    raise Exception.Create(RPS_TypeMismatch);
                end;
                case var2type.BaseType of
                  btU8: b := tbts32(var1^) <= tbtu8(Var2^);
                  btS8: b := tbts32(var1^) <= tbts8(Var2^);
                  btU16: b := tbts32(var1^) <= tbtu16(Var2^);
                  btS16: b := tbts32(var1^) <= tbts16(Var2^);
                  btU32: b := tbts32(var1^) <= Longint(tbtu32(Var2^));
                  btS32: b := tbts32(var1^) <= tbts32(Var2^);
                  btDouble: b := PSGetReal(Var1, var1type) <= tbtdouble(var2^);
                  btSingle: B := psGetReal(Var1, var1Type) <= tbtsingle(var2^);
                  btExtended: B := psGetReal(Var1, var1Type) <= tbtExtended(var2^);
                  {$IFNDEF PS_NOINT64}
                  btS64: b := tbts32(var1^) <= tbts64(Var2^);
                  {$ENDIF !PS_NOINT64}
                  btChar: b := tbts32(var1^) <= Ord(tbtchar(Var2^));
                  {$IFNDEF PS_NOWIDESTRING}
                  btWideChar: b := tbts32(var1^) <= Ord(TbtWideChar(Var2^));
                  {$ENDIF !PS_NOWIDESTRING}
                  btVariant: b := tbts32(var1^) <= Variant(Var2^);
                  else raise Exception.Create(RPS_TypeMismatch);
                end;
              end;            btSingle: b := tbtsingle(var1^) <= PSGetReal(Var2, var2type);
            btCurrency: b := tbtcurrency(var1^) <= PSGetCurrency(Var2, var2type);
            btDouble: b := tbtdouble(var1^) <= PSGetReal(Var2, var2type);
            btExtended: b := tbtextended(var1^) <= PSGetReal(Var2, var2type);
            {$IFNDEF PS_NOINT64}
            btS64: b := tbts64(var1^) <= PSGetInt64(Var2, var2type);
            {$ENDIF !PS_NOINT64}
            btPChar,btString: b := TbtString(var1^) <= PSGetAnsiString(Var2, var2type);
            btChar: b := tbtchar(var1^) <= PSGetAnsiString(Var2, var2type);
            {$IFNDEF PS_NOWIDESTRING}
            btWideChar: b := TbtWideChar(var1^) <= PSGetWideString(Var2, var2type);
            {+}{$if declared(btPWideChar)}btPWideChar,{$ifend}{+.}
            btWideString: b := tbtwidestring(var1^) <= PSGetWideString(Var2, var2type);
            btUnicodeString: b := TbtUnicodeString(var1^) <= PSGetUnicodeString(Var2, var2type);
            {$ENDIF !PS_NOWIDESTRING}
            btVariant:
              begin
                if not IntPIFVariantToVariant(var2, var2type, tvar) then
                begin
                  Result := False;
                end else
                  b := Variant(var1^) <= tvar;
              end;
            btSet:
              begin
                if var1Type = var2Type then
                begin
                  Set_Subset(var1, var2, TPSTypeRec_Set(var1Type).aByteSize, b);
                end else Result := False;
              end;
          else begin
              CMD_Err2(erTypeMismatch, TbtString(RPS_TypeMismatch));
              Exit;
            end;
          end;
          if not Result then begin
            CMD_Err2(erTypeMismatch, TbtString(RPS_TypeMismatch));
            Exit;
          end;
          SetBoolean(b, Result);
        end;
      2: begin { > }
          case var1Type.BaseType of
            btU8:
            if (var2Type.BaseType = btString) or (Var2Type.BaseType = btPChar) then
              b := tbtchar(tbtu8(var1^)) > PSGetAnsiString(Var2, var2type)
            {+}
            {$IFNDEF PS_NOWIDESTRING}
            else if (var2Type.BaseType = btWideString) {$if declared(btPWideChar)} or (Var2Type.BaseType = btPWideChar){$ifend} then
              b := TbtWideChar(tbtu16(var1^)) > PSGetWideString(Var2, var2type)
            {$ENDIF !PS_NOWIDESTRING}
            {+.}
            else
              b := tbtu8(var1^) > PSGetUInt(Var2, var2type);
            btS8: b := tbts8(var1^) > PSGetInt(Var2, var2type);
            btU16: b := tbtu16(var1^) > PSGetUInt(Var2, var2type);
            btS16: b := tbts16(var1^) > PSGetInt(Var2, var2type);
            btU32: b := tbtu32(var1^) > PSGetUInt(Var2, var2type);
            btS32:
              begin
                if var2type.BaseType = btPointer then
                begin
                  var2type := PIFTypeRec(Pointer(IPointer(var2)+PointerSize)^);
                  var2 := Pointer(var2^);
                  if (var2 = nil) or (var2type = nil) then
                    raise Exception.Create(RPS_TypeMismatch);
                end;
                case var2type.BaseType of
                  btU8: b := tbts32(var1^) > tbtu8(Var2^);
                  btS8: b := tbts32(var1^) > tbts8(Var2^);
                  btU16: b := tbts32(var1^) > tbtu16(Var2^);
                  btS16: b := tbts32(var1^) > tbts16(Var2^);
                  btU32: b := tbts32(var1^) > Longint(tbtu32(Var2^));
                  btS32: b := tbts32(var1^) > tbts32(Var2^);
                  btDouble: b := PSGetReal(Var1, var1type) > tbtdouble(var2^);
                  btSingle: B := psGetReal(Var1, var1Type) > tbtsingle(var2^);
                  btExtended: B := psGetReal(Var1, var1Type) > tbtExtended(var2^);
                  {$IFNDEF PS_NOINT64}
                  btS64: b := tbts32(var1^) > tbts64(Var2^);
                  {$ENDIF !PS_NOINT64}
                  btChar: b := tbts32(var1^) > Ord(tbtchar(Var2^));
                  {$IFNDEF PS_NOWIDESTRING}
                  btWideChar: b := tbts32(var1^) = Ord(TbtWideChar(Var2^));
                  {$ENDIF !PS_NOWIDESTRING}
                  btVariant: b := tbts32(var1^) > Variant(Var2^);
                  else raise Exception.Create(RPS_TypeMismatch);
                end;
              end;            btSingle: b := tbtsingle(var1^) > PSGetReal(Var2, var2type);
            btDouble: b := tbtdouble(var1^) > PSGetReal(Var2, var2type);
            btExtended: b := tbtextended(var1^) > PSGetReal(Var2, var2type);
            btCurrency: b := tbtcurrency(var1^) > PSGetCurrency(Var2, var2type);
            {$IFNDEF PS_NOINT64}
            btS64: b := tbts64(var1^) > PSGetInt64(Var2, var2type);
            {$ENDIF !PS_NOINT64}
            btPChar,btString: b := TbtString(var1^) > PSGetAnsiString(Var2, var2type);
            btChar: b := tbtchar(var1^) > PSGetAnsiString(Var2, var2type);
            {$IFNDEF PS_NOWIDESTRING}
            btWideChar: b := TbtWideChar(var1^) > PSGetWideString(Var2, var2type);
            {+}{$if declared(btPWideChar)}btPWideChar,{$ifend}{+.}
            btWideString: b := tbtwidestring(var1^) > PSGetWideString(Var2, var2type);
            btUnicodeString: b := TbtUnicodeString(var1^) > PSGetUnicodeString(Var2, var2type);
            {$ENDIF !PS_NOWIDESTRING}
            btVariant:
              begin
                if not IntPIFVariantToVariant(var2, var2type, tvar) then
                begin
                  Result := False;
                end else
                  b := Variant(var1^) > tvar;
              end;
          else begin
              CMD_Err2(erTypeMismatch, TbtString(RPS_TypeMismatch));
              Exit;
            end;
          end;
          if not Result then begin
            CMD_Err2(erTypeMismatch, TbtString(RPS_TypeMismatch));
            Exit;
          end;
          SetBoolean(b, Result);
        end;
      3: begin { < }
          case var1Type.BaseType of
            btU8:
            if (var2Type.BaseType = btString) or (Var2Type.BaseType = btPChar) then
              b := tbtchar(tbtu8(var1^)) < PSGetAnsiString(Var2, var2type)
            {+}
            {$IFNDEF PS_NOWIDESTRING}
            else if (var2Type.BaseType = btWideString)
              {$if declared(btPWideChar)} or (Var2Type.BaseType = btPWideChar){$ifend} then
              b := TbtWideChar(tbtu16(var1^)) < PSGetWideString(Var2, var2type)
            {$ENDIF !PS_NOWIDESTRING}
            {+.}
            else
              b := tbtu8(var1^) < PSGetUInt(Var2, var2type);
            btS8: b := tbts8(var1^) < PSGetInt(Var2, var2type);
            btU16: b := tbtu16(var1^) < PSGetUInt(Var2, var2type);
            btS16: b := tbts16(var1^) < PSGetInt(Var2, var2type);
            btU32: b := tbtu32(var1^) < PSGetUInt(Var2, var2type);
            btS32:
              begin
                if var2type.BaseType = btPointer then
                begin
                  var2type := PIFTypeRec(Pointer(IPointer(var2)+PointerSize)^);
                  var2 := Pointer(var2^);
                  if (var2 = nil) or (var2type = nil) then
                    raise Exception.Create(RPS_TypeMismatch);
                end;
                case var2type.BaseType of
                  btU8: b := tbts32(var1^) < tbtu8(Var2^);
                  btS8: b := tbts32(var1^) < tbts8(Var2^);
                  btU16: b := tbts32(var1^) < tbtu16(Var2^);
                  btS16: b := tbts32(var1^) < tbts16(Var2^);
                  btU32: b := tbts32(var1^) < Longint(tbtu32(Var2^));
                  btS32: b := tbts32(var1^) < tbts32(Var2^);
                  btDouble: b := PSGetReal(Var1, var1type) < tbtdouble(var2^);
                  btSingle: B := psGetReal(Var1, var1Type) < tbtsingle(var2^);
                  btExtended: B := psGetReal(Var1, var1Type) < tbtExtended(var2^);
                  {$IFNDEF PS_NOINT64}
                  btS64: b := tbts32(var1^) < tbts64(Var2^);
                  {$ENDIF !PS_NOINT64}
                  btChar: b := tbts32(var1^) < Ord(tbtchar(Var2^));
                  {$IFNDEF PS_NOWIDESTRING}
                  btWideChar: b := tbts32(var1^) < Ord(TbtWideChar(Var2^));
                  {$ENDIF !PS_NOWIDESTRING}
                  btVariant: b := tbts32(var1^) < Variant(Var2^);
                  else raise Exception.Create(RPS_TypeMismatch);
                end;
              end;            btSingle: b := tbtsingle(var1^) < PSGetReal(Var2, var2type);
            btDouble: b := tbtdouble(var1^) < PSGetReal(Var2, var2type);
            btCurrency: b := tbtcurrency(var1^) < PSGetCurrency(Var2, var2type);
            btExtended: b := tbtextended(var1^) < PSGetReal(Var2, var2type);
            {$IFNDEF PS_NOINT64}
            btS64: b := tbts64(var1^) < PSGetInt64(Var2, var2type);
            {$ENDIF !PS_NOINT64}
            btPChar,btString: b := TbtString(var1^) < PSGetAnsiString(Var2, var2type);
            btChar: b := tbtchar(var1^) < PSGetAnsiString(Var2, var2type);
            {$IFNDEF PS_NOWIDESTRING}
            btWideChar: b := TbtWideChar(var1^) < PSGetWideString(Var2, var2type);
            {+}{$if declared(btPWideChar)}btPWideChar,{$ifend}{+.}
            btWideString: b := tbtwidestring(var1^) < PSGetWideString(Var2, var2type);
            btUnicodeString: b := TbtUnicodeString(var1^) < PSGetUnicodeString(Var2, var2type);
            {$ENDIF !PS_NOWIDESTRING}
            btVariant:
              begin
                if not IntPIFVariantToVariant(var2, var2type, tvar) then
                begin
                  Result := False;
                end else
                  b := Variant(var1^) < tvar;
              end;
          else begin
              CMD_Err2(erTypeMismatch, TbtString(RPS_TypeMismatch));
              Exit;
            end;
          end;
          if not Result then begin
            CMD_Err2(erTypeMismatch, TbtString(RPS_TypeMismatch));
            Exit;
          end;
          SetBoolean(b, Result);
        end;
      4: begin { <> }
          case var1Type.BaseType of
            btInterface:
              begin
                if var2Type.BaseType = btInterface then
                  b := Pointer(var1^) <> Pointer(var2^) // no need to cast it to IUnknown
                else
                  Result := False;
              end;
            btClass:
              begin
                if var2Type.BaseType = btclass then
                  b := TObject(var1^) <> TObject(var2^)
                else
                  Result := False;
              end;
            btU8:
            if (var2Type.BaseType = btString) or (Var2Type.BaseType = btPChar) then
              b := tbtchar(tbtu8(var1^)) <> PSGetAnsiString(Var2, var2type)
            {+}
            {$IFNDEF PS_NOWIDESTRING}
            else if (var2Type.BaseType = btWideString)
              {$if declared(btPWideChar)} or (Var2Type.BaseType = btPWideChar){$ifend} then
              b := TbtWideChar(tbtu16(var1^)) <> PSGetWideString(Var2, var2type)
            {$ENDIF !PS_NOWIDESTRING}
            {+.}
            else
              b := tbtu8(var1^) <> PSGetUInt(Var2, var2type);
            btS8: b := tbts8(var1^) <> PSGetInt(Var2, var2type);
            btU16: b := tbtu16(var1^) <> PSGetUInt(Var2, var2type);
            btS16: b := tbts16(var1^) <> PSGetInt(Var2, var2type);
            btProcPtr:
              begin
                if Pointer(Var1^) = Pointer(Var2^) then begin
                  if Longint(Var1^) = 0 then begin
                    b := ((Pointer(Pointer(IPointer(Var1)+PointerSize2)^) <> Pointer(Pointer(IPointer(Var2)+PointerSize2)^)) or
                   (Pointer(Pointer(IPointer(Var1)+PointerSize2)^) <> Pointer(Pointer(IPointer(Var2)+PointerSize2)^)));
                  end else
                    b := False;
                end else
                  b := True;
              end;
            btU32: b := tbtu32(var1^) <> PSGetUInt(Var2, var2type);
            btS32:
              begin
                if var2type.BaseType = btPointer then
                begin
                  var2type := PIFTypeRec(Pointer(IPointer(var2)+PointerSize)^);
                  var2 := Pointer(var2^);
                  if (var2 = nil) or (var2type = nil) then
                    raise Exception.Create(RPS_TypeMismatch);
                end;
                case var2type.BaseType of
                  btU8: b := tbts32(var1^) <> tbtu8(Var2^);
                  btS8: b := tbts32(var1^) <> tbts8(Var2^);
                  btU16: b := tbts32(var1^) <> tbtu16(Var2^);
                  btS16: b := tbts32(var1^) <> tbts16(Var2^);
                  btProcPtr, btU32: b := tbts32(var1^)<> Longint(tbtu32(Var2^));
                  btS32: b := tbts32(var1^) <> tbts32(Var2^);
                  btDouble: b := PSGetReal(Var1, var1type) <> tbtdouble(var2^);
                  btSingle: B := psGetReal(Var1, var1Type) <> tbtsingle(var2^);
                  btExtended: B := psGetReal(Var1, var1Type) <> tbtExtended(var2^);
                  {$IFNDEF PS_NOINT64}
                  btS64: b := tbts32(var1^) <> tbts64(Var2^);
                  {$ENDIF !PS_NOINT64}
                  btChar: b := tbts32(var1^) <> Ord(tbtchar(Var2^));
                  {$IFNDEF PS_NOWIDESTRING}
                  btWideChar: b := tbts32(var1^) <> Ord(TbtWideChar(Var2^));
                  {$ENDIF !PS_NOWIDESTRING}
                  btVariant: b := tbts32(var1^) <> Variant(Var2^);
                  else raise Exception.Create(RPS_TypeMismatch);
                end;
              end;            btSingle: b := tbtsingle(var1^) <> PSGetReal(Var2, var2type);
            btDouble: b := tbtdouble(var1^) <> PSGetReal(Var2, var2type);
            btExtended: b := tbtextended(var1^) <> PSGetReal(Var2, var2type);
            btCurrency: b := tbtcurrency(var1^) <> PSGetCurrency(Var2, var2type);
            btPChar,btString: b := TbtString(var1^) <> PSGetAnsiString(Var2, var2type);
            {$IFNDEF PS_NOINT64}
            btS64: b := tbts64(var1^) <> PSGetInt64(Var2, var2type);
            {$ENDIF !PS_NOINT64}
            btChar: b := tbtchar(var1^) <> PSGetAnsiString(Var2, var2type);
            {$IFNDEF PS_NOWIDESTRING}
            btWideChar: b := TbtWideChar(var1^) <> PSGetWideString(Var2, var2type);
            {+}{$if declared(btPWideChar)}btPWideChar,{$ifend}{+.}
            btWideString: b := tbtwidestring(var1^) <> PSGetWideString(Var2, var2type);
            btUnicodeString: b := TbtUnicodeString(var1^) <> PSGetUnicodeString(Var2, var2type);
            {$ENDIF !PS_NOWIDESTRING}
            btVariant:
              begin
                if not IntPIFVariantToVariant(var2, var2type, tvar) then
                begin
                  Result := False;
                end else
                  b := Variant(var1^) <> tvar;
              end;
            btSet:
              begin
                if var1Type = var2Type then
                begin
                  Set_Equal(var1, var2, TPSTypeRec_Set(var1Type).aByteSize, b);
                  b := not b;
                end else Result := False;
              end;
            btRecord:
              begin
                if var1Type = var2Type then
                begin
                  Set_Equal(var1, var2, TPSTypeRec_Record(var1Type).RealSize, b);
                  b := not b;
                end else Result := False;
              end

          else begin
              CMD_Err2(erTypeMismatch, TbtString(RPS_TypeMismatch));
              Exit;
            end;
          end;
          if not Result then begin
            CMD_Err2(erTypeMismatch, TbtString(RPS_TypeMismatch));
            Exit;
          end;
          SetBoolean(b, Result);
        end;
      5: begin { = }
          case var1Type.BaseType of
            btInterface:
              begin
                if var2Type.BaseType = btInterface then
                  b := Pointer(var1^) = Pointer(var2^) // no need to cast it to IUnknown
                else
                  Result := False;
              end;
            btClass:
              begin
                if var2Type.BaseType = btclass then
                  b := TObject(var1^) = TObject(var2^)
                else
                  Result := False;
              end;
            btU8:
            if (var2Type.BaseType = btString) or (Var2Type.BaseType = btPChar) then
              b := tbtchar(tbtu8(var1^)) = PSGetAnsiString(Var2, var2type)
            {+}
            {$IFNDEF PS_NOWIDESTRING}
            else if (var2Type.BaseType = btWideString)
              {$if declared(btPWideChar)} or (Var2Type.BaseType = btPWideChar){$ifend} then
              b := TbtWideChar(tbtu16(var1^)) = PSGetWideString(Var2, var2type)
            {$ENDIF !PS_NOWIDESTRING}
            {+.}
            else
              b := tbtu8(var1^) = PSGetUInt(Var2, var2type);
            btS8: b := tbts8(var1^) = PSGetInt(Var2, var2type);
            btU16: b := tbtu16(var1^) = PSGetUInt(Var2, var2type);
            btS16: b := tbts16(var1^) = PSGetInt(Var2, var2type);
            btU32: b := tbtu32(var1^) = PSGetUInt(Var2, var2type);
            btProcPtr:
              begin
                if Pointer(Var1^) = Pointer(Var2^) then begin
                  if Longint(Var1^) = 0 then begin
                    b := ((Pointer(Pointer(IPointer(Var1)+PointerSize2)^)
                           = Pointer(Pointer(IPointer(Var2)+PointerSize2)^))
                      and (Pointer(Pointer(IPointer(Var1)+PointerSize2)^)
                           = Pointer(Pointer(IPointer(Var2)+PointerSize2)^))
                      );
                  end else
                    b := True;
                end else
                  b := False;
              end;
            btS32:
              begin
                if var2type.BaseType = btPointer then begin
                  var2type := PIFTypeRec(Pointer(IPointer(var2)+PointerSize)^);
                  var2 := Pointer(var2^);
                  if (var2 = nil) or (var2type = nil) then
                    raise Exception.Create(RPS_TypeMismatch);
                end;
                case var2type.BaseType of
                  btU8: b := tbts32(var1^) = tbtu8(Var2^);
                  btS8: b := tbts32(var1^) = tbts8(Var2^);
                  btU16: b := tbts32(var1^) = tbtu16(Var2^);
                  btS16: b := tbts32(var1^) = tbts16(Var2^);
                  btProcPtr, btU32: b := tbts32(var1^) = Longint(tbtu32(Var2^));
                  btS32: b := tbts32(var1^) = tbts32(Var2^);
                  btDouble: b := PSGetReal(Var1, var1type) = tbtdouble(var2^);
                  btSingle: B := psGetReal(Var1, var1Type) = tbtsingle(var2^);
                  btExtended: B := psGetReal(Var1, var1Type) = tbtExtended(var2^);
                  {$IFNDEF PS_NOINT64}
                  btS64: b := tbts32(var1^) = tbts64(Var2^);
                  {$ENDIF !PS_NOINT64}
                  btChar: b := tbts32(var1^) = Ord(tbtchar(Var2^));
                  {$IFNDEF PS_NOWIDESTRING}
                  btWideChar: b := tbts32(var1^) = Ord(TbtWideChar(Var2^));
                  {$ENDIF !PS_NOWIDESTRING}
                  btVariant: b := tbts32(var1^) = Variant(Var2^);
                  else raise Exception.Create(RPS_TypeMismatch);
                end;
              end;            btSingle: b := tbtsingle(var1^) = PSGetReal(Var2, var2type);
            btDouble: b := tbtdouble(var1^) = PSGetReal(Var2, var2type);
            btExtended: b := tbtextended(var1^) = PSGetReal(Var2, var2type);
            btCurrency: b := tbtcurrency(var1^) = PSGetCurrency(Var2, var2type);
            btPchar, btString: b := TbtString(var1^) = PSGetAnsiString(Var2, var2type);
            {$IFNDEF PS_NOINT64}
            btS64: b := tbts64(var1^) = PSGetInt64(Var2, var2type);
            {$ENDIF !PS_NOINT64}
            btChar: b := tbtchar(var1^) = PSGetAnsiString(Var2, var2type);
            {$IFNDEF PS_NOWIDESTRING}
            btWideChar: b := TbtWideChar(var1^) = PSGetWideString(Var2, var2type);
            {+}{$if declared(btPWideChar)}btPWideChar,{$ifend}{+.}
            btWideString: b := tbtwidestring(var1^) = PSGetWideString(Var2, var2type);
            btUnicodeString: b := TbtUnicodeString(var1^) = PSGetUnicodeString(Var2, var2type);
            {$ENDIF !PS_NOWIDESTRING}
            btVariant:
              begin
                if not IntPIFVariantToVariant(var2, var2type, tvar) then
                begin
                  Result := False;
                end else
                  b := Variant(var1^) = tvar;
              end;
            btSet:
              begin
                if var1Type = var2Type then
                begin
                  Set_Equal(var1, var2, TPSTypeRec_Set(var1Type).aByteSize, b);
                end else Result := False;
              end;
            btRecord:
              begin
                if var1Type = var2Type then
                begin
                  Set_Equal(var1, var2, TPSTypeRec_Record(var1Type).RealSize, b);
                end else Result := False;
              end
          else begin
              CMD_Err2(erTypeMismatch, TbtString(RPS_TypeMismatch));
              Exit;
            end;
          end;
          if not Result then begin
            CMD_Err2(erTypeMismatch, TbtString(RPS_TypeMismatch));
            Exit;
          end;
          SetBoolean(b, Result);
        end;
      6: begin { in }
          if (var2Type.BaseType = btArray) and (TPSTypeRec_Array(var2type).ArrayType.BaseType = btVariant) then
          begin
            b := VariantInArray(var1, var1Type, var2);
            SetBoolean(b, Result);
          end else
          if var2Type.BaseType = btSet then
          begin
            Cmd := PSGetUInt(var1, var1type);
            if not Result then
            begin
              CMD_Err2(erTypeMismatch, TbtString(RPS_TypeMismatch));
              Exit;
            end;
            if Cmd >= Cardinal(TPSTypeRec_Set(var2Type).aBitSize) then
            begin
              CMD_Err2(erOutofRecordRange, TbtString(RPS_OutofRecordRange));
              Result := False;
              Exit;
            end;
            Set_membership(Cmd, var2, b);
            SetBoolean(b, Result);
          end else
          begin
            CMD_Err2(erTypeMismatch, TbtString(RPS_TypeMismatch));
            Exit;
          end;
        end;
      7:
        begin // is
          case var1Type.BaseType of
            btClass:
              begin
                if var2type.BaseType <> btU32 then
                  Result := False
                else
                begin
                  var2type := FTypes[tbtu32(var2^)];
                  if (var2type = nil) or (var2type.BaseType <> btClass) then
                    Result := False
                  else
                  begin
                    Setboolean(Class_IS(Self, TObject(var1^), var2type), Result);
                  end;
                end;
              end;
          else begin
              CMD_Err2(erTypeMismatch, TbtString(RPS_TypeMismatch));
              Exit;
            end;
          end;
          if not Result then begin
            CMD_Err2(erTypeMismatch, TbtString(RPS_TypeMismatch));
            Exit;
          end;
        end;
    else begin
        Result := False;
        CMD_Err2(erInvalidOpcodeParameter, TbtString(RPS_InvalidOpcodeParameter));
        Exit;
      end;
    end;
  except
    {$IFDEF DELPHI6UP}
    Tmp := AcquireExceptionObject;
    {$ELSE}
    if RaiseList <> nil then
    begin
      Tmp := Exception(PRaiseFrame(RaiseList)^.ExceptObject);
      PRaiseFrame(RaiseList)^.ExceptObject := nil;
    end else
      Tmp := nil;
    {$ENDIF !DELPHI6UP}
    if Tmp <> nil then
    begin
      if Tmp is EPSException then
      begin
        Result := False;
        ExceptionProc(EPSException(tmp).ProcNo, EPSException(tmp).ProcPos, erCustomError, TbtString(EPSException(tmp).Message), nil);
        Exit;
      end else
      if Tmp is EDivByZero then
      begin
        Result := False;
        CMD_Err3(erDivideByZero, TbtString(Exception(Tmp).Message), Tmp);
        Exit;
      end;
      if Tmp is EZeroDivide then
      begin
        Result := False;
        CMD_Err3(erDivideByZero, TbtString(Exception(Tmp).Message), Tmp);
        Exit;
      end;
      if Tmp is EMathError then
      begin
        Result := False;
        CMD_Err3(erMathError, TbtString(Exception(Tmp).Message), Tmp);
        Exit;
      end;
    end;
    if (tmp <> nil) and (Tmp is {+}ExceptionBase{+.}) then
      CMD_Err3(erException, TbtString(Exception(Tmp).Message), Tmp)
    else
      CMD_Err3(erException, '', Tmp);
    Result := False;
  end;
end; // function TPSExec.DoBooleanCalc

function VarIsFloat(const V: Variant): Boolean;
begin
  {+}
  //Result := VarType(V) in [varSingle, varDouble, varCurrency];
  Result := TVarData(V).VType in [varSingle, varDouble, varCurrency];
  {+.}
end;

function TPSExec.DoCalc(var1, Var2: Pointer; var1Type, var2type: TPSTypeRec; CalcType: Cardinal): Boolean;
// var1=dest, var2=src
var
  Tmp: TObject;
  tvar: Variant;
begin
  try
    Result := True;
    case CalcType of
      0: begin { + }
          case var1Type.BaseType of
            btU8: tbtU8(var1^) := tbtU8(var1^) + PSGetUInt(Var2, var2type);
            btS8: tbts8(var1^) := tbts8(var1^) + PSGetInt(Var2, var2type);
            btU16: tbtU16(var1^) := tbtU16(var1^) + PSGetUInt(Var2, var2type);
            btS16: tbts16(var1^) := tbts16(var1^) + PSGetInt(Var2, var2type);
            btU32:
              begin
                if var2type.BaseType = btPointer then
                begin
                  var2type := PIFTypeRec(Pointer(IPointer(var2)+PointerSize)^);
                  var2 := Pointer(var2^);
                  if (var2 = nil) or (var2type = nil) then
                    raise Exception.Create(RPS_TypeMismatch);
                end;
                case var2type.BaseType of
                  btU8: tbtU32(var1^) := tbtU32(var1^) + tbtu8(var2^);
                  btS8: tbtU32(var1^) := tbtU32(var1^) + cardinal(longint(tbts8(var2^)));
                  btU16: tbtU32(var1^) := tbtU32(var1^) + tbtu16(var2^);
                  btS16: tbtU32(var1^) := tbtU32(var1^) + cardinal(longint(tbts16(var2^)));
                  btU32: tbtU32(var1^) := tbtU32(var1^) + tbtu32(var2^);
                  btS32: tbtU32(var1^) := tbtU32(var1^) + cardinal(tbts32(var2^));
                  {$IFNDEF PS_NOINT64}
                  btS64: tbtU32(var1^) := tbtU32(var1^) + tbts64(var2^);
                  {$ENDIF !PS_NOINT64}
                  btChar: tbtU32(var1^) := tbtU32(var1^) +  Ord(tbtchar(var2^));
                  {$IFNDEF PS_NOWIDESTRING}
                  btWideChar: tbtU32(var1^) := tbtU32(var1^) + Ord(TbtWideChar(var2^));
                  {$ENDIF !PS_NOWIDESTRING}
                  btVariant: tbtU32(var1^) := tbtU32(var1^) + Variant(var2^);
                  else raise Exception.Create(RPS_TypeMismatch);
                end;
              end;
            btS32:
              begin
                if var2type.BaseType = btPointer then
                begin
                  var2type := PIFTypeRec(Pointer(IPointer(var2)+PointerSize)^);
                  var2 := Pointer(var2^);
                  if (var2 = nil) or (var2type = nil) then
                    raise Exception.Create(RPS_TypeMismatch);
                end;
                case var2type.BaseType of
                  btU8: tbts32(var1^) := tbts32(var1^) + tbtu8(var2^);
                  btS8: tbts32(var1^) := tbts32(var1^) + tbts8(var2^);
                  btU16: tbts32(var1^) := tbts32(var1^) + tbtu16(var2^);
                  btS16: tbts32(var1^) := tbts32(var1^) + tbts16(var2^);
                  btU32: tbts32(var1^) := tbts32(var1^) + Longint(tbtu32(var2^));
                  btS32: tbts32(var1^) := tbts32(var1^) + tbts32(var2^);
                  {$IFNDEF PS_NOINT64}
                  btS64: tbts32(var1^) := tbts32(var1^) + tbts64(var2^);
                  {$ENDIF !PS_NOINT64}
                  btChar: tbts32(var1^) := tbts32(var1^) +  Ord(tbtchar(var2^));
                  {$IFNDEF PS_NOWIDESTRING}
                  btWideChar: tbts32(var1^) := tbts32(var1^) + Ord(TbtWideChar(var2^));
                  {$ENDIF !PS_NOWIDESTRING}
                  btVariant: tbts32(var1^) := tbts32(var1^) + Variant(var2^);
                  else raise Exception.Create(RPS_TypeMismatch);
                end;
              end;
            {$IFNDEF PS_NOINT64}
            btS64:  tbts64(var1^) := tbts64(var1^) + PSGetInt64(var2, var2type);
            {$ENDIF !PS_NOINT64}
            btSingle:
              begin
                if var2type.BaseType = btPointer then
                begin
                  var2type := PIFTypeRec(Pointer(IPointer(var2)+PointerSize)^);
                  var2 := Pointer(var2^);
                  if (var2 = nil) or (var2type = nil) then
                    raise Exception.Create(RPS_TypeMismatch);
                end;
                case var2type.BaseType of
                  btU8: tbtsingle(var1^) := tbtsingle(var1^) + tbtu8(var2^);
                  btS8: tbtsingle(var1^) := tbtsingle(var1^) + tbts8(var2^);
                  btU16: tbtsingle(var1^) := tbtsingle(var1^) + tbtu16(var2^);
                  btS16: tbtsingle(var1^) := tbtsingle(var1^) + tbts16(var2^);
                  btU32: tbtsingle(var1^) := tbtsingle(var1^) + tbtu32(var2^);
                  btS32: tbtsingle(var1^) := tbtsingle(var1^) + tbts32(var2^);
                  {$IFNDEF PS_NOINT64}
                  btS64: tbtsingle(var1^) := tbtsingle(var1^) + tbts64(var2^);
                  {$ENDIF !PS_NOINT64}
                  btSingle: tbtsingle(var1^) := tbtsingle(var1^) + tbtsingle(var2^);
                  btDouble: tbtsingle(var1^) := tbtsingle(var1^) + tbtdouble(var2^);
                  btExtended: tbtsingle(var1^) := tbtsingle(var1^) + tbtextended(var2^);
                  btCurrency: tbtsingle(var1^) := tbtsingle(var1^) + tbtcurrency(var2^);
                  btVariant:  tbtsingle(var1^) := tbtsingle(var1^) +  Variant(var2^);
                  else raise Exception.Create(RPS_TypeMismatch);
                end;
              end;
            btDouble:
              begin
                if var2type.BaseType = btPointer then
                begin
                  var2type := PIFTypeRec(Pointer(IPointer(var2)+PointerSize)^);
                  var2 := Pointer(var2^);
                  if (var2 = nil) or (var2type = nil) then
                    raise Exception.Create(RPS_TypeMismatch);
                end;
                case var2type.BaseType of
                  btU8: tbtdouble(var1^) := tbtdouble(var1^) + tbtu8(var2^);
                  btS8: tbtdouble(var1^) := tbtdouble(var1^) + tbts8(var2^);
                  btU16: tbtdouble(var1^) := tbtdouble(var1^) + tbtu16(var2^);
                  btS16: tbtdouble(var1^) := tbtdouble(var1^) + tbts16(var2^);
                  btU32: tbtdouble(var1^) := tbtdouble(var1^) + tbtu32(var2^);
                  btS32: tbtdouble(var1^) := tbtdouble(var1^) + tbts32(var2^);
                  {$IFNDEF PS_NOINT64}
                  btS64: tbtdouble(var1^) := tbtdouble(var1^) + tbts64(var2^);
                  {$ENDIF !PS_NOINT64}
                  btSingle: tbtdouble(var1^) := tbtdouble(var1^) + tbtsingle(var2^);
                  btDouble: tbtdouble(var1^) := tbtdouble(var1^) + tbtdouble(var2^);
                  btExtended: tbtdouble(var1^) := tbtdouble(var1^) + tbtextended(var2^);
                  btCurrency: tbtdouble(var1^) := tbtdouble(var1^) + tbtcurrency(var2^);
                  btVariant:  tbtdouble(var1^) := tbtdouble(var1^) +  Variant(var2^);
                  else raise Exception.Create(RPS_TypeMismatch);
                end;
              end;
            btCurrency:
              begin
                if var2type.BaseType = btPointer then
                begin
                  var2type := PIFTypeRec(Pointer(IPointer(var2)+PointerSize)^);
                  var2 := Pointer(var2^);
                  if (var2 = nil) or (var2type = nil) then
                    raise Exception.Create(RPS_TypeMismatch);
                end;
                case var2type.BaseType of
                  btU8: tbtcurrency(var1^) := tbtcurrency(var1^) + tbtu8(var2^);
                  btS8: tbtcurrency(var1^) := tbtcurrency(var1^) + tbts8(var2^);
                  btU16: tbtcurrency(var1^) := tbtcurrency(var1^) + tbtu16(var2^);
                  btS16: tbtcurrency(var1^) := tbtcurrency(var1^) + tbts16(var2^);
                  btU32: tbtcurrency(var1^) := tbtdouble(var1^) + tbtu32(var2^);
                  btS32: tbtcurrency(var1^) := tbtcurrency(var1^) + tbts32(var2^);
                  {$IFNDEF PS_NOINT64}
                  btS64: tbtcurrency(var1^) := tbtdouble(var1^) + tbts64(var2^);
                  {$ENDIF !PS_NOINT64}
                  btSingle: tbtcurrency(var1^) := tbtcurrency(var1^) + tbtsingle(var2^);
                  btDouble: tbtcurrency(var1^) := tbtcurrency(var1^) + tbtdouble(var2^);
                  btExtended: tbtcurrency(var1^) := tbtcurrency(var1^) + tbtextended(var2^);
                  btCurrency: tbtcurrency(var1^) := tbtcurrency(var1^) + tbtcurrency(var2^);
                  btVariant:  tbtcurrency(var1^) := tbtcurrency(var1^) +  Variant(var2^);
                  else raise Exception.Create(RPS_TypeMismatch);
                end;
              end;
            btExtended:
              begin
                if var2type.BaseType = btPointer then
                begin
                  var2type := PIFTypeRec(Pointer(IPointer(var2)+PointerSize)^);
                  var2 := Pointer(var2^);
                  if (var2 = nil) or (var2type = nil) then
                    raise Exception.Create(RPS_TypeMismatch);
                end;
                case var2type.BaseType of
                  btU8: tbtextended(var1^) := tbtextended(var1^) + tbtu8(var2^);
                  btS8: tbtextended(var1^) := tbtextended(var1^) + tbts8(var2^);
                  btU16: tbtextended(var1^) := tbtextended(var1^) + tbtu16(var2^);
                  btS16: tbtextended(var1^) := tbtextended(var1^) + tbts16(var2^);
                  btU32: tbtextended(var1^) := tbtextended(var1^) + tbtu32(var2^);
                  btS32: tbtextended(var1^) := tbtextended(var1^) + tbts32(var2^);
                  {$IFNDEF PS_NOINT64}
                  btS64: tbtextended(var1^) := tbtextended(var1^) + tbts64(var2^);
                  {$ENDIF PS_NOINT64}
                  btSingle: tbtextended(var1^) := tbtextended(var1^) + tbtsingle(var2^);
                  btDouble: tbtextended(var1^) := tbtextended(var1^) + tbtdouble(var2^);
                  btExtended: tbtextended(var1^) := tbtextended(var1^) + tbtextended(var2^);
                  btCurrency: tbtextended(var1^) := tbtextended(var1^) + tbtcurrency(var2^);
                  btVariant:  tbtextended(var1^) := tbtextended(var1^) +  Variant(var2^);
                  else raise Exception.Create(RPS_TypeMismatch);
                end;
              end;
            {+}
            btPChar, btString: begin
              var1Type.Clear;
              TbtString(var1^) := TbtString(var1^) + PSGetAnsiString(Var2, var2type);
            end;
            {+.}
            btChar: tbtchar(var1^) := tbtchar(ord(tbtchar(var1^)) +  PSGetUInt(Var2, var2type));
            {$IFNDEF PS_NOWIDESTRING}
            btWideChar: TbtWideChar(var1^) := widechar(ord(TbtWideChar(var1^)) + PSGetUInt(Var2, var2type));
            {+}{$if declared(btPWideChar)}btPWideChar,{$ifend}{+.}
            btWideString: {+} begin
              var1Type.Clear;
              tbtWideString(var1^) := tbtWideString(var1^) + PSGetWideString(Var2, var2type);
            end; {+.}
            btUnicodeString: TbtUnicodeString(var1^) := TbtUnicodeString(var1^) + PSGetUnicodeString(Var2, var2type);
            {$ENDIF !PS_NOWIDESTRING}
            btVariant:
              begin
                tvar := null;
                if not IntPIFVariantToVariant(var2, var2type, tvar) then
                begin
                  Result := False;
                end else
                  Variant(var1^) := Variant(var1^) + tvar;
              end;
            btSet:
              begin
                if var1Type = var2Type then
                begin
                  Set_Union(var1, var2, TPSTypeRec_Set(var1Type).aByteSize);
                end else Result := False;
              end;

          else begin
              CMD_Err2(erTypeMismatch, TbtString(RPS_TypeMismatch));
              Exit;
            end;
          end;
          if not Result then begin
            CMD_Err2(erTypeMismatch, TbtString(RPS_TypeMismatch));
            Exit;
          end;
        end;
      1: begin { - }
          case var1Type.BaseType of
            btU8: tbtU8(var1^) := tbtU8(var1^) - PSGetUInt(Var2, var2type);
            btS8: tbts8(var1^) := tbts8(var1^) - PSGetInt(Var2, var2type);
            btU16: tbtU16(var1^) := tbtU16(var1^) - PSGetUInt(Var2, var2type);
            btS16: tbts16(var1^) := tbts16(var1^) - PSGetInt(Var2, var2type);
            btU32:
              begin
                if var2type.BaseType = btPointer then
                begin
                  var2type := PIFTypeRec(Pointer(IPointer(var2)+PointerSize)^);
                  var2 := Pointer(var2^);
                  if (var2 = nil) or (var2type = nil) then
                    raise Exception.Create(RPS_TypeMismatch);
                end;
                case var2type.BaseType of
                  btU8: tbtU32(var1^) := tbtU32(var1^) - tbtu8(var2^);
                  btS8: tbtU32(var1^) := tbtU32(var1^) - cardinal(longint(tbts8(var2^)));
                  btU16: tbtU32(var1^) := tbtU32(var1^) - tbtu16(var2^);
                  btS16: tbtU32(var1^) := tbtU32(var1^) - cardinal(longint(tbts16(var2^)));
                  btU32: tbtU32(var1^) := tbtU32(var1^) - tbtu32(var2^);
                  btS32: tbtU32(var1^) := tbtU32(var1^) - cardinal(tbts32(var2^));
                  {$IFNDEF PS_NOINT64}
                  btS64: tbtU32(var1^) := tbtU32(var1^) - tbts64(var2^);
                  {$ENDIF !PS_NOINT64}
                  btChar: tbtU32(var1^) := tbtU32(var1^) -  Ord(tbtchar(var2^));
                  {$IFNDEF PS_NOWIDESTRING}
                  btWideChar: tbtU32(var1^) := tbtU32(var1^) - Ord(TbtWideChar(var2^));
                  {$ENDIF !PS_NOWIDESTRING}
                  btVariant: tbtU32(var1^) := tbtU32(var1^) - Variant(var2^);
                  else raise Exception.Create(RPS_TypeMismatch);
                end;
              end;
            btS32:
              begin
                if var2type.BaseType = btPointer then
                begin
                  var2type := PIFTypeRec(Pointer(IPointer(var2)+PointerSize)^);
                  var2 := Pointer(var2^);
                  if (var2 = nil) or (var2type = nil) then
                    raise Exception.Create(RPS_TypeMismatch);
                end;
                case var2type.BaseType of
                  btU8: tbts32(var1^) := tbts32(var1^) - tbtu8(var2^);
                  btS8: tbts32(var1^) := tbts32(var1^) - tbts8(var2^);
                  btU16: tbts32(var1^) := tbts32(var1^) - tbtu16(var2^);
                  btS16: tbts32(var1^) := tbts32(var1^) - tbts16(var2^);
                  btU32: tbts32(var1^) := tbts32(var1^) - Longint(tbtu32(var2^));
                  btS32: tbts32(var1^) := tbts32(var1^) - tbts32(var2^);
                  {$IFNDEF PS_NOINT64}
                  btS64: tbts32(var1^) := tbts32(var1^) - tbts64(var2^);
                  {$ENDIF !PS_NOINT64}
                  btChar: tbts32(var1^) := tbts32(var1^) -  Ord(tbtchar(var2^));
                  {$IFNDEF PS_NOWIDESTRING}
                  btWideChar: tbts32(var1^) := tbts32(var1^) - Ord(TbtWideChar(var2^));
                  {$ENDIF !PS_NOWIDESTRING}
                  btVariant: tbts32(var1^) := tbts32(var1^) - Variant(var2^);
                  else raise Exception.Create(RPS_TypeMismatch);
                end;
              end;
            {$IFNDEF PS_NOINT64}
            btS64: tbts64(var1^) := tbts64(var1^) - PSGetInt64(var2, var2type);
            {$ENDIF !PS_NOINT64}
            btSingle:
              begin
                if var2type.BaseType = btPointer then
                begin
                  var2type := PIFTypeRec(Pointer(IPointer(var2)+PointerSize)^);
                  var2 := Pointer(var2^);
                  if (var2 = nil) or (var2type = nil) then
                    raise Exception.Create(RPS_TypeMismatch);
                end;
                case var2type.BaseType of
                  btU8: tbtsingle(var1^) := tbtsingle(var1^) - tbtu8(var2^);
                  btS8: tbtsingle(var1^) := tbtsingle(var1^) - tbts8(var2^);
                  btU16: tbtsingle(var1^) := tbtsingle(var1^) - tbtu16(var2^);
                  btS16: tbtsingle(var1^) := tbtsingle(var1^) - tbts16(var2^);
                  btU32: tbtsingle(var1^) := tbtsingle(var1^) - tbtu32(var2^);
                  btS32: tbtsingle(var1^) := tbtsingle(var1^) - tbts32(var2^);
                  {$IFNDEF PS_NOINT64}
                  btS64: tbtsingle(var1^) := tbtsingle(var1^) - tbts64(var2^);
                  {$ENDIF !PS_NOINT64}
                  btSingle: tbtsingle(var1^) := tbtsingle(var1^) - tbtsingle(var2^);
                  btDouble: tbtsingle(var1^) := tbtsingle(var1^) - tbtdouble(var2^);
                  btExtended: tbtsingle(var1^) := tbtsingle(var1^) - tbtextended(var2^);
                  btCurrency: tbtsingle(var1^) := tbtsingle(var1^) - tbtcurrency(var2^);
                  btVariant:  tbtsingle(var1^) := tbtsingle(var1^) - Variant(var2^);
                  else raise Exception.Create(RPS_TypeMismatch);
                end;
              end;
            btCurrency:
              begin
                if var2type.BaseType = btPointer then
                begin
                  var2type := PIFTypeRec(Pointer(IPointer(var2)+PointerSize)^);
                  var2 := Pointer(var2^);
                  if (var2 = nil) or (var2type = nil) then
                    raise Exception.Create(RPS_TypeMismatch);
                end;
                case var2type.BaseType of
                  btU8: tbtcurrency(var1^) := tbtcurrency(var1^) - tbtu8(var2^);
                  btS8: tbtcurrency(var1^) := tbtcurrency(var1^) - tbts8(var2^);
                  btU16: tbtcurrency(var1^) := tbtcurrency(var1^) - tbtu16(var2^);
                  btS16: tbtcurrency(var1^) := tbtcurrency(var1^) - tbts16(var2^);
                  btU32: tbtcurrency(var1^) := tbtdouble(var1^) - tbtu32(var2^);
                  btS32: tbtcurrency(var1^) := tbtcurrency(var1^) - tbts32(var2^);
                  {$IFNDEF PS_NOINT64}
                  btS64: tbtcurrency(var1^) := tbtdouble(var1^) - tbts64(var2^);
                  {$ENDIF !PS_NOINT64}
                  btSingle: tbtcurrency(var1^) := tbtcurrency(var1^) - tbtsingle(var2^);
                  btDouble: tbtcurrency(var1^) := tbtcurrency(var1^) - tbtdouble(var2^);
                  btExtended: tbtcurrency(var1^) := tbtcurrency(var1^) - tbtextended(var2^);
                  btCurrency: tbtcurrency(var1^) := tbtcurrency(var1^) - tbtcurrency(var2^);
                  btVariant:  tbtcurrency(var1^) := tbtcurrency(var1^) -  Variant(var2^);
                  else raise Exception.Create(RPS_TypeMismatch);
                end;
              end;
            btDouble:
              begin
                if var2type.BaseType = btPointer then
                begin
                  var2type := PIFTypeRec(Pointer(IPointer(var2)+PointerSize)^);
                  var2 := Pointer(var2^);
                  if (var2 = nil) or (var2type = nil) then
                    raise Exception.Create(RPS_TypeMismatch);
                end;
                case var2type.BaseType of
                  btU8: tbtdouble(var1^) := tbtdouble(var1^) - tbtu8(var2^);
                  btS8: tbtdouble(var1^) := tbtdouble(var1^) - tbts8(var2^);
                  btU16: tbtdouble(var1^) := tbtdouble(var1^) - tbtu16(var2^);
                  btS16: tbtdouble(var1^) := tbtdouble(var1^) - tbts16(var2^);
                  btU32: tbtdouble(var1^) := tbtdouble(var1^) - tbtu32(var2^);
                  btS32: tbtdouble(var1^) := tbtdouble(var1^) - tbts32(var2^);
                  {$IFNDEF PS_NOINT64}
                  btS64: tbtdouble(var1^) := tbtdouble(var1^) - tbts64(var2^);
                  {$ENDIF !PS_NOINT64}
                  btSingle: tbtdouble(var1^) := tbtdouble(var1^) - tbtsingle(var2^);
                  btDouble: tbtdouble(var1^) := tbtdouble(var1^) - tbtdouble(var2^);
                  btExtended: tbtdouble(var1^) := tbtdouble(var1^) - tbtextended(var2^);
                  btCurrency: tbtdouble(var1^) := tbtdouble(var1^) - tbtcurrency(var2^);
                  btVariant:  tbtdouble(var1^) := tbtdouble(var1^) -  Variant(var2^);
                  else raise Exception.Create(RPS_TypeMismatch);
                end;
              end;
            btExtended:
              begin
                if var2type.BaseType = btPointer then
                begin
                  var2type := PIFTypeRec(Pointer(IPointer(var2)+PointerSize)^);
                  var2 := Pointer(var2^);
                  if (var2 = nil) or (var2type = nil) then
                    raise Exception.Create(RPS_TypeMismatch);
                end;
                case var2type.BaseType of
                  btU8: tbtextended(var1^) := tbtextended(var1^) - tbtu8(var2^);
                  btS8: tbtextended(var1^) := tbtextended(var1^) - tbts8(var2^);
                  btU16: tbtextended(var1^) := tbtextended(var1^) - tbtu16(var2^);
                  btS16: tbtextended(var1^) := tbtextended(var1^) - tbts16(var2^);
                  btU32: tbtextended(var1^) := tbtextended(var1^) - tbtu32(var2^);
                  btS32: tbtextended(var1^) := tbtextended(var1^) - tbts32(var2^);
                  {$IFNDEF PS_NOINT64}
                  btS64: tbtextended(var1^) := tbtextended(var1^) -+tbts64(var2^);
                  {$ENDIF !PS_NOINT64}
                  btSingle: tbtextended(var1^) := tbtextended(var1^) - tbtsingle(var2^);
                  btDouble: tbtextended(var1^) := tbtextended(var1^) - tbtdouble(var2^);
                  btExtended: tbtextended(var1^) := tbtextended(var1^) - tbtextended(var2^);
                  btCurrency: tbtextended(var1^) := tbtextended(var1^) - tbtcurrency(var2^);
                  btVariant:  tbtextended(var1^) := tbtextended(var1^) -  Variant(var2^);
                  else raise Exception.Create(RPS_TypeMismatch);
                end;
              end;
            btChar: tbtchar(var1^):= tbtchar(ord(tbtchar(var1^)) - PSGetUInt(Var2, var2type));
            {$IFNDEF PS_NOWIDESTRING}
            btWideChar: TbtWideChar(var1^) := widechar(ord(TbtWideChar(var1^)) - PSGetUInt(Var2, var2type));
            {$ENDIF !PS_NOWIDESTRING}
            btVariant:
              begin
                if not IntPIFVariantToVariant(var2, var2type, tvar) then
                begin
                  Result := False;
                end else
                  Variant(var1^) := Variant(var1^) - tvar;
              end;
            btSet:
              begin
                if var1Type = var2Type then
                begin
                  Set_Diff(var1, var2, TPSTypeRec_Set(var1Type).aByteSize);
                end else Result := False;
              end;
          else begin
              CMD_Err2(erTypeMismatch, TbtString(RPS_TypeMismatch));
              Exit;
            end;
          end;
          if not Result then begin
            CMD_Err2(erTypeMismatch, TbtString(RPS_TypeMismatch));
            Exit;
          end;
        end;
      2: begin { * }
          case var1Type.BaseType of
            btU8: tbtU8(var1^) := tbtU8(var1^) * PSGetUInt(Var2, var2type);
            btS8: tbts8(var1^) := tbts8(var1^) * PSGetInt(Var2, var2type);
            btU16: tbtU16(var1^) := tbtU16(var1^) * PSGetUInt(Var2, var2type);
            btS16: tbts16(var1^) := tbts16(var1^) * PSGetInt(Var2, var2type);
            btU32:
              begin
                if var2type.BaseType = btPointer then
                begin
                  var2type := PIFTypeRec(Pointer(IPointer(var2)+PointerSize)^);
                  var2 := Pointer(var2^);
                  if (var2 = nil) or (var2type = nil) then
                    raise Exception.Create(RPS_TypeMismatch);
                end;
                case var2type.BaseType of
                  btU8: tbtU32(var1^) := tbtU32(var1^) * tbtu8(var2^);
                  btS8: tbtU32(var1^) := tbtU32(var1^) * cardinal(longint(tbts8(var2^)));
                  btU16: tbtU32(var1^) := tbtU32(var1^) * tbtu16(var2^);
                  btS16: tbtU32(var1^) := tbtU32(var1^) * cardinal(longint(tbts16(var2^)));
                  btU32: tbtU32(var1^) := tbtU32(var1^) * tbtu32(var2^);
                  btS32: tbtU32(var1^) := tbtU32(var1^) * cardinal(tbts32(var2^));
                  {$IFNDEF PS_NOINT64}
                  btS64: tbtU32(var1^) := tbtU32(var1^) * tbts64(var2^);
                  {$ENDIF !PS_NOINT64}
                  btChar: tbtU32(var1^) := tbtU32(var1^) *  Ord(tbtchar(var2^));
                  {$IFNDEF PS_NOWIDESTRING}
                  btWideChar: tbtU32(var1^) := tbtU32(var1^) * Ord(TbtWideChar(var2^));
                  {$ENDIF !PS_NOWIDESTRING}
                  btVariant: tbtU32(var1^) := tbtU32(var1^) * Variant(var2^);
                  else raise Exception.Create(RPS_TypeMismatch);
                end;
              end;
            btS32:
              begin
                if var2type.BaseType = btPointer then
                begin
                  var2type := PIFTypeRec(Pointer(IPointer(var2)+PointerSize)^);
                  var2 := Pointer(var2^);
                  if (var2 = nil) or (var2type = nil) then
                    raise Exception.Create(RPS_TypeMismatch);
                end;
                case var2type.BaseType of
                  btU8: tbts32(var1^) := tbts32(var1^) * tbtu8(var2^);
                  btS8: tbts32(var1^) := tbts32(var1^) * tbts8(var2^);
                  btU16: tbts32(var1^) := tbts32(var1^) * tbtu16(var2^);
                  btS16: tbts32(var1^) := tbts32(var1^) * tbts16(var2^);
                  btU32: tbts32(var1^) := tbts32(var1^) * Longint(tbtu32(var2^));
                  btS32: tbts32(var1^) := tbts32(var1^) * tbts32(var2^);
                  {$IFNDEF PS_NOINT64}
                  btS64: tbts32(var1^) := tbts32(var1^) * tbts64(var2^);
                  {$ENDIF !PS_NOINT64}
                  btChar: tbts32(var1^) := tbts32(var1^) *  Ord(tbtchar(var2^));
                  {$IFNDEF PS_NOWIDESTRING}
                  btWideChar: tbts32(var1^) := tbts32(var1^) * Ord(TbtWideChar(var2^));
                  {$ENDIF !PS_NOWIDESTRING}
                  btVariant: tbts32(var1^) := tbts32(var1^) * Variant(var2^);
                  else raise Exception.Create(RPS_TypeMismatch);
                end;
              end;
           {$IFNDEF PS_NOINT64}
            btS64: tbts64(var1^) := tbts64(var1^) * PSGetInt64(var2, var2type);
           {$ENDIF !PS_NOINT64}
            btCurrency:
              begin
                if var2type.BaseType = btPointer then
                begin
                  var2type := PIFTypeRec(Pointer(IPointer(var2)+PointerSize)^);
                  var2 := Pointer(var2^);
                  if (var2 = nil) or (var2type = nil) then
                    raise Exception.Create(RPS_TypeMismatch);
                end;
                case var2type.BaseType of
                  btU8: tbtcurrency(var1^) := tbtcurrency(var1^) * tbtu8(var2^);
                  btS8: tbtcurrency(var1^) := tbtcurrency(var1^) * tbts8(var2^);
                  btU16: tbtcurrency(var1^) := tbtcurrency(var1^) * tbtu16(var2^);
                  btS16: tbtcurrency(var1^) := tbtcurrency(var1^) * tbts16(var2^);
                  btU32: tbtcurrency(var1^) := tbtdouble(var1^) * tbtu32(var2^);
                  btS32: tbtcurrency(var1^) := tbtcurrency(var1^) * tbts32(var2^);
                  {$IFNDEF PS_NOINT64}
                  btS64: tbtcurrency(var1^) := tbtdouble(var1^) * tbts64(var2^);
                  {$ENDIF !PS_NOINT64}
                  btSingle: tbtcurrency(var1^) := tbtcurrency(var1^) * tbtsingle(var2^);
                  btDouble: tbtcurrency(var1^) := tbtcurrency(var1^) * tbtdouble(var2^);
                  btExtended: tbtcurrency(var1^) := tbtcurrency(var1^) * tbtextended(var2^);
                  btCurrency: tbtcurrency(var1^) := tbtcurrency(var1^) * tbtcurrency(var2^);
                  btVariant:  tbtcurrency(var1^) := tbtcurrency(var1^) *  Variant(var2^);
                  else raise Exception.Create(RPS_TypeMismatch);
                end;
              end;
            btSingle:
              begin
                if var2type.BaseType = btPointer then
                begin
                  var2type := PIFTypeRec(Pointer(IPointer(var2)+PointerSize)^);
                  var2 := Pointer(var2^);
                  if (var2 = nil) or (var2type = nil) then
                    raise Exception.Create(RPS_TypeMismatch);
                end;
                case var2type.BaseType of
                  btU8: tbtsingle(var1^) := tbtsingle(var1^) *tbtu8(var2^);
                  btS8: tbtsingle(var1^) := tbtsingle(var1^) *tbts8(var2^);
                  btU16: tbtsingle(var1^) := tbtsingle(var1^) *tbtu16(var2^);
                  btS16: tbtsingle(var1^) := tbtsingle(var1^) *tbts16(var2^);
                  btU32: tbtsingle(var1^) := tbtsingle(var1^) *tbtu32(var2^);
                  btS32: tbtsingle(var1^) := tbtsingle(var1^) *tbts32(var2^);
                  {$IFNDEF PS_NOINT64}
                  btS64: tbtsingle(var1^) := tbtsingle(var1^) *tbts64(var2^);
                  {$ENDIF !PS_NOINT64}
                  btSingle: tbtsingle(var1^) := tbtsingle(var1^) *tbtsingle(var2^);
                  btDouble: tbtsingle(var1^) := tbtsingle(var1^) *tbtdouble(var2^);
                  btExtended: tbtsingle(var1^) := tbtsingle(var1^) *tbtextended(var2^);
                  btCurrency: tbtsingle(var1^) := tbtsingle(var1^) *tbtcurrency(var2^);
                  btVariant:  tbtsingle(var1^) := tbtsingle(var1^) * Variant(var2^);
                  else raise Exception.Create(RPS_TypeMismatch);
                end;
              end;
            btDouble:
              begin
                if var2type.BaseType = btPointer then
                begin
                  var2type := PIFTypeRec(Pointer(IPointer(var2)+PointerSize)^);
                  var2 := Pointer(var2^);
                  if (var2 = nil) or (var2type = nil) then
                    raise Exception.Create(RPS_TypeMismatch);
                end;
                case var2type.BaseType of
                  btU8: tbtdouble(var1^) := tbtdouble(var1^) *tbtu8(var2^);
                  btS8: tbtdouble(var1^) := tbtdouble(var1^) *tbts8(var2^);
                  btU16: tbtdouble(var1^) := tbtdouble(var1^) *tbtu16(var2^);
                  btS16: tbtdouble(var1^) := tbtdouble(var1^) *tbts16(var2^);
                  btU32: tbtdouble(var1^) := tbtdouble(var1^) *tbtu32(var2^);
                  btS32: tbtdouble(var1^) := tbtdouble(var1^) *tbts32(var2^);
                  {$IFNDEF PS_NOINT64}
                  btS64: tbtdouble(var1^) := tbtdouble(var1^) *tbts64(var2^);
                  {$ENDIF !PS_NOINT64}
                  btSingle: tbtdouble(var1^) := tbtdouble(var1^) *tbtsingle(var2^);
                  btDouble: tbtdouble(var1^) := tbtdouble(var1^) *tbtdouble(var2^);
                  btExtended: tbtdouble(var1^) := tbtdouble(var1^) *tbtextended(var2^);
                  btCurrency: tbtdouble(var1^) := tbtdouble(var1^) *tbtcurrency(var2^);
                  btVariant:  tbtdouble(var1^) := tbtdouble(var1^) * Variant(var2^);
                  else raise Exception.Create(RPS_TypeMismatch);
                end;
              end;
            btExtended:
              begin
                if var2type.BaseType = btPointer then
                begin
                  var2type := PIFTypeRec(Pointer(IPointer(var2)+PointerSize)^);
                  var2 := Pointer(var2^);
                  if (var2 = nil) or (var2type = nil) then
                    raise Exception.Create(RPS_TypeMismatch);
                end;
                case var2type.BaseType of
                  btU8: tbtextended(var1^) := tbtextended(var1^) *tbtu8(var2^);
                  btS8: tbtextended(var1^) := tbtextended(var1^) *tbts8(var2^);
                  btU16: tbtextended(var1^) := tbtextended(var1^) *tbtu16(var2^);
                  btS16: tbtextended(var1^) := tbtextended(var1^) *tbts16(var2^);
                  btU32: tbtextended(var1^) := tbtextended(var1^) *tbtu32(var2^);
                  btS32: tbtextended(var1^) := tbtextended(var1^) *tbts32(var2^);
                  {$IFNDEF PS_NOINT64}
                  btS64: tbtextended(var1^) := tbtextended(var1^) *tbts64(var2^);
                  {$ENDIF !PS_NOINT64}
                  btSingle: tbtextended(var1^) := tbtextended(var1^) *tbtsingle(var2^);
                  btDouble: tbtextended(var1^) := tbtextended(var1^) *tbtdouble(var2^);
                  btExtended: tbtextended(var1^) := tbtextended(var1^) *tbtextended(var2^);
                  btCurrency: tbtextended(var1^) := tbtextended(var1^) *tbtcurrency(var2^);
                  btVariant:  tbtextended(var1^) := tbtextended(var1^) * Variant(var2^);
                  else raise Exception.Create(RPS_TypeMismatch);
                end;
              end;
            btVariant:
              begin
                if not IntPIFVariantToVariant(var2, var2type, tvar) then
                begin
                  Result := False;
                end else
                  Variant(var1^) := Variant(var1^) * tvar;
              end;
            btSet:
              begin
                if var1Type = var2Type then
                begin
                  Set_Intersect(var1, var2, TPSTypeRec_Set(var1Type).aByteSize);
                end else Result := False;
              end;
          else begin
              CMD_Err2(erTypeMismatch, TbtString(RPS_TypeMismatch));
              Exit;
            end;
          end;
          if not Result then begin
            CMD_Err2(erTypeMismatch, TbtString(RPS_TypeMismatch));
            Exit;
          end;
        end;
      3: begin { / }
          case var1Type.BaseType of
            btU8: tbtU8(var1^) := tbtU8(var1^) div PSGetUInt(Var2, var2type);
            btS8: tbts8(var1^) := tbts8(var1^) div PSGetInt(Var2, var2type);
            btU16: tbtU16(var1^) := tbtU16(var1^) div PSGetUInt(Var2, var2type);
            btS16: tbts16(var1^) := tbts16(var1^) div PSGetInt(Var2, var2type);
            btU32:
              begin
                if var2type.BaseType = btPointer then
                begin
                  var2type := PIFTypeRec(Pointer(IPointer(var2)+PointerSize)^);
                  var2 := Pointer(var2^);
                  if (var2 = nil) or (var2type = nil) then
                    raise Exception.Create(RPS_TypeMismatch);
                end;
                case var2type.BaseType of
                  btU8: tbtU32(var1^) := tbtU32(var1^) div tbtu8(var2^);
                  btS8: tbtU32(var1^) := tbtU32(var1^) div cardinal(longint(tbts8(var2^)));
                  btU16: tbtU32(var1^) := tbtU32(var1^) div tbtu16(var2^);
                  btS16: tbtU32(var1^) := tbtU32(var1^) div cardinal(longint(tbts16(var2^)));
                  btU32: tbtU32(var1^) := tbtU32(var1^) div tbtu32(var2^);
                  btS32: tbtU32(var1^) := tbtU32(var1^) div cardinal(tbts32(var2^));
                  {$IFNDEF PS_NOINT64}
                  btS64: tbtU32(var1^) := tbtU32(var1^) div tbts64(var2^);
                  {$ENDIF !PS_NOINT64}
                  btChar: tbtU32(var1^) := tbtU32(var1^) div  Ord(tbtchar(var2^));
                  {$IFNDEF PS_NOWIDESTRING}
                  btWideChar: tbtU32(var1^) := tbtU32(var1^) div Ord(TbtWideChar(var2^));
                  {$ENDIF ! PS_NOWIDESTRING}
                  btVariant: tbtU32(var1^) := tbtU32(var1^) div Variant(var2^);
                  else raise Exception.Create(RPS_TypeMismatch);
                end;
              end;
            btS32:
              begin
                if var2type.BaseType = btPointer then
                begin
                  var2type := PIFTypeRec(Pointer(IPointer(var2)+PointerSize)^);
                  var2 := Pointer(var2^);
                  if (var2 = nil) or (var2type = nil) then
                    raise Exception.Create(RPS_TypeMismatch);
                end;
                case var2type.BaseType of
                  btU8: tbts32(var1^) := tbts32(var1^) div tbtu8(var2^);
                  btS8: tbts32(var1^) := tbts32(var1^) div tbts8(var2^);
                  btU16: tbts32(var1^) := tbts32(var1^) div tbtu16(var2^);
                  btS16: tbts32(var1^) := tbts32(var1^) div tbts16(var2^);
                  btU32: tbts32(var1^) := tbts32(var1^) div Longint(tbtu32(var2^));
                  btS32: tbts32(var1^) := tbts32(var1^) div tbts32(var2^);
                  {$IFNDEF PS_NOINT64}
                  btS64: tbts32(var1^) := tbts32(var1^) div tbts64(var2^);
                  {$ENDIF !PS_NOINT64}
                  btChar: tbts32(var1^) := tbts32(var1^) div  Ord(tbtchar(var2^));
                  {$IFNDEF PS_NOWIDESTRING}
                  btWideChar: tbts32(var1^) := tbts32(var1^) div Ord(TbtWideChar(var2^));
                  {$ENDIF ! PS_NOWIDESTRING}
                  btVariant: tbts32(var1^) := tbts32(var1^) div Variant(var2^);
                  else raise Exception.Create(RPS_TypeMismatch);
                end;
              end;
            {$IFNDEF PS_NOINT64}
            btS64: tbts64(var1^) := tbts64(var1^) div PSGetInt64(var2, var2type);
            {$ENDIF}
            btSingle:
              begin
                if var2type.BaseType = btPointer then
                begin
                  var2type := PIFTypeRec(Pointer(IPointer(var2)+PointerSize)^);
                  var2 := Pointer(var2^);
                  if (var2 = nil) or (var2type = nil) then
                    raise Exception.Create(RPS_TypeMismatch);
                end;
                case var2type.BaseType of
                  btU8: tbtsingle(var1^) := tbtsingle(var1^) / tbtu8(var2^);
                  btS8: tbtsingle(var1^) := tbtsingle(var1^) / tbts8(var2^);
                  btU16: tbtsingle(var1^) := tbtsingle(var1^) / tbtu16(var2^);
                  btS16: tbtsingle(var1^) := tbtsingle(var1^) / tbts16(var2^);
                  btU32: tbtsingle(var1^) := tbtsingle(var1^) / tbtu32(var2^);
                  btS32: tbtsingle(var1^) := tbtsingle(var1^) / tbts32(var2^);
                  {$IFNDEF PS_NOINT64}
                  btS64: tbtsingle(var1^) := tbtsingle(var1^) / tbts64(var2^);
                  {$ENDIF !PS_NOINT64}
                  btSingle: tbtsingle(var1^) := tbtsingle(var1^) / tbtsingle(var2^);
                  btDouble: tbtsingle(var1^) := tbtsingle(var1^) / tbtdouble(var2^);
                  btExtended: tbtsingle(var1^) := tbtsingle(var1^) / tbtextended(var2^);
                  btCurrency: tbtsingle(var1^) := tbtsingle(var1^) / tbtcurrency(var2^);
                  btVariant:  tbtsingle(var1^) := tbtsingle(var1^) /  Variant(var2^);
                  else raise Exception.Create(RPS_TypeMismatch);
                end;
              end;
            btCurrency:
              begin
                if var2type.BaseType = btPointer then
                begin
                  var2type := PIFTypeRec(Pointer(IPointer(var2)+PointerSize)^);
                  var2 := Pointer(var2^);
                  if (var2 = nil) or (var2type = nil) then
                    raise Exception.Create(RPS_TypeMismatch);
                end;
                case var2type.BaseType of
                  btU8:
                    TbtCurrency(var1^) := TbtCurrency(var1^) / TbtU8(var2^);
                  btS8:
                    TbtCurrency(var1^) := TbtCurrency(var1^) / TbtS8(var2^);
                  btU16:
                    TbtCurrency(var1^) := TbtCurrency(var1^) / TbtU16(var2^);
                  btS16:
                    TbtCurrency(var1^) := TbtCurrency(var1^) / TbtS16(var2^);
                  btU32:
                    TbtCurrency(var1^) := TbtDouble(var1^) / TbtU32(var2^);
                  btS32:
                    TbtCurrency(var1^) := TbtCurrency(var1^) / TbtS32(var2^);
                  {$IFNDEF PS_NOINT64}
                  btS64:
                    TbtCurrency(var1^) := TbtDouble(var1^) / TbtS64(var2^);
                  {$ENDIF !PS_NOINT64}
                  btSingle:
                    TbtCurrency(var1^) := TbtCurrency(var1^) / TbtSingle(var2^);
                  btDouble:
                    TbtCurrency(var1^) := TbtCurrency(var1^) / TbtDouble(var2^);
                  btExtended:
                    TbtCurrency(var1^) := TbtCurrency(var1^) / TbtExtended(var2^);
                  btCurrency:
                    TbtCurrency(var1^) := TbtCurrency(var1^) / TbtCurrency(var2^);
                  btVariant:
                    TbtCurrency(var1^) := TbtCurrency(var1^) / Variant(var2^);
                  else
                    raise Exception.Create(RPS_TypeMismatch);
                end;
              end;
            btDouble:
              begin
                if var2type.BaseType = btPointer then
                begin
                  var2type := PIFTypeRec(Pointer(IPointer(var2)+PointerSize)^);
                  var2 := Pointer(var2^);
                  if (var2 = nil) or (var2type = nil) then
                    raise Exception.Create(RPS_TypeMismatch);
                end;
                case var2type.BaseType of
                  btU8: tbtdouble(var1^) := tbtdouble(var1^) / tbtu8(var2^);
                  btS8: tbtdouble(var1^) := tbtdouble(var1^) / tbts8(var2^);
                  btU16: tbtdouble(var1^) := tbtdouble(var1^) / tbtu16(var2^);
                  btS16: tbtdouble(var1^) := tbtdouble(var1^) / tbts16(var2^);
                  btU32: tbtdouble(var1^) := tbtdouble(var1^) / tbtu32(var2^);
                  btS32: tbtdouble(var1^) := tbtdouble(var1^) / tbts32(var2^);
                  {$IFNDEF PS_NOINT64}
                  btS64: tbtdouble(var1^) := tbtdouble(var1^) / tbts64(var2^);
                  {$ENDIF !PS_NOINT64}
                  btSingle: tbtdouble(var1^) := tbtdouble(var1^) / tbtsingle(var2^);
                  btDouble: tbtdouble(var1^) := tbtdouble(var1^) / tbtdouble(var2^);
                  btExtended: tbtdouble(var1^) := tbtdouble(var1^) / tbtextended(var2^);
                  btCurrency: tbtdouble(var1^) := tbtdouble(var1^) / tbtcurrency(var2^);
                  btVariant:  tbtdouble(var1^) := tbtdouble(var1^) /  Variant(var2^);
                  else raise Exception.Create(RPS_TypeMismatch);
                end;
              end;
            btExtended:
              begin
                if var2type.BaseType = btPointer then
                begin
                  var2type := PIFTypeRec(Pointer(IPointer(var2)+PointerSize)^);
                  var2 := Pointer(var2^);
                  if (var2 = nil) or (var2type = nil) then
                    raise Exception.Create(RPS_TypeMismatch);
                end;
                case var2type.BaseType of
                  btU8: tbtextended(var1^) := tbtextended(var1^) / tbtu8(var2^);
                  btS8: tbtextended(var1^) := tbtextended(var1^) / tbts8(var2^);
                  btU16: tbtextended(var1^) := tbtextended(var1^) / tbtu16(var2^);
                  btS16: tbtextended(var1^) := tbtextended(var1^) / tbts16(var2^);
                  btU32: tbtextended(var1^) := tbtextended(var1^) / tbtu32(var2^);
                  btS32: tbtextended(var1^) := tbtextended(var1^) / tbts32(var2^);
                  {$IFNDEF PS_NOINT64}
                  btS64: tbtextended(var1^) := tbtextended(var1^) / tbts64(var2^);
                  {$ENDIF !PS_NOINT64}
                  btSingle: tbtextended(var1^) := tbtextended(var1^) / tbtsingle(var2^);
                  btDouble: tbtextended(var1^) := tbtextended(var1^) / tbtdouble(var2^);
                  btExtended: tbtextended(var1^) := tbtextended(var1^) / tbtextended(var2^);
                  btCurrency: tbtextended(var1^) := tbtextended(var1^) / tbtcurrency(var2^);
                  btVariant:  tbtextended(var1^) := tbtextended(var1^) /  Variant(var2^);
                  else raise Exception.Create(RPS_TypeMismatch);
                end;
              end;
            btVariant:
              begin
                if not IntPIFVariantToVariant(var2, var2type, tvar) then
                begin
                  Result := False;
                end else
                begin
                  if VarIsFloat(variant(var1^)) then
                    Variant(var1^) := Variant(var1^) / tvar
                  else
                    Variant(var1^) := Variant(var1^) div tvar;
                end;
              end;
          else begin
              CMD_Err2(erTypeMismatch, TbtString(RPS_TypeMismatch));
              Exit;
            end;
          end;
          if not Result then begin
            CMD_Err2(erTypeMismatch, TbtString(RPS_TypeMismatch));
            Exit;
          end;
        end;
      4: begin { MOD }
          case var1Type.BaseType of
            btU8: tbtU8(var1^) := tbtU8(var1^) mod PSGetUInt(Var2, var2type);
            btS8: tbts8(var1^) := tbts8(var1^) mod PSGetInt(Var2, var2type);
            btU16: tbtU16(var1^) := tbtU16(var1^) mod PSGetUInt(Var2, var2type);
            btS16: tbts16(var1^) := tbts16(var1^) mod PSGetInt(Var2, var2type);
            btU32:
              begin
                if var2type.BaseType = btPointer then
                begin
                  var2type := PIFTypeRec(Pointer(IPointer(var2)+PointerSize)^);
                  var2 := Pointer(var2^);
                  if (var2 = nil) or (var2type = nil) then
                    raise Exception.Create(RPS_TypeMismatch);
                end;
                case var2type.BaseType of
                  btU8: tbtU32(var1^) := tbtU32(var1^) mod tbtu8(var2^);
                  btS8: tbtU32(var1^) := tbtU32(var1^) mod cardinal(longint(tbts8(var2^)));
                  btU16: tbtU32(var1^) := tbtU32(var1^) mod tbtu16(var2^);
                  btS16: tbtU32(var1^) := tbtU32(var1^) mod cardinal(longint(tbts16(var2^)));
                  btU32: tbtU32(var1^) := tbtU32(var1^) mod tbtu32(var2^);
                  btS32: tbtU32(var1^) := tbtU32(var1^) mod cardinal(tbts32(var2^));
                  {$IFNDEF PS_NOINT64}
                  btS64: tbtU32(var1^) := tbtU32(var1^) mod tbts64(var2^);
                  {$ENDIF !PS_NOINT64}
                  btChar: tbtU32(var1^) := tbtU32(var1^) mod  Ord(tbtchar(var2^));
                  {$IFNDEF PS_NOWIDESTRING}
                  btWideChar: tbtU32(var1^) := tbtU32(var1^) mod Ord(TbtWideChar(var2^));
                  {$ENDIF !PS_NOWIDESTRING}
                  btVariant: tbtU32(var1^) := tbtU32(var1^) mod Variant(var2^);
                  else raise Exception.Create(RPS_TypeMismatch);
                end;
              end;
            btS32:
              begin
                if var2type.BaseType = btPointer then
                begin
                  var2type := PIFTypeRec(Pointer(IPointer(var2)+PointerSize)^);
                  var2 := Pointer(var2^);
                  if (var2 = nil) or (var2type = nil) then
                    raise Exception.Create(RPS_TypeMismatch);
                end;
                case var2type.BaseType of
                  btU8: tbts32(var1^) := tbts32(var1^) mod tbtu8(var2^);
                  btS8: tbts32(var1^) := tbts32(var1^) mod tbts8(var2^);
                  btU16: tbts32(var1^) := tbts32(var1^) mod tbtu16(var2^);
                  btS16: tbts32(var1^) := tbts32(var1^) mod tbts16(var2^);
                  btU32: tbts32(var1^) := tbts32(var1^) mod Longint(tbtu32(var2^));
                  btS32: tbts32(var1^) := tbts32(var1^) mod tbts32(var2^);
              {$IFNDEF PS_NOINT64} btS64: tbts32(var1^) := tbts32(var1^) mod tbts64(var2^);{$ENDIF}
                  btChar: tbts32(var1^) := tbts32(var1^) mod  Ord(tbtchar(var2^));
              {$IFNDEF PS_NOWIDESTRING}    btWideChar: tbts32(var1^) := tbts32(var1^) mod Ord(TbtWideChar(var2^));{$ENDIF}
                  btVariant: tbts32(var1^) := tbts32(var1^) mod Variant(var2^);
                  else raise Exception.Create(RPS_TypeMismatch);
                end;
              end;
           {$IFNDEF PS_NOINT64}
            btS64: tbts64(var1^) := tbts64(var1^) mod PSGetInt64(var2, var2type);
           {$ENDIF}
            btVariant:
              begin
                if not IntPIFVariantToVariant(var2, var2type, tvar) then
                begin
                  Result := False;
                end else
                  Variant(var1^) := Variant(var1^) mod tvar;
              end;
          else begin
              CMD_Err2(erTypeMismatch, TbtString(RPS_TypeMismatch));
              Exit;
            end;
          end;
          if not Result then begin
            CMD_Err2(erTypeMismatch, TbtString(RPS_TypeMismatch));
            Exit;
          end;
        end;
      5: begin { SHL }
          case var1Type.BaseType of
            btU8: tbtU8(var1^) := tbtU8(var1^) shl PSGetUInt(Var2, var2type);
            btS8: tbts8(var1^) := tbts8(var1^) shl PSGetInt(Var2, var2type);
            btU16: tbtU16(var1^) := tbtU16(var1^) shl PSGetUInt(Var2, var2type);
            btS16: tbts16(var1^) := tbts16(var1^) shl PSGetInt(Var2, var2type);
            btU32: tbtU32(var1^) := tbtU32(var1^) shl PSGetUInt(Var2, var2type);
            btS32: tbts32(var1^) := tbts32(var1^) shl PSGetInt(Var2, var2type);
           {$IFNDEF PS_NOINT64}
            btS64: tbts64(var1^) := tbts64(var1^) shl PSGetInt64(var2, var2type);
           {$ENDIF}
            btVariant:
              begin
                if not IntPIFVariantToVariant(var2, var2type, tvar) then
                begin
                  Result := False;
                end else
                  Variant(var1^) := Variant(var1^) shl tvar;
              end;
          else begin
              CMD_Err2(erTypeMismatch, TbtString(RPS_TypeMismatch));
              Exit;
            end;
          end;
          if not Result then begin
            CMD_Err2(erTypeMismatch, TbtString(RPS_TypeMismatch));
            Exit;
          end;
        end;
      6: begin { SHR }
          case var1Type.BaseType of
            btU8: tbtU8(var1^) := tbtU8(var1^) shr PSGetUInt(Var2, var2type);
            btS8: tbts8(var1^) := tbts8(var1^) shr PSGetInt(Var2, var2type);
            btU16: tbtU16(var1^) := tbtU16(var1^) shr PSGetUInt(Var2, var2type);
            btS16: tbts16(var1^) := tbts16(var1^) shr PSGetInt(Var2, var2type);
            btU32: tbtU32(var1^) := tbtU32(var1^) shr PSGetUInt(Var2, var2type);
            btS32: tbts32(var1^) := tbts32(var1^) shr PSGetInt(Var2, var2type);
           {$IFNDEF PS_NOINT64}
            btS64: tbts64(var1^) := tbts64(var1^) shr PSGetInt64(var2, var2type);
           {$ENDIF}
            btVariant:
              begin
                if not IntPIFVariantToVariant(var2, var2type, tvar) then
                begin
                  Result := False;
                end else
                  Variant(var1^) := Variant(var1^) shr tvar;
              end;
          else begin
              CMD_Err2(erTypeMismatch, TbtString(RPS_TypeMismatch));
              Exit;
            end;
          end;
          if not Result then begin
            CMD_Err2(erTypeMismatch, TbtString(RPS_TypeMismatch));
            Exit;
          end;
        end;
      7: begin { AND }
          case var1Type.BaseType of
            btU8: tbtU8(var1^) := tbtU8(var1^) and PSGetUInt(Var2, var2type);
            btS8: tbts8(var1^) := tbts8(var1^) and PSGetInt(Var2, var2type);
            btU16: tbtU16(var1^) := tbtU16(var1^) and PSGetUInt(Var2, var2type);
            btS16: tbts16(var1^) := tbts16(var1^) and PSGetInt(Var2, var2type);
            btU32: tbtU32(var1^) := tbtU32(var1^) and PSGetUInt(Var2, var2type);
            btS32: tbts32(var1^) := tbts32(var1^) and PSGetInt(Var2, var2type);
           {$IFNDEF PS_NOINT64}
            btS64: tbts64(var1^) := tbts64(var1^) and PSGetInt64(var2, var2type);
           {$ENDIF}
            btVariant:
              begin
                if not IntPIFVariantToVariant(var2, var2type, tvar) then
                begin
                  Result := False;
                end else
                  Variant(var1^) := Variant(var1^) and tvar;
              end;
          else begin
              CMD_Err2(erTypeMismatch, TbtString(RPS_TypeMismatch));
              Exit;
            end;
          end;
          if not Result then begin
            CMD_Err2(erTypeMismatch, TbtString(RPS_TypeMismatch));
            Exit;
          end;
        end;
      8: begin { OR }
          case var1Type.BaseType of
            btU8: tbtU8(var1^) := tbtU8(var1^) or PSGetUInt(Var2, var2type);
            btS8: tbts8(var1^) := tbts8(var1^) or PSGetInt(Var2, var2type);
            btU16: tbtU16(var1^) := tbtU16(var1^) or PSGetUInt(Var2, var2type);
            btS16: tbts16(var1^) := tbts16(var1^) or PSGetInt(Var2, var2type);
            btU32: tbtU32(var1^) := tbtU32(var1^) or PSGetUInt(Var2, var2type);
            btS32: tbts32(var1^) := tbts32(var1^) or PSGetInt(Var2, var2type);
           {$IFNDEF PS_NOINT64}
            btS64: tbts64(var1^) := tbts64(var1^) or PSGetInt64(var2, var2type);
           {$ENDIF}
            btVariant:
              begin
                if not IntPIFVariantToVariant(var2, var2type, tvar) then
                begin
                  Result := False;
                end else
                  Variant(var1^) := Variant(var1^) or tvar;
              end;
          else begin
              CMD_Err2(erTypeMismatch, TbtString(RPS_TypeMismatch));
              Exit;
            end;
          end;
          if not Result then begin
            CMD_Err2(erTypeMismatch, TbtString(RPS_TypeMismatch));
            Exit;
          end;
        end;
      9: begin { XOR }
          case var1Type.BaseType of
            btU8: tbtU8(var1^) := tbtU8(var1^) xor PSGetUInt(Var2, var2type);
            btS8: tbts8(var1^) := tbts8(var1^) xor PSGetInt(Var2, var2type);
            btU16: tbtU16(var1^) := tbtU16(var1^) xor PSGetUInt(Var2, var2type);
            btS16: tbts16(var1^) := tbts16(var1^) xor PSGetInt(Var2, var2type);
            btU32: tbtU32(var1^) := tbtU32(var1^) xor PSGetUInt(Var2, var2type);
            btS32: tbts32(var1^) := tbts32(var1^) xor PSGetInt(Var2, var2type);
           {$IFNDEF PS_NOINT64}
            btS64: tbts64(var1^) := tbts64(var1^) xor PSGetInt64(var2, var2type);
           {$ENDIF}
            btVariant:
              begin
                if not IntPIFVariantToVariant(var2, var2type, tvar) then
                begin
                  Result := False;
                end else
                  Variant(var1^) := Variant(var1^) xor tvar;
              end;
          else begin
              CMD_Err2(erTypeMismatch, TbtString(RPS_TypeMismatch));
              Exit;
            end;
          end;
          if not Result then begin
            CMD_Err2(erTypeMismatch, TbtString(RPS_TypeMismatch));
            Exit;
          end;
        end;
      10:
        begin // as
          case var1Type.BaseType of
            btClass:
              begin
                if var2type.BaseType <> btU32 then
                  Result := False
                else
                begin
                  var2type := FTypes[tbtu32(var2^)];
                  if (var2type = nil) or (var2type.BaseType <> btClass) then
                    Result := False
                  else
                  begin
                    if not Class_IS(Self, TObject(var1^), var2type) then
                      Result := False
                  end;
                end;
              end;
          else begin
              CMD_Err2(erTypeMismatch, TbtString(RPS_TypeMismatch));
              Exit;
            end;
          end;
          if not Result then begin
            CMD_Err2(erTypeMismatch, TbtString(RPS_TypeMismatch));
            Exit;
          end;
        end;
    else begin
        Result := False;
        CMD_Err2(erInvalidOpcodeParameter, TbtString(RPS_InvalidOpcodeParameter));
        Exit;
      end;
    end;
  except
    {$IFDEF DELPHI6UP}
    Tmp := AcquireExceptionObject;
    {$ELSE}
    if RaiseList <> nil then
    begin
      Tmp := Exception(PRaiseFrame(RaiseList)^.ExceptObject);
      PRaiseFrame(RaiseList)^.ExceptObject := nil;
    end else
      Tmp := nil;
    {$ENDIF !DELPHI6UP}
    if Tmp <> nil then
    begin
      if Tmp is EPSException then
      begin
        Result := False;
        ExceptionProc(EPSException(tmp).ProcNo, EPSException(tmp).ProcPos, erCustomError, TbtString(EPSException(tmp).Message), nil);
        Exit;
      end else
      if Tmp is EDivByZero then
      begin
        Result := False;
        CMD_Err3(erDivideByZero, TbtString(Exception(Tmp).Message), Tmp);
        Exit;
      end;
      if Tmp is EZeroDivide then
      begin
        Result := False;
        CMD_Err3(erDivideByZero, TbtString(Exception(Tmp).Message), Tmp);
        Exit;
      end;
      if Tmp is EMathError then
      begin
        Result := False;
        CMD_Err3(erMathError,TbtString(Exception(Tmp).Message), Tmp);
        Exit;
      end;
    end;
    if (tmp <> nil) and (Tmp is {+}ExceptionBase{+.}) then
      CMD_Err3(erException, TbtString(Exception(Tmp).Message), Tmp)
    else
      CMD_Err3(erException, '', Tmp);
    Result := False;
  end;
end;

function TPSExec.ReadVariable(var Dest: TPSResultData; UsePointer: Boolean): Boolean;
var
  VarType: Cardinal;
  Param: Cardinal;
  Tmp: PIfVariant;
  at: TPSTypeRec;
begin
  if FCurrentPosition + 4 >= FDataLength then
  begin
    CMD_Err2(erOutOfRange, TbtString(RPS_OutOfRange)); // Error
    Result := False;
    Exit;
  end;
  VarType := FData[FCurrentPosition];
  Inc(FCurrentPosition); {+}{@dbg@:hook.parser}{+.} // dbg.cond: vartype=3
  Param := {$ifdef FPC_REQUIRES_PROPER_ALIGNMENT}unaligned{$endif}(
    Cardinal((@FData[FCurrentPosition])^)
  );
  Inc(FCurrentPosition, 4);
  case VarType of
    0:
      begin // @dbg: Dest.aType,r
        Dest.FreeType := vtNone;
        if Param < PSAddrNegativeStackStart then
        begin
          if Param >= Cardinal(FGlobalVars.Count) then
          begin
            CMD_Err2(erOutOfGlobalVarsRange, TbtString(RPS_OutOfGlobalVarsRange));
            Result := False;
            Exit;
          end;
          Tmp := FGlobalVars.Data[param];
        end else
        begin
          Param := Cardinal(Longint(-PSAddrStackStart) + Longint(FCurrStackBase) + Longint(Param));
          if Param >= Cardinal(FStack.Count) then
          begin
            CMD_Err2(erOutOfStackRange, TbtString(RPS_OutOfStackRange));
            Result := False;
            Exit;
          end;
          Tmp := FStack.Data[param];
        end;
        if (UsePointer) and (Tmp.FType.BaseType = btPointer) then
        begin // @dbg: TObject(PPSVariantPointer(Tmp).DataDest).ClassName  ;  TMBCSEncoding(PPSVariantPointer(Tmp).DataDest),r
          Dest.aType := PPSVariantPointer(Tmp).DestType; // @dbg: PPSVariantPointer(Tmp).VI.FType,r
          Dest.P := PPSVariantPointer(Tmp).DataDest;
          if Dest.P = nil then
          begin
            Cmd_Err2(erNullPointerException, TbtString(RPS_NullPointerException));
            Result := False;
            Exit;
          end;
        end else
        begin
          Dest.aType := PPSVariantData(Tmp).vi.FType; // @dbg: PPSVariantData(Tmp).VI.FType,r
          Dest.P := Pointer(@PPSVariantData(Tmp).Data); // @dbg: TObject(@PPSVariantData(Tmp).Data).ClassName  ; TMBCSEncoding(@PPSVariantData(Tmp).Data),r
        end;
      end;
    1: begin
        if Param >= FTypes.Count then begin
          CMD_Err2(erInvalidType, TbtString(RPS_InvalidType));
          Result := False;
          Exit;
        end;
        at := FTypes.Data^[Param];
        Param := FTempVars.FLength;
        FTempVars.FLength := Cardinal(Longint(Param) + Longint(at.RealSize) + Longint(RTTISize + 3)) and not 3;
        if FTempVars.FLength > FTempVars.FCapacity then
          FtempVars.AdjustLength();
        Tmp := Pointer(IPointer(FtempVars.FDataPtr) + IPointer(Param));

        if Cardinal(FTempVars.FCount) >= Cardinal(FTempVars.FCapacity) then begin
          Inc(FTempVars.FCapacity, FCapacityInc);// := FCount + 1;
          ReAllocMem(FTempVars.FData, FTempVars.FCapacity shl 2);
        end;
        FTempVars.FData[FTempVars.FCount] := Tmp; // Instead of SetItem
        Inc(FTempVars.FCount);
        {$IFNDEF PS_NOSMARTLIST}
        Inc(FTempVars.FCheckCount);
        if FTempVars.FCheckCount > FMaxCheckCount then
          FTempVars.Recreate;
        {$ENDIF !PS_NOSMARTLIST}

        Tmp.FType := at;
        Dest.P := Pointer(@PPSVariantData(Tmp).Data);
        Dest.aType := tmp.FType;
        dest.FreeType := vtTempVar;
        case Dest.aType.BaseType of
          btSet:
            begin
              if not ReadData(Dest.P^, TPSTypeRec_Set(Dest.aType).aByteSize) then begin
                CMD_Err2(erOutOfRange, TbtString(RPS_OutOfRange));
                FTempVars.Pop;
                Result := False;
                Exit;
              end;
            end;
          bts8, btchar, btU8:
            begin
              if FCurrentPosition >= FDataLength then begin
                CMD_Err2(erOutOfRange, TbtString(RPS_OutOfRange));
                FTempVars.Pop;
                Result := False;
                Exit;
              end;
              tbtu8(dest.p^) := FData[FCurrentPosition];
              Inc(FCurrentPosition);
            end;
          bts16, {$IFNDEF PS_NOWIDESTRING}btwidechar,{$ENDIF} btU16:
            begin
              if FCurrentPosition + 1>= FDataLength then
              begin
                CMD_Err2(erOutOfRange, TbtString(RPS_OutOfRange));
                FTempVars.Pop;
                Result := False;
                Exit;
              end;
              tbtu16(dest.p^) := {$ifdef FPC_REQUIRES_PROPER_ALIGNMENT}unaligned{$endif}(
                tbtu16((@FData[FCurrentPosition])^)
              );
              Inc(FCurrentPosition, 2);
            end;
          bts32, btU32:
            begin
              if FCurrentPosition + 3>= FDataLength then
              begin
                CMD_Err2(erOutOfRange, TbtString(RPS_OutOfRange));
                FTempVars.Pop;
                Result := False;
                Exit;
              end;
              tbtu32(dest.p^) := {$ifdef FPC_REQUIRES_PROPER_ALIGNMENT}unaligned{$endif}(
                tbtu32((@FData[FCurrentPosition])^)
              );
              Inc(FCurrentPosition, 4);
            end;
          btProcPtr:
            begin
              if FCurrentPosition + 3>= FDataLength then
              begin
                CMD_Err2(erOutOfRange, TbtString(RPS_OutOfRange));
                FTempVars.Pop;
                Result := False;
                Exit;
              end;
              tbtu32(dest.p^) := {$ifdef FPC_REQUIRES_PROPER_ALIGNMENT}unaligned{$endif}(
                tbtu32((@FData[FCurrentPosition])^)
              );
              tbtu32(Pointer(IPointer(dest.p)+PointerSize)^) := 0;
              tbtu32(Pointer(IPointer(dest.p)+PointerSize)^) := 0;
              Inc(FCurrentPosition, 4);
            end;
          {$IFNDEF PS_NOINT64}
          bts64:
            begin
              if FCurrentPosition + 7>= FDataLength then
              begin
                CMD_Err2(erOutOfRange, TbtString(RPS_OutOfRange));
                FTempVars.Pop;
                Result := False;
                Exit;
              end;
              tbts64(dest.p^) := {$ifdef FPC_REQUIRES_PROPER_ALIGNMENT}unaligned{$endif}(
                tbts64((@FData[FCurrentPosition])^)
              );
              Inc(FCurrentPosition, 8);
            end;
          {$ENDIF !PS_NOINT64}
          btSingle:
            begin
              if FCurrentPosition + (SizeOf(Single)-1)>= FDataLength then
              begin
                CMD_Err2(erOutOfRange, TbtString(RPS_OutOfRange));
                FTempVars.Pop;
                Result := False;
                Exit;
              end;
              TbtSingle(dest.p^) := {$ifdef FPC_REQUIRES_PROPER_ALIGNMENT}unaligned{$endif}(
                TbtSingle((@FData[FCurrentPosition])^)
              );
              Inc(FCurrentPosition, SizeOf(Single));
            end;
          btDouble:
            begin
              if FCurrentPosition + (SizeOf(Double)-1)>= FDataLength then
              begin
                CMD_Err2(erOutOfRange, TbtString(RPS_OutOfRange));
                FTempVars.Pop;
                Result := False;
                Exit;
              end;
              TbtDouble(dest.p^) := {$ifdef FPC_REQUIRES_PROPER_ALIGNMENT}unaligned{$endif}(
                TbtDouble((@FData[FCurrentPosition])^)
              );
              Inc(FCurrentPosition, SizeOf(Double));
            end;

          btExtended:
            begin
              if FCurrentPosition + (SizeOf(Extended)-1)>= FDataLength then
              begin
                CMD_Err2(erOutOfRange, TbtString(RPS_OutOfRange));
                FTempVars.Pop;
                Result := False;
                Exit;
              end;
              TbtExtended(dest.p^) := {$ifdef FPC_REQUIRES_PROPER_ALIGNMENT}unaligned{$endif}(
                TbtExtended((@FData[FCurrentPosition])^)
              );
              Inc(FCurrentPosition, SizeOf(Extended));
            end;
          btPChar, btString:
          begin
              if FCurrentPosition + 3 >= FDataLength then
              begin
                CMD_Err2(erOutOfRange, TbtString(RPS_OutOfRange));
                FTempVars.Pop;
                Result := False;
                Exit;
              end;
              Param := {$ifdef FPC_REQUIRES_PROPER_ALIGNMENT}unaligned{$endif}(
                Cardinal((@FData[FCurrentPosition])^)
              );
              Inc(FCurrentPosition, 4);
              {+}
              //?Dest.aType.Clear;
              {+.}
              Pointer(Dest.P^) := nil;
              SetLength(TbtString(Dest.P^), Param);
              if Param <> 0 then begin
              if not ReadData(TbtString(Dest.P^)[1], Param) then
              begin
                CMD_Err2(erOutOfRange, TbtString(RPS_OutOfRange));
                FTempVars.Pop;
                Result := False;
                Exit;
              end;
                PAnsiChar(Dest.p^)[Param] := #0;
              end;
            end;
          {$IFNDEF PS_NOWIDESTRING}
          {+}{$if declared(btPWideChar)}btPWideChar,{$ifend}{+.}
          btWidestring:
            begin
              if FCurrentPosition + 3 >= FDataLength then
              begin
                CMD_Err2(erOutOfRange, TbtString(RPS_OutOfRange));
                FTempVars.Pop;
                Result := False;
                Exit;
              end;
              Param := {$ifdef FPC_REQUIRES_PROPER_ALIGNMENT}unaligned{$endif}(
                Cardinal((@FData[FCurrentPosition])^)
              );
              Inc(FCurrentPosition, 4);
              {+}
              //?Dest.aType.Clear;
              {+.}
              Pointer(Dest.P^) := nil;
              SetLength(TbtWideString(Dest.P^), Param);
              if not ReadData(TbtWideString(Dest.P^)[1], Param*2) then
              begin
                CMD_Err2(erOutOfRange, TbtString(RPS_OutOfRange));
                FTempVars.Pop;
                Result := False;
                Exit;
              end;
            end;
          btUnicodeString:
            begin
              if FCurrentPosition + 3 >= FDataLength then
              begin
                CMD_Err2(erOutOfRange, TbtString(RPS_OutOfRange));
                FTempVars.Pop;
                Result := False;
                Exit;
              end;
              Param := {$ifdef FPC_REQUIRES_PROPER_ALIGNMENT}unaligned{$endif}(
                Cardinal((@FData[FCurrentPosition])^)
              );
              Inc(FCurrentPosition, 4);
              Pointer(Dest.P^) := nil;
              SetLength(TbtUnicodeString(Dest.P^), Param);
              if not ReadData(TbtUnicodeString(Dest.P^)[1], Param*2) then
              begin
                CMD_Err2(erOutOfRange, TbtString(RPS_OutOfRange));
                FTempVars.Pop;
                Result := False;
                Exit;
              end;
            end;
          {$ENDIF !PS_NOWIDESTRING}
        else begin
            CMD_Err2(erInvalidType, TbtString(RPS_InvalidType));
            FTempVars.Pop;
            Result := False;
            Exit;
          end;
        end;
      end;
    2:
      begin
        Dest.FreeType := vtNone;
        if Param < PSAddrNegativeStackStart then begin
          if Param >= Cardinal(FGlobalVars.Count) then
          begin
            CMD_Err2(erOutOfGlobalVarsRange, TbtString(RPS_OutOfGlobalVarsRange));
            Result := False;
            Exit;
          end;
          Tmp := FGlobalVars.Data[param];
        end
        else begin
          Param := Cardinal(Longint(-PSAddrStackStart) + Longint(FCurrStackBase) + Longint(Param));
          if Param >= Cardinal(FStack.Count) then
          begin
            CMD_Err2(erOutOfStackRange, TbtString(RPS_OutOfStackRange));
            Result := False;
            Exit;
          end;
          Tmp := FStack.Data[param];
        end;
        if Tmp.FType.BaseType = btPointer then
        begin
          Dest.aType := PPSVariantPointer(Tmp).DestType;
          Dest.P := PPSVariantPointer(Tmp).DataDest;
          if Dest.P = nil then
          begin
            CMD_Err2(erNullPointerException, TbtString(RPS_NullPointerException));
            Result := False;
            Exit;
          end;
        end else
        begin
          Dest.aType := PPSVariantData(Tmp).vi.FType;
          Dest.P := Pointer(@PPSVariantData(Tmp).Data);
        end;
        if FCurrentPosition + 3 >= FDataLength then
        begin
          CMD_Err2(erOutOfRange, TbtString(RPS_OutOfRange));
          Result := False;
          Exit;
        end;
        Param := {$ifdef FPC_REQUIRES_PROPER_ALIGNMENT}unaligned{$endif}(
          Cardinal((@FData[FCurrentPosition])^)
        );
        Inc(FCurrentPosition, 4);
        case Dest.aType.BaseType of
          btRecord:
            begin
              if Param > Cardinal(TPSTypeRec_Record(Dest.aType).FFieldTypes.Count) then
              begin
                CMD_Err2(erOutOfRange, TbtString(RPS_OutOfRange));
                Result := False;
                Exit;
              end;
              {+}
              //Dest.P := Pointer(IPointer(Dest.P) + IPointer(TPSTypeRec_Record(Dest.aType).RealFieldOffsets[Param]));
              Dest.P := PointerShift(Dest.P, TPSTypeRec_Record(Dest.aType).RealFieldOffsets[Param]);
              {+.}
              Dest.aType := TPSTypeRec_Record(Dest.aType).FieldTypes[Param];
            end;
          btArray:
            begin
              if Param >= Cardinal(PSDynArrayGetLength(Pointer(Dest.P^), dest.aType)) then
              begin
                CMD_Err2(erCustomError,
                  TbtString(Format(RPS_OutOfRangeEx,
                    [Cardinal(PSDynArrayGetLength(Pointer(Dest.P^), dest.aType)),Param])));
                Result := False;
                Exit;
              end;
              {+}
              //Dest.P := Pointer(IPointer(Dest.P^) + (Param * TPSTypeRec_Array(Dest.aType).FArrayType.RealSize));
              Dest.P := PointerShift(Pointer(Dest.P^), Param * TPSTypeRec_Array(Dest.aType).FArrayType.RealSize);
              {+.}
              Dest.aType := TPSTypeRec_Array(dest.aType).ArrayType;
            end;
          btStaticArray:
            begin
              if Param >= Cardinal(TPSTypeRec_StaticArray(Dest.aType).Size) then
              begin
                CMD_Err2(erCustomError,
                  TbtString(Format(RPS_OutOfRangeEx,
                    [Cardinal(TPSTypeRec_StaticArray(Dest.aType).Size),Param])));
                Result := False;
                Exit;
              end;
              {+}
              //Dest.P := Pointer(IPointer(Dest.P) + (Param * TPSTypeRec_Array(Dest.aType).FArrayType.RealSize));
              Dest.P := PointerShift(Dest.P, Param * TPSTypeRec_Array(Dest.aType).FArrayType.RealSize);
              {+.}
              Dest.aType := TPSTypeRec_Array(dest.aType).ArrayType;
            end;
        else
          CMD_Err2(erInvalidType, TbtString(RPS_InvalidType));
          Result := False;
          Exit;
        end;

        if UsePointer and (Dest.aType.BaseType = btPointer) then
        begin
          Dest.aType := TPSTypeRec(Pointer(IPointer(Dest.p)+PointerSize)^);
          Dest.P := Pointer(Dest.p^);
          if Dest.P = nil then
          begin
            CMD_Err2(erNullPointerException, TbtString(RPS_NullPointerException));
            Result := False;
            Exit;
          end;
        end;
      end;
    3:
      begin
        Dest.FreeType := vtNone;
        if Param < PSAddrNegativeStackStart then begin
          if Param >= Cardinal(FGlobalVars.Count) then
          begin
            CMD_Err2(erOutOfGlobalVarsRange, TbtString(RPS_OutOfGlobalVarsRange));
            Result := False;
            Exit;
          end;
          Tmp := FGlobalVars.Data[param];
        end
        else begin
          Param := Cardinal(Longint(-PSAddrStackStart) + Longint(FCurrStackBase) + Longint(Param));
          if Param >= Cardinal(FStack.Count) then
          begin
            CMD_Err2(erOutOfStackRange, TbtString(RPS_OutOfStackRange));
            Result := False;
            Exit;
          end;
          Tmp := FStack.Data[param];
        end;
        if (Tmp.FType.BaseType = btPointer) then
        begin
          Dest.aType := PPSVariantPointer(Tmp).DestType;
          Dest.P := PPSVariantPointer(Tmp).DataDest;
          if Dest.P = nil then
          begin
            CMD_Err2(erNullPointerException, TbtString(RPS_NullPointerException));
            Result := False;
            Exit;
          end;
        end else
        begin
          Dest.aType := PPSVariantData(Tmp).vi.FType;
          Dest.P := Pointer(@PPSVariantData(Tmp).Data);
        end;
        Param := {$ifdef FPC_REQUIRES_PROPER_ALIGNMENT}unaligned{$endif}(
          Cardinal((@FData[FCurrentPosition])^)
        );
        Inc(FCurrentPosition, 4);
        if Param < PSAddrNegativeStackStart then
        begin
          if Param >= Cardinal(FGlobalVars.Count) then
          begin
            CMD_Err2(erOutOfGlobalVarsRange, TbtString(RPS_OutOfGlobalVarsRange));
            Result := False;
            Exit;
          end;
          Tmp := FGlobalVars[Param];
        end
        else begin
          Param := Cardinal(Longint(-PSAddrStackStart) + Longint(FCurrStackBase) + Longint(Param));
          if Cardinal(Param) >= Cardinal(FStack.Count) then
          begin
            CMD_Err2(erOutOfStackRange, TbtString(RPS_OutOfStackRange));
            Result := False;
            Exit;
          end;
          Tmp := FStack[Param];
        end;
        case Tmp.FType.BaseType of
          btu8: Param := PPSVariantU8(Tmp).Data;
          bts8: Param := PPSVariants8(Tmp).Data;
          btu16: Param := PPSVariantU16(Tmp).Data;
          bts16: Param := PPSVariants16(Tmp).Data;
          btu32: Param := PPSVariantU32(Tmp).Data;
          bts32: Param := PPSVariants32(Tmp).Data;
          btPointer:
            begin
              if PPSVariantPointer(tmp).DestType <> nil then
              begin
                case PPSVariantPointer(tmp).DestType.BaseType of
                  btu8: Param := tbtu8(PPSVariantPointer(tmp).DataDest^);
                  bts8: Param := tbts8(PPSVariantPointer(tmp).DataDest^);
                  btu16: Param := tbtu16(PPSVariantPointer(tmp).DataDest^);
                  bts16: Param := tbts16(PPSVariantPointer(tmp).DataDest^);
                  btu32, btProcPtr: Param := tbtu32(PPSVariantPointer(tmp).DataDest^);
                  bts32: Param := tbts32(PPSVariantPointer(tmp).DataDest^);
                  else
                    begin
                      CMD_Err2(erTypeMismatch, TbtString(RPS_TypeMismatch));
                      Result := False;
                      Exit;
                    end;
                end;
              end else
              begin
                CMD_Err2(erTypeMismatch, TbtString(RPS_TypeMismatch));
                Result := False;
                Exit;
              end;
            end;
          {+}
          {$IFNDEF PS_NOINT64}
          btS64: Param := PPSVariantS64(Tmp).Data;
          {$ENDIF}
          {+.}
          else
            CMD_Err2(erTypeMismatch, TbtString(RPS_TypeMismatch));
            Result := False;
            Exit;
        end; // case Tmp.FType.BaseType
        //
        case Dest.aType.BaseType of
          btRecord:
            begin
              if Param > Cardinal(TPSTypeRec_Record(Dest.aType).FFieldTypes.Count) then
              begin
                CMD_Err2(erOutOfRange, TbtString(RPS_OutOfRange));
                Result := False;
                Exit;
              end;
              {+}
              //Dest.P := Pointer(IPointer(Dest.P) + IPointer(TPSTypeRec_Record(Dest.aType).RealFieldOffsets[Param]));
              Dest.P := PointerShift(Dest.P, TPSTypeRec_Record(Dest.aType).RealFieldOffsets[Param]);
              {+.}
              Dest.aType := TPSTypeRec_Record(Dest.aType).FieldTypes[Param];
            end;
          btArray:
            begin
              if Cardinal(Param) >= Cardinal(PSDynArrayGetLength(Pointer(Dest.P^), dest.aType)) then
              begin
                CMD_Err2(erCustomError,
                  TbtString(Format(RPS_OutOfRangeEx,
                    [Cardinal(PSDynArrayGetLength(Pointer(Dest.P^), dest.aType)),Param])));
                Result := False;
                Exit;
              end;
              {+}
              //Dest.P := Pointer(IPointer(Dest.P^) + (Param * TPSTypeRec_Array(Dest.aType).FArrayType.RealSize));
              Dest.P := PointerShift(Pointer(Dest.P^), Param * TPSTypeRec_Array(Dest.aType).FArrayType.RealSize);
              {+.}
              Dest.aType := TPSTypeRec_Array(dest.aType).ArrayType;
            end;
          btStaticArray:
            begin
              if Param >= Cardinal(TPSTypeRec_StaticArray(Dest.aType).Size) then
              begin
                CMD_Err2(erCustomError,
                  TbtString(Format(RPS_OutOfRangeEx,
                    [Cardinal(TPSTypeRec_StaticArray(Dest.aType).Size),Param])));
                Result := False;
                Exit;
              end;
              {+}
              //Dest.P := Pointer(IPointer(Dest.P) + (Param * TPSTypeRec_Array(Dest.aType).FArrayType.RealSize));
              Dest.P := PointerShift(Dest.P, Param * TPSTypeRec_Array(Dest.aType).FArrayType.RealSize);
              {+.}
              Dest.aType := TPSTypeRec_Array(dest.aType).ArrayType;
            end;
          else
            CMD_Err2(erInvalidType, TbtString(RPS_InvalidType));
            Result := False;
            Exit;
        end; // case Dest.aType.BaseType
        //
        if UsePointer and (Dest.aType.BaseType = btPointer) then
        begin
          Dest.aType := TPSTypeRec(Pointer(IPointer(Dest.p)+PointerSize)^);
          Dest.P := Pointer(Dest.p^);
          if Dest.P = nil then
          begin
            CMD_Err2(erNullPointerException, TbtString(RPS_NullPointerException));
            Result := False;
            Exit;
          end;
        end;
      end;
    else begin
      Result := False;
      Exit;
    end;
  end; // case VarType of
  Result := True; // @dbg: TObject(Dest.P^).ClassName  ; UnicodeString(Dest.P^)
end; // function TPSExec.ReadVariable

function TPSExec.DoMinus(Dta: Pointer; aType: TPSTypeRec): Boolean;
begin
  case atype.BaseType of
    btU8: tbtu8(dta^) := -tbtu8(dta^);
    btU16: tbtu16(dta^) := -tbtu16(dta^);
    btU32: tbtu32(dta^) := -tbtu32(dta^);
    btS8: tbts8(dta^) := -tbts8(dta^);
    btS16: tbts16(dta^) := -tbts16(dta^);
    btS32: tbts32(dta^) := -tbts32(dta^);
    {$IFNDEF PS_NOINT64}
    bts64: tbts64(dta^) := -tbts64(dta^);
    {$ENDIF}
    btSingle: tbtsingle(dta^) := -tbtsingle(dta^);
    btDouble: tbtdouble(dta^) := -tbtdouble(dta^);
    btExtended: tbtextended(dta^) := -tbtextended(dta^);
    btCurrency: tbtcurrency(dta^) := -tbtcurrency(dta^);
    btVariant:
      begin
        try
          Variant(dta^) := - Variant(dta^);
        except
          CMD_Err2(erTypeMismatch, TbtString(RPS_TypeMismatch));
          Result := False;
          Exit;
        end;
      end;
  else
    begin
      CMD_Err2(erTypeMismatch, TbtString(RPS_TypeMismatch));
      Result := False;
      Exit;
    end;
  end;
  Result := True;
end;

function TPSExec.DoBooleanNot(Dta: Pointer; aType: TPSTypeRec): Boolean;
begin
  case aType.BaseType of
    btU8: tbtu8(dta^) := tbtu8(tbtu8(dta^) = 0);
    btU16: tbtu16(dta^) := tbtu16(tbtu16(dta^) = 0);
    btU32: tbtu32(dta^) := tbtu32(tbtu32(dta^) = 0);
    btS8: tbts8(dta^) := tbts8(tbts8(dta^) = 0);
    btS16: tbts16(dta^) := tbts16(tbts16(dta^) = 0);
    btS32: tbts32(dta^) := tbts32(tbts32(dta^) = 0);
    {$IFNDEF PS_NOINT64}
    bts64: tbts64(dta^) := tbts64(tbts64(dta^) = 0);
    {$ENDIF}
    btVariant:
      begin
        try
          Variant(dta^) := Variant(dta^) = 0;
        except
          CMD_Err2(erTypeMismatch, TbtString(RPS_TypeMismatch));
          Result := False;
          Exit;
        end;
      end;
  else
    begin
      CMD_Err2(erTypeMismatch, TbtString(RPS_TypeMismatch));
      Result := False;
      Exit;
    end;
  end;
  Result := True;
end;

procedure TPSExec.Stop;
begin
  if FStatus = isRunning then
    FStatus := isLoaded
  else if FStatus = isPaused then begin
    FStatus := isLoaded;
    FStack.Clear;
    FTempVars.Clear;
  end;
end;

function TPSExec.ReadLong(var b: Cardinal): Boolean;
begin
  if FCurrentPosition + 3 < FDataLength then begin
    b := {$ifdef FPC_REQUIRES_PROPER_ALIGNMENT}unaligned{$endif}(
      Cardinal((@FData[FCurrentPosition])^)
    );
    Inc(FCurrentPosition, 4);
    Result := True;
  end
  else
    Result := False;
end;

function TPSExec.RunProcP(const Params: array of Variant; const Procno: Cardinal): Variant;
var
  ParamList: TPSList;
  ct: PIFTypeRec;
  pvar: PPSVariant;
  res, s: TbtString;
  Proc: TPSInternalProcRec;
  i: Longint;
begin
  Result := VarEmpty;
  if ProcNo >= FProcs.Count then raise Exception.Create(RPS_UnknownProcedure);
  Proc := GetProcNo(ProcNo) as TPSInternalProcRec;
  ParamList := TPSList.Create;
  try
    s := Proc.ExportDecl;
    res := grfw(s);
    i := High(Params);
    while s <> '' do
    begin
      if i < 0 then raise Exception.Create(RPS_NotEnoughParameters);
      ct := FTypes[StrToInt(copy(GRLW(s), 2, MaxInt))];
      if ct = nil then raise Exception.Create(RPS_InvalidParameter);
      PVar := CreateHeapVariant(ct);
      ParamList.Add(PVar);

      if not VariantToPIFVariant(Self, Params[i], PVar) then
        raise Exception.Create(RPS_InvalidParameter);

      Dec(i);
    end;
    if I > -1 then raise Exception.Create(RPS_TooManyParameters);
    if res <> '-1' then
    begin
      PVar := CreateHeapVariant(FTypes[StrToInt(res)]);
      ParamList.Add(PVar);
    end else
      PVar := nil;

    RunProc(ParamList, ProcNo);

    RaiseCurrentException;

    if PVar <> nil then begin
      PIFVariantToVariant(PVar, Result);
    end else
      Result := Null;
  finally
    FreePIFVariantList(ParamList);
  end;
end;
function TPSExec.RunProcPVar(var Params: array of Variant; const Procno: Cardinal): Variant;
var
  ParamList: TPSList;
  ct: PIFTypeRec;
  PVar: PPSVariant;
  res, s: TbtString;
  Proc: TPSInternalProcRec;
  i: Longint;
begin
  Result := VarEmpty;
  if ProcNo >= FProcs.Count then raise Exception.Create(RPS_UnknownProcedure);
  Proc := GetProcNo(ProcNo) as TPSInternalProcRec;
  ParamList := TPSList.Create;
  try
    s := Proc.ExportDecl;
    res := grfw(s);
    i := High(Params);
    while s <> '' do
    begin
      if i < 0 then raise Exception.Create(RPS_NotEnoughParameters);
      ct := FTypes[StrToInt(copy(GRLW(s), 2, MaxInt))];
      if ct = nil then raise Exception.Create(RPS_InvalidParameter);
      PVar := CreateHeapVariant(ct);
      ParamList.Add(PVar);

      if not VariantToPIFVariant(Self, Params[i], PVar) then
        raise Exception.Create(RPS_InvalidParameter);

      Dec(i);
    end;
    if I > -1 then
      raise Exception.Create(RPS_TooManyParameters);
    if res <> '-1' then begin
      PVar := CreateHeapVariant(FTypes[StrToInt(res)]);
      ParamList.Add(PVar);
    end else
      pvar := nil;

    RunProc(ParamList, ProcNo);

    RaiseCurrentException();

    for i := 0 to Length(Params) - 1 do
      PIFVariantToVariant(
        ParamList[i],
        Params[(Length(Params) - 1) - i]
      );

    if PVar <> nil then begin
      PIFVariantToVariant(PVar, Result);
    end
    else
      Result := Null;
  finally
    FreePIFVariantList(ParamList);
  end;
end;

function TPSExec.RunProcPN(const Params: array of Variant; const ProcName: TbtString): Variant;
var
  ProcNo: Cardinal;
begin
  ProcNo := GetProc(ProcName);
  if ProcNo = InvalidVal then
    raise Exception.Create(RPS_UnknownProcedure);
  Result := RunProcP(Params, ProcNo);
end;

function TPSExec.RunProc(Params: TPSList; ProcNo: Cardinal): Boolean;
var
  I, I2: Integer;
  vnew, vd: PIfVariant;
  Cp: TPSInternalProcRec;
  oldStatus: TPSStatus;
  tmp: TObject;
begin
  if FStatus <> isNotLoaded then begin
    if ProcNo >= FProcs.Count then begin
      CMD_Err2(erOutOfProcRange, TbtString(RPS_OutOfProcRange));
      Result := False;
      Exit;
    end;
    if Params <> nil then
    begin
      for I := 0 to Params.Count - 1 do
      begin
        vd := Params[I];
        if vd = nil then
        begin
          Result := False;
          Exit;
        end;
        vnew := FStack.PushType(FindType2(btPointer));
        if vd.FType.BaseType = btPointer then
        begin
          PPSVariantPointer(vnew).DestType := PPSVariantPointer(vd).DestType;
          PPSVariantPointer(vnew).DataDest := PPSVariantPointer(vd).DataDest;
        end else begin
          PPSVariantPointer(vnew).DestType := vd.FType;
          PPSVariantPointer(vnew).DataDest := @PPSVariantData(vd).Data;
        end;
      end;
    end;
    I := FStack.Count;
    Cp := FCurrProc;
    oldStatus := FStatus;
    if TPSProcRec(FProcs.Data^[ProcNo]).ClassType <> TPSExternalProcRec then
    begin
      vd := FStack.PushType(FReturnAddressType);
      PPSVariantReturnAddress(vd).Addr.ProcNo := nil;
      PPSVariantReturnAddress(vd).Addr.Position := FCurrentPosition;
      PPSVariantReturnAddress(vd).Addr.StackBase := FCurrStackBase;
      FCurrStackBase := FStack.Count - 1;
      FCurrProc := FProcs.Data^[ProcNo];
      FData := FCurrProc.Data;
      FDataLength := FCurrProc.Length;
      FCurrentPosition := 0;
      FStatus := isPaused;
      Result := RunScript;
    end else
    begin
      try
        Result := TPSExternalProcRec(FProcs.Data^[ProcNo]).ProcPtr(Self, TPSExternalProcRec(FProcs.Data^[ProcNo]), FGlobalVars, FStack);
        if not Result then
        begin
          if ExEx = erNoError then
            CMD_Err2(erCouldNotCallProc, TbtString(RPS_CouldNotCallProc));
        end;
      except
        {$IFDEF DELPHI6UP}
        Tmp := AcquireExceptionObject;
        {$ELSE}
        if RaiseList <> nil then
        begin
          Tmp := Exception(PRaiseFrame(RaiseList)^.ExceptObject);
          PRaiseFrame(RaiseList)^.ExceptObject := nil;
        end else
          Tmp := nil;
        {$ENDIF !DELPHI6UP}
        if Tmp <> nil then
        begin
          if Tmp is EPSException then
          begin
            Result := False;
            ExceptionProc(EPSException(tmp).ProcNo, EPSException(tmp).ProcPos, erCustomError, TbtString(EPSException(tmp).Message), nil);
            Exit;
          end else
          if Tmp is EDivByZero then
          begin
            Result := False;
            CMD_Err3(erDivideByZero, TbtString(Exception(Tmp).Message), Tmp);
            Exit;
          end;
          if Tmp is EZeroDivide then
          begin
            Result := False;
            CMD_Err3(erDivideByZero, TbtString(Exception(Tmp).Message), Tmp);
            Exit;
          end;
          if Tmp is EMathError then
          begin
            Result := False;
            CMD_Err3(erMathError, TbtString(Exception(Tmp).Message), Tmp);
            Exit;
          end;
        end;
        if (Tmp <> nil) and (Tmp is {+}ExceptionBase{+.}) then
          CMD_Err3(erException, TbtString(Exception(Tmp).Message), Tmp) else
          CMD_Err3(erException, '', Tmp);
        Result := False;
        Exit;
      end;
    end;
    if Cardinal(FStack.Count) > Cardinal(I) then
    begin
      vd := FStack[I];
      if (vd <> nil) and (vd.FType = FReturnAddressType) then
      begin
        for i2 := FStack.Count - 1 downto I + 1 do
          FStack.Pop;
        FCurrentPosition := PPSVariantReturnAddress(vd).Addr.Position;
        FCurrStackBase := PPSVariantReturnAddress(vd).Addr.StackBase;
        FStack.Pop;
      end;
    end;
    if Params <> nil then
    begin
      for I := Params.Count - 1 downto 0 do
      begin
        if FStack.Count = 0 then
          Break
        else
          FStack.Pop;
      end;
    end;
    FStatus := oldStatus;
    FCurrProc := Cp;
    if FCurrProc <> nil then
    begin
      FData := FCurrProc.Data;
      FDataLength := FCurrProc.Length;
    end;
  end else begin
    Result := False;
  end;
end;

function TPSExec.FindType2(BaseType: TPSBaseType): PIFTypeRec;
var l: Cardinal;
begin
  FindType2 := FindType(0, BaseType, {%H-}l);
end;

function TPSExec.FindType(StartAt: Cardinal; BaseType: TPSBaseType; var l: Cardinal): PIFTypeRec;
var
  I: Integer;
  n: PIFTypeRec;
begin
  for I := StartAt to FTypes.Count - 1 do begin
    n := FTypes[I];
    if n.BaseType = BaseType then begin
      l := I;
      Result := n;
      Exit;
    end;
  end;
  Result := nil;
end;

function TPSExec.GetTypeNo(l: Cardinal): PIFTypeRec;
begin
  Result := FTypes[l];
end;

function TPSExec.GetProc(const Name: TbtString): Cardinal;
var
  MM,
    I: Longint;
  n: PIFProcRec;
  s: TbtString;
begin
  s := FastUpperCase(name);
  MM := MakeHash(s);
  for I := FProcs.Count - 1 downto 0 do begin
    n := FProcs.Data^[I];
    if (n.ClassType = TPSInternalProcRec) and (TPSInternalProcRec(n).ExportNameHash = MM) and (TPSInternalProcRec(n).ExportName = s) then begin
      Result := I;
      Exit;
    end else if (n.ClassType = TPSExternalProcRec) and (TPSExternalProcRec(n).Name = s) then
    begin
      Result := I;
      Exit;
    end;
  end;
  Result := InvalidVal;
end;

function TPSExec.GetType(const Name: TbtString): Cardinal;
var
  MM,
    I: Longint;
  n: PIFTypeRec;
  s: TbtString;
begin
  s := FastUpperCase(name);
  MM := MakeHash(s);
  for I := 0 to FTypes.Count - 1 do begin
    n := FTypes.Data^[I];
    if (Length(n.ExportName) <> 0) and (n.ExportNameHash = MM) and (n.ExportName = s) then begin
      Result := I;
      Exit;
    end;
  end;
  Result := InvalidVal;
end;

procedure TPSExec.AddResource(Proc, P: Pointer);
var
  Temp: PPSResource;
begin
  New(Temp);
  Temp^.Proc := Proc;
  Temp^.P := p;
  FResources.Add(temp);
end;

procedure TPSExec.DeleteResource(P: Pointer);
var
  i: Longint;
begin
  for i := Longint(FResources.Count)-1 downto 0 do
  begin
    if PPSResource(FResources[I])^.P = P then
    begin
      FResources.Delete(I);
      Exit;
    end;
  end;
end;

function TPSExec.FindProcResource(Proc: Pointer): Pointer;
var
  I: Longint;
  temp: PPSResource;
begin
  for i := Longint(FResources.Count)-1 downto 0 do
  begin
    temp := FResources[I];
    if temp^.Proc = proc then
    begin
      Result := Temp^.P;
      Exit;
    end;
  end;
  Result := nil;
end;

function TPSExec.IsValidResource(Proc, P: Pointer): Boolean;
var
  i: Longint;
  temp: PPSResource;
begin
  for i := 0 to Longint(FResources.Count)-1 do begin
    temp := FResources[i];
    if temp^.p = p then begin
      Result := temp^.Proc = Proc;
      Exit;
    end;
  end;
  Result := False;
end;

function TPSExec.FindProcResource2(Proc: Pointer; var StartAt: Longint): Pointer;
var
  I: Longint;
  temp: PPSResource;
begin
  if StartAt > Longint(FResources.Count)-1 then
    StartAt := Longint(FResources.Count)-1;
  for i := StartAt downto 0 do begin
    temp := FResources[I];
    if temp^.Proc = proc then begin
      Result := Temp^.P;
      StartAt := i - 1;
      Exit;
    end;
  end;
  StartAt := -1;
  Result := nil;
end;

procedure TPSExec.RunLine;
begin
  if @FOnRunLine <> nil then
    FOnRunLine(Self);
end;

procedure TPSExec.CMD_Err3(EC: TPSError; const Param: TbtString; ExObject: TObject);
var
  l: Longint;
  C: Cardinal;
  sParam: TbtString;
begin {+}{@dbg@:hook.set.err}{+.} // dbg.cond: EC=ErTypeMismatch
  C := InvalidVal;
  sParam := Param + GetCurrentPositionDebugInfo('; ');
  if EC <> ErNoError then // @duplicate for convenient debugging
  begin
    for l := FProcs.Count - 1 downto 0 do begin
      if FProcs.Data^[l] = FCurrProc then begin
        C := l;
        break;
      end;
    end;
    if @FOnException <> nil then
      FOnException({Sender:}Self, {ExError:}Ec, {ExParam:}sParam, {ExObject:}ExObject, {ProcNo:}C,
        {Position:}FCurrentPosition);
    ExceptionProc({proc:}C, {Position:}FCurrentPosition, {Ex:}EC, {S:}sParam, {NewObject:}ExObject);
  end
  else
  begin
    for l := FProcs.Count - 1 downto 0 do begin
      if FProcs.Data^[l] = FCurrProc then begin
        C := l;
        break;
      end;
    end;
    if @FOnException <> nil then
      FOnException({Sender:}Self, {ExError:}Ec, {ExParam:}sParam, {ExObject:}ExObject, {ProcNo:}C,
        {Position:}FCurrentPosition);
    ExceptionProc({proc:}C, {Position:}FCurrentPosition, {Ex:}EC, {S:}sParam, {NewObject:}ExObject);
  end;
end;

procedure TPSExec.AddSpecialProcImport(const FName: TbtString;
  P: TPSOnSpecialProcImport; Tag: Pointer);
var
  N: PSpecialProc;
begin
  New(n);
  n^.P := P;
  N^.Name := FName;
  n^.namehash := MakeHash(N^.Name);
  n^.Tag := Tag;
  FSpecialProcList.Add(n);
end;

function TPSExec.GetVar(const Name: TbtString): Cardinal;
var
  l: Longint;
  h: longint;
  s: TbtString;
  p: PPSExportedVar;
begin
  s := FastUpperCase(name);
  h := MakeHash(s);
  for l := FExportedVars.Count - 1 downto 0 do
  begin
    p := FexportedVars.Data^[L];
    if (p^.FNameHash = h) and(p^.FName=s) then
    begin
      Result := L;
      Exit;
    end;
  end;
  Result := InvalidVal;
end;

function TPSExec.GetVarNo(C: Cardinal): PIFVariant;
begin
  Result := FGlobalVars[c];
end;

function TPSExec.GetVar2(const Name: TbtString): PIFVariant;
begin
  Result := GetVarNo(GetVar(Name));
end;

function TPSExec.GetProcNo(C: Cardinal): PIFProcRec;
begin
  Result := FProcs[c];
end;

function TPSExec.DoIntegerNot(Dta: Pointer; aType: TPSTypeRec): Boolean;
begin
  case aType.BaseType of
    btU8: tbtu8(dta^) := not tbtu8(dta^);
    btU16: tbtu16(dta^) := not tbtu16(dta^);
    btU32: tbtu32(dta^) := not tbtu32(dta^);
    btS8: tbts8(dta^) := not tbts8(dta^);
    btS16: tbts16(dta^) := not tbts16(dta^);
    btS32: tbts32(dta^) := not tbts32(dta^);
    {$IFNDEF PS_NOINT64}
    bts64: tbts64(dta^) := not tbts64(dta^);
    {$ENDIF}
    btVariant:
      begin
        try
          Variant(dta^) := not Variant(dta^);
        except
          CMD_Err2(erTypeMismatch, TbtString(RPS_TypeMismatch));
          Result := False;
          Exit;
        end;
      end;
  else
    begin
      CMD_Err2(erTypeMismatch, TbtString(RPS_TypeMismatch));
      Result := False;
      Exit;
    end;
  end;
  Result := True;
end;

type
  TMyRunLine = procedure(Self: TPSExec);
  TPSRunLine = procedure of object;

function GetRunLine(FOnRunLine: TPSOnLineEvent; meth: TPSRunLine): TMyRunLine;
begin
  if (TMethod(Meth).Code = @TPSExec.RunLine) and (@FOnRunLine = nil) then
    Result := nil
  else
    Result := TMethod(Meth).Code;
end;

function TPSExec.RunScript: Boolean;
var
  CalcType: Cardinal;
  vd, vs, v3: TPSResultData;
  vtemp: PIFVariant;
  p: Cardinal;
  P2: Longint;
  u: PIFProcRec;
  Cmd: Cardinal;
  I: Longint;
  pp: TPSExceptionHandler; //AExceptAddr: Pointer;
  FExitPoint: Cardinal;
  FOldStatus: TPSStatus;
  Tmp: TObject;
  OK: Boolean;
  CallRunline: TMyRunLine;
  sError: TbtString;
begin
  FExitPoint := InvalidVal;
  if FStatus = isLoaded then
  begin
    for i := FExceptionStack.Count-1 downto 0 do
    begin
      pp := FExceptionStack.Data[i];
      pp.Free;
    end;
    FExceptionStack.Clear;
  end;
  ExceptionProc(InvalidVal, InvalidVal, erNoError, '', nil);
  Result := True;
  FOldStatus := FStatus;
  case FStatus of
    isLoaded: begin
        if FMainProc = InvalidVal then
        begin
          Result := False;
          Exit;
        end;
        FStatus := isRunning;
        FCurrProc := FProcs.Data^[FMainProc];
        if FCurrProc.ClassType = TPSExternalProcRec then begin
          CMD_Err2(erNoMainProc, TbtString(RPS_NoMainProc));
          FStatus := isLoaded;
          Exit;
        end;
        FData := FCurrProc.Data;
        FDataLength := FCurrProc.Length;
        FCurrStackBase := InvalidVal;
        FCurrentPosition := 0;
      end;
    isPaused: begin
        FStatus := isRunning;
      end;
  else begin
      Result := False;
      Exit;
    end;
  end;
  CallRunLine := GetRunLine(FOnRunLine, Self.RunLine);
  repeat
    FStatus := isRunning;
    //Cmd := InvalidVal;
    while FStatus = isRunning do
    begin
      if Assigned(CallRunLine) then
        CallRunLine(Self);
      if FCurrentPosition >= FDataLength then
      begin
        CMD_Err2(erOutOfRange, TbtString(RPS_OutOfRange));
        break;
      end;
      //if cmd <> InvalidVal then ProfilerExitProc(Cmd+1);
      cmd := FData[FCurrentPosition];
      //ProfilerEnterProc(Cmd+1);
      Inc(FCurrentPosition);
        case Cmd of
          CM_A:
            begin
              if not ReadVariable({%H-}vd, True) then
                break;
              if vd.FreeType <> vtNone then
              begin
                if vd.aType.BaseType in NeedFinalization then
                  FinalizeVariant(vd.P, vd.aType);
                p := IPointer(FTempVars.Data^[FtempVars.Count-1]) - IPointer(FtempVars.DataPtr);
                Dec(FTempVars.FCount);
                {$IFNDEF PS_NOSMARTLIST}
                Inc(FTempVars.FCheckCount);
                if FTempVars.FCheckCount > FMaxCheckCount then FTempVars.Recreate;
                {$ENDIF}
                FTempVars.FLength := P;
                if ((FTempVars.FCapacity - FTempVars.FLength) shr 12) > 2 then FTempVars.AdjustLength;

                CMD_Err2(erInvalidOpcodeParameter, TbtString(RPS_InvalidOpcodeParameter));
                break;
              end;
              if not ReadVariable({%H-}vs, True) then
                Break;
              // nx change end
{              if (vd.aType.BaseType = btClass) and (vs.aType.BaseType in [btS32]) then
                DWord(vd.P^):=Dword(vs.P^)
              else
              if (vd.aType.BaseType in [btS32]) and (vs.aType.BaseType = btClass) then
                DWord(vd.P^):=Dword(vs.P^)
              else}
              // nx change start
              if not SetVariantValue(vd.P, vs.P, vd.aType, vs.aType{+}, @vd{+.}) then
              begin
                if vs.FreeType <> vtNone then
                begin
                  if vs.aType.BaseType in NeedFinalization then
                  {+}
//                  if (vs.aType.ClassType <> TPSTypeRec_Chars) or (TPSTypeRec_Chars(vs.aType).FPChasrRefCount = 0) then
                    FinalizeVariant(vs.P, vs.aType);
                  {+.}
                  p := IPointer(FTempVars.Data^[FtempVars.Count-1]) - IPointer(FtempVars.DataPtr);
                  Dec(FTempVars.FCount);
                  {$IFNDEF PS_NOSMARTLIST}
                  Inc(FTempVars.FCheckCount);
                  if FTempVars.FCheckCount > FMaxCheckCount then FTempVars.Recreate;
                  {$ENDIF}
                  FTempVars.FLength := P;
                  if ((FTempVars.FCapacity - FTempVars.FLength) shr 12) > 2 then FTempVars.AdjustLength;
                end;
                Break;
              end;
              if vs.FreeType <> vtNone then
              begin
{+}
//TODO: bug. Sample:
{
var
  s: ansistring;
  ps: pansichar;
begin
  ps := 'pansichar';
  s := ansistring(ps);
  writeln(s);
end.
}
                if vs.aType.BaseType in NeedFinalization then
                begin
                  (*
                  //if vd.aType.BaseType in [btPChar{$IFNDEF PS_NOWIDESTRING}{$if declared(btPWideChar)},btPWideChar{$ifend}{$ENDIF}] then begin
                  if (vd.aType.ClassType = TPSTypeRec_PChar)
                    //and (vs.aType.BaseType in [btString{$IFNDEF PS_NOWIDESTRING}, btWidestring, btUnicodeString{$ENDIF}]) then
                    and (vs.aType.ClassType = TPSTypeRec_Chars) then
                  begin
                    //if vs.aType.ClassType = TPSTypeRec_Chars then
                    begin
                      TPSTypeRec_PChar(vd.aType).FTypeCharsRec := TPSTypeRec_Chars(vs.aType);
                      Inc(TPSTypeRec_Chars(vs.aType).FPChasrRefCount);
                      //TPSTypeRec_PChar(vd.aType).FTempBaseType := vs.aType.BaseType;
                      TPSTypeRec_PChar(vd.aType).FTempData := vs.P;
                    //end else begin
                    //  TPSTypeRec_PChar(vd.aType).FTempBaseType := vs.aType.BaseType;
                    //  TPSTypeRec_PChar(vd.aType).FTempData := vs.P;
                    //  vs.P := nil;
                    //  FinalizeVariant(vs.P, vs.aType);
                    end;
                  end
                  else
                  begin
                    if (vs.aType.ClassType <> TPSTypeRec_Chars) or (TPSTypeRec_Chars(vs.aType).FPChasrRefCount <= 0) then
                      FinalizeVariant(vs.P, vs.aType);
                  end; // dbg: PChar(@pstring(vs.P)^[1])[2]
                  //*)
                  FinalizeVariant(vs.P, vs.aType);
                end;
                {+.}
                p := IPointer(FTempVars.Data^[FtempVars.Count-1]) - IPointer(FtempVars.DataPtr);
                Dec(FTempVars.FCount);
                {$IFNDEF PS_NOSMARTLIST}
                Inc(FTempVars.FCheckCount);
                if FTempVars.FCheckCount > FMaxCheckCount then FTempVars.Recreate;
                {$ENDIF}
                FTempVars.FLength := P;
                if ((FTempVars.FCapacity - FTempVars.FLength) shr 12) > 2 then FTempVars.AdjustLength;
              end;
            end;
          CM_CA:
            begin
              if FCurrentPosition >= FDataLength then
              begin
                CMD_Err2(erOutOfRange, TbtString(RPS_OutOfRange)); // Error
                break;
              end;
              calctype := FData[FCurrentPosition];
              Inc(FCurrentPosition);
              if not ReadVariable(vd, True) then
                break;
              if vd.FreeType <> vtNone then
              begin
                if vd.aType.BaseType in NeedFinalization then
                FinalizeVariant(vd.P, vd.aType);
                p := IPointer(FTempVars.Data^[FtempVars.Count-1]) - IPointer(FtempVars.DataPtr);
                Dec(FTempVars.FCount);
                {$IFNDEF PS_NOSMARTLIST}
                Inc(FTempVars.FCheckCount);
                if FTempVars.FCheckCount > FMaxCheckCount then FTempVars.Recreate;
                {$ENDIF}
                FTempVars.FLength := P;
                if ((FTempVars.FCapacity - FTempVars.FLength) shr 12) > 2 then FTempVars.AdjustLength;
                CMD_Err2(erInvalidOpcodeParameter, TbtString(RPS_InvalidOpcodeParameter));
                break;
              end;
              if not ReadVariable(vs, True) then
                Break;
              if not DoCalc(vd.P, vs.p, vd.aType, vs.aType, CalcType) then
              begin
                if vs.FreeType <> vtNone then
                begin
                  if vs.aType.BaseType in NeedFinalization then
                  FinalizeVariant(vs.P, vs.aType);
                  p := IPointer(FTempVars.Data^[FtempVars.Count-1]) - IPointer(FtempVars.DataPtr);
                  Dec(FTempVars.FCount);
                  {$IFNDEF PS_NOSMARTLIST}
                  Inc(FTempVars.FCheckCount);
                  if FTempVars.FCheckCount > FMaxCheckCount then FTempVars.Recreate;
                  {$ENDIF}
                  FTempVars.FLength := P;
                  if ((FTempVars.FCapacity - FTempVars.FLength) shr 12) > 2 then FTempVars.AdjustLength;
                end;
                Break;
              end;
              if vs.FreeType <> vtNone then
              begin
                if vs.aType.BaseType in NeedFinalization then
                FinalizeVariant(vs.P, vs.aType);
                p := IPointer(FTempVars.Data^[FtempVars.Count-1]) - IPointer(FtempVars.DataPtr);
                Dec(FTempVars.FCount);
                {$IFNDEF PS_NOSMARTLIST}
                Inc(FTempVars.FCheckCount);
                if FTempVars.FCheckCount > FMaxCheckCount then FTempVars.Recreate;
                {$ENDIF}
                FTempVars.FLength := P;
                if ((FTempVars.FCapacity - FTempVars.FLength) shr 12) > 2 then FTempVars.AdjustLength;
              end;
            end;
          CM_P:
            begin
              if not ReadVariable(vs, True) then
                Break;
              vtemp := FStack.PushType(vs.aType);
              vd.P := Pointer(IPointer(vtemp)+PointerSize);
              vd.aType := Pointer(vtemp^);
              vd.FreeType := vtNone;
              if not SetVariantValue(Vd.P, vs.P, vd.aType, vs.aType{+}, @vd{+.}) then
              begin
                if vs.FreeType <> vtnone then
                begin
                  if vs.aType.BaseType in NeedFinalization then
                    FinalizeVariant(vs.P, vs.aType);  //todo:...
                  p := IPointer(FTempVars.Data^[FtempVars.Count-1]) - IPointer(FtempVars.DataPtr);
                  Dec(FTempVars.FCount);
                  {$IFNDEF PS_NOSMARTLIST}
                  Inc(FTempVars.FCheckCount);
                  if FTempVars.FCheckCount > FMaxCheckCount then FTempVars.Recreate;
                  {$ENDIF}
                  FTempVars.FLength := P;
                  if ((FTempVars.FCapacity - FTempVars.FLength) shr 12) > 2 then FTempVars.AdjustLength;
                end;
                break;
              end;
              if vs.FreeType <> vtnone then
              begin
                if vs.aType.BaseType in NeedFinalization then
                  FinalizeVariant(vs.P, vs.aType); //todo:...
                p := IPointer(FTempVars.Data^[FtempVars.Count-1]) - IPointer(FtempVars.DataPtr);
                Dec(FTempVars.FCount);
                {$IFNDEF PS_NOSMARTLIST}
                Inc(FTempVars.FCheckCount);
                if FTempVars.FCheckCount > FMaxCheckCount then FTempVars.Recreate;
                {$ENDIF}
                FTempVars.FLength := P;
                if ((FTempVars.FCapacity - FTempVars.FLength) shr 12) > 2 then FTempVars.AdjustLength;
              end;
            end;
          CM_PV:
            begin
              if not ReadVariable(vs, True) then
                Break;
              if vs.FreeType <> vtnone then
              begin
                FTempVars.Pop;
                CMD_Err2(erInvalidOpcodeParameter, TbtString(RPS_InvalidOpcodeParameter));
                break;
              end;
              vtemp := FStack.PushType(FindType2(btPointer));
              if vs.aType.BaseType = btPointer then
              begin
                PPSVariantPointer(vtemp).DataDest := Pointer(vs.p^);
                PPSVariantPointer(vtemp).DestType := Pointer(Pointer(IPointer(vs.P)+PointerSize)^);
                PPSVariantPointer(vtemp).FreeIt := False;
              end
              else
              begin
                PPSVariantPointer(vtemp).DataDest := vs.p;
                PPSVariantPointer(vtemp).DestType := vs.aType;
                PPSVariantPointer(vtemp).FreeIt := False;
              end;
            end;
          CM_PO: begin
              if FStack.Count = 0 then
              begin
                CMD_Err2(erOutOfStackRange, TbtString(RPS_OutOfStackRange));
                break;
              end;
              vtemp := FStack.Data^[FStack.Count-1];
              if (vtemp = nil) or (vtemp.FType.BaseType = btReturnAddress) then
              begin
                CMD_Err2(erOutOfStackRange, TbtString(RPS_OutOfStackRange));
                break;
              end;
              FStack.Pop;
(*            Dec(FStack.FCount);
              {$IFNDEF PS_NOSMARTLIST}
              Inc(FStack.FCheckCount);
              if FStack.FCheckCount > FMaxCheckCount then FStack.Recreate;
              {$ENDIF}
              FStack.FLength := Longint(IPointer(vtemp) - IPointer(FStack.DataPtr));
              if TPSTypeRec(vtemp^).BaseType in NeedFinalization then
                FinalizeVariant(Pointer(IPointer(vtemp)+PointerSize), Pointer(vtemp^));
              if ((FStack.FCapacity - FStack.FLength) shr 12) > 2 then FStack.AdjustLength;*)
            end;
          Cm_C: begin
              if FCurrentPosition + 3 >= FDataLength then
              begin
                CMD_Err2(erOutOfRange, TbtString(RPS_OutOfRange));
                Break;
              end;
              p := {$ifdef FPC_REQUIRES_PROPER_ALIGNMENT}unaligned{$endif}(
                Cardinal((@FData[FCurrentPosition])^)
              );
              Inc(FCurrentPosition, 4);
              if p >= FProcs.Count then begin
                CMD_Err2(erOutOfProcRange, TbtString(RPS_OutOfProcRange));
                break;
              end;
              u := FProcs.Data^[p];
              if u.ClassType = TPSExternalProcRec then begin
                try // dbg: PPSVariantIFC(MyList[2]).aType.FBaseType ; FStack[2].FType,r
                  OK := TPSExternalProcRec(u).ProcPtr(Self, TPSExternalProcRec(u), FGlobalVars, FStack);
                  if (not OK) then
                  begin // @dbg: TPSExternalProcRec(u).fName = 'CREATE'
                    if (ExEx = erNoError) then
                    {+}
                      //CMD_Err2(erCouldNotCallProc, TbtString(RPS_CouldNotCallProc));  // #dbg: TPSExternalProcRec(u).fName
                    begin //@dbg: GetCurrentPositionDebugInfo()  ;  GetCallStack(p)
                      if (FExceptionStack.Count = 0) then
                        CMD_Err2(erCouldNotCallProc, TbtString(RPS_CouldNotCallProc))
                      else
                      begin
                        pp := FExceptionStack.Data[FExceptionStack.Count-1];
                        if (pp.ExceptionParam = '') then
                          CMD_Err2(erCustomError, TbtString(RPS_CouldNotCallProc) + TbtString(': ') + TPSExternalProcRec(u).fName)
                        else
                        begin
                          //-if TbtString(RPS_CouldNotCallProc) <> pp.ExceptionParam then
                          if Pos(RPS_CouldNotCallProc, string(pp.ExceptionParam)) <= 0 then
                            CMD_Err2(erCustomError, TbtString(RPS_CouldNotCallProc) + TbtString(': ') + pp.ExceptionParam)
                          else
                            CMD_Err2(erCustomError, TbtString(RPS_CouldNotCallProc));
                        end;
                      end;
                    end;
                    {+.}
                    Break;
                  end;
                except
                  //--on e: Exception do
                  begin
                    //-AExceptAddr := {System.}ExceptAddr; // == nil !!! => Failed calculate exception stack
                    {$IFDEF DELPHI6UP}
                    Tmp := AcquireExceptionObject;
                    {$ELSE}
                    if RaiseList <> nil then
                    begin
                      Tmp := Exception(PRaiseFrame(RaiseList)^.ExceptObject);
                      PRaiseFrame(RaiseList)^.ExceptObject := nil;
                    end else
                      Tmp := nil;
                    {$ENDIF !DELPHI6UP}
                    if Tmp <> nil then
                    begin
                      if Tmp is EPSException then
                      begin
                        ExceptionProc(EPSException(tmp).ProcNo, EPSException(tmp).ProcPos, erCustomError, TbtString(EPSException(tmp).Message), nil);
                        Break;
                      end else
                      if Tmp is EDivByZero then
                      begin
                        CMD_Err3(erDivideByZero, TbtString(Exception(Tmp).Message), Tmp);
                        Break;
                      end;
                      if Tmp is EZeroDivide then
                      begin
                        CMD_Err3(erDivideByZero, TbtString(Exception(Tmp).Message), Tmp);
                        Break;
                      end;
                      if Tmp is EMathError then
                      begin
                        CMD_Err3(erMathError, TbtString(Exception(Tmp).Message), Tmp);
                        Break;
                      end;
                    end;
                    if (Tmp <> nil) and (Tmp is {+}ExceptionBase{+.}) then
                      CMD_Err3(erException, TbtString(Exception(Tmp).Message), Tmp) else
                      CMD_Err3(erException, '', Tmp);
                    Break;
                  end;
                end;
              end
              else begin
                Vtemp := Fstack.PushType(FReturnAddressType);
                vd.P := Pointer(IPointer(VTemp)+PointerSize);
                vd.aType := pointer(vtemp^);
                vd.FreeType := vtNone;
                PPSVariantReturnAddress(vtemp).Addr.ProcNo := FCurrProc;
                PPSVariantReturnAddress(vtemp).Addr.Position := FCurrentPosition;
                PPSVariantReturnAddress(vtemp).Addr.StackBase := FCurrStackBase;

                FCurrStackBase := FStack.Count - 1;
                FCurrProc := TPSInternalProcRec(u);
                FData := FCurrProc.Data;
                FDataLength := FCurrProc.Length;
                FCurrentPosition := 0;
              end;
            end;
          CM_PG:
            begin
              FStack.Pop;
              if FCurrentPosition + 3 >= FDataLength then
              begin
                CMD_Err2(erOutOfRange, TbtString(RPS_OutOfRange));
                Break;
              end;
              p := {$ifdef FPC_REQUIRES_PROPER_ALIGNMENT}unaligned{$endif}(
                Cardinal((@FData[FCurrentPosition])^)
              );
              Inc(FCurrentPosition, 4);
              FCurrentPosition := FCurrentPosition + p;
            end;
          CM_P2G:
            begin
              FStack.Pop;
              FStack.Pop;
              if FCurrentPosition + 3 >= FDataLength then
              begin
                CMD_Err2(erOutOfRange, TbtString(RPS_OutOfRange));
                Break;
              end;
              p := {$ifdef FPC_REQUIRES_PROPER_ALIGNMENT}unaligned{$endif}(
                Cardinal((@FData[FCurrentPosition])^)
              );
              Inc(FCurrentPosition, 4);
              FCurrentPosition := FCurrentPosition + p;
            end;
          Cm_G:
            begin
              if FCurrentPosition + 3 >= FDataLength then
              begin
                CMD_Err2(erOutOfRange, TbtString(RPS_OutOfRange));
                Break;
              end;
              p := {$ifdef FPC_REQUIRES_PROPER_ALIGNMENT}unaligned{$endif}(
                Cardinal((@FData[FCurrentPosition])^)
              );
              Inc(FCurrentPosition, 4);
              FCurrentPosition := FCurrentPosition + p;
            end;
          Cm_CG:
            begin
              if FCurrentPosition + 3 >= FDataLength then
              begin
                CMD_Err2(erOutOfRange, TbtString(RPS_OutOfRange));
                Break;
              end;
              p := {$ifdef FPC_REQUIRES_PROPER_ALIGNMENT}unaligned{$endif}(
                Cardinal((@FData[FCurrentPosition])^)
              );
              Inc(FCurrentPosition, 4);
              OK := True;
              if not ReadVariable(vs, OK) then
                Break;
              case Vs.aType.BaseType of
                btU8: OK := tbtu8(vs.p^) <> 0;
                btS8: OK := tbts8(vs.p^) <> 0;
                btU16: OK := tbtu16(vs.p^) <> 0;
                btS16: OK := tbts16(vs.p^) <> 0;
                btU32: OK := tbtu32(vs.p^) <> 0;
                btS32: OK := tbts32(vs.p^) <> 0;
              else begin
                  CMD_Err2(erInvalidType, TbtString(RPS_InvalidType));
                  if vs.FreeType <> vtNone then
                    FTempVars.Pop;
                  break;
                end;
              end;
              if vs.FreeType <> vtNone then
                FTempVars.Pop;
              if OK then
                FCurrentPosition := FCurrentPosition + p;
            end;
          Cm_CNG:
            begin
              if FCurrentPosition + 3 >= FDataLength then
              begin
                CMD_Err2(erOutOfRange, TbtString(RPS_OutOfRange));
                Break;
              end;
              p := {$ifdef FPC_REQUIRES_PROPER_ALIGNMENT}unaligned{$endif}(
                Cardinal((@FData[FCurrentPosition])^)
              );
              Inc(FCurrentPosition, 4);
              OK := True;
              if not ReadVariable(vs, OK) then
                Break;
              case Vs.aType.BaseType of
                btU8: OK := tbtu8(vs.p^) = 0;
                btS8: OK := tbts8(vs.p^) = 0;
                btU16: OK := tbtu16(vs.p^) = 0;
                btS16: OK := tbts16(vs.p^) = 0;
                btU32: OK := tbtu32(vs.p^) = 0;
                btS32: OK := tbts32(vs.p^) = 0;
              else begin
                  CMD_Err2(erInvalidType, TbtString(RPS_InvalidType));
                  if vs.FreeType <> vtNone then
                    FTempVars.Pop;
                  break;
                end;
              end;
              if vs.FreeType <> vtNone then
                FTempVars.Pop;
              if OK then
                FCurrentPosition := FCurrentPosition + p;
            end;
          Cm_R: begin
              FExitPoint := FCurrentPosition-1;
              P2 := 0;
              if FExceptionStack.Count > 0 then
              begin
                pp := FExceptionStack.Data[FExceptionStack.Count-1];
                while (pp.BasePtr = FCurrStackBase) or ((pp.BasePtr > FCurrStackBase) and (pp.BasePtr <> InvalidVal)) do
                begin
                  if pp.StackSize < Cardinal(FStack.Count) then
                  begin
                    for p := Longint(FStack.count)-1 downto Longint(pp.StackSize) do
                      FStack.Pop
                  end;
                  FCurrStackBase := pp.BasePtr;
                  if pp.FinallyOffset <> InvalidVal then
                  begin
                    FCurrentPosition := pp.FinallyOffset;
                    pp.FinallyOffset := InvalidVal;
                    p2 := 1;
                    break;
                  end else if pp.Finally2Offset <> InvalidVal then
                  begin
                    FCurrentPosition := pp.Finally2Offset;
                    pp.Finally2Offset := InvalidVal;
                    p2 := 1;
                    break;
                  end else
                  begin
                    pp.Free;
                    FExceptionStack.DeleteLast;
                    if FExceptionStack.Count = 0 then break;
                    pp := FExceptionStack.Data[FExceptionStack.Count-1];
                  end;
                end;
              end;
              if p2 = 0 then
              begin
                FExitPoint := InvalidVal;
                if FCurrStackBase = InvalidVal then
                begin
                  FStatus := FOldStatus;
                  break;
                end;
                for P2 := FStack.Count - 1 downto FCurrStackBase + 1 do
                  FStack.Pop;
                if FCurrStackBase >= FStack.Count  then
                begin
                  FStatus := FOldStatus;
                  break;
                end;
                vtemp := FStack.Data[FCurrStackBase];
                FCurrProc := PPSVariantReturnAddress(vtemp).Addr.ProcNo;
                FCurrentPosition := PPSVariantReturnAddress(vtemp).Addr.Position;
                FCurrStackBase := PPSVariantReturnAddress(vtemp).Addr.StackBase;
                FStack.Pop;
                if FCurrProc = nil then begin
                  FStatus := FOldStatus;
                  break;
                end;
                {+}
                try
                  FData := FCurrProc.Data; // TODO: AV: need TEST when in script call: try ?raise ... ?try except ?raise ... end finally ... ?raise ... end
                  FDataLength := FCurrProc.Length;
                except
                  //on e: Exception do
                  begin
                    FData := nil;
                    FDataLength := 0;
                    raise;
                  end;
                end;
                {+.}
              end;
            end;
          Cm_Pt: begin
              if FCurrentPosition + 3 >= FDataLength then
              begin
                CMD_Err2(erOutOfRange, TbtString(RPS_OutOfRange));
                Break;
              end;
              p := {$ifdef FPC_REQUIRES_PROPER_ALIGNMENT}unaligned{$endif}(
                Cardinal((@FData[FCurrentPosition])^)
              );
              Inc(FCurrentPosition, 4);
              if p > FTypes.Count then
              begin
                CMD_Err2(erInvalidType, TbtString(RPS_InvalidType));
                break;
              end;
              FStack.PushType(FTypes.Data^[p]);
            end;
          cm_bn:
            begin
              if not ReadVariable(vd, True) then
                Break;
              if vd.FreeType <> vtNone then
                FTempVars.Pop;
              if not DoBooleanNot(Vd.P, vd.aType) then
                break;
            end;
          cm_in:
            begin
              if not ReadVariable(vd, True) then
                Break;
              if vd.FreeType <> vtNone then
                FTempVars.Pop;
              if not DoIntegerNot(Vd.P, vd.aType) then
                break;
            end;
          cm_vm:
            begin
              if not ReadVariable(vd, True) then
                Break;
              if vd.FreeType <> vtNone then
                FTempVars.Pop;
              if not DoMinus(Vd.P, vd.aType) then
                break;
            end;
          cm_sf:
            begin
              if not ReadVariable(vd, True) then
                Break;
              if FCurrentPosition >= FDataLength then
              begin
                CMD_Err2(erOutOfRange, TbtString(RPS_OutOfRange)); // Error
                if vd.FreeType <> vtNone then
                  FTempVars.Pop;
                break;
              end;
              p := FData[FCurrentPosition];
              Inc(FCurrentPosition);
              case Vd.aType.BaseType of
                btU8: FJumpFlag := tbtu8(Vd.p^) <> 0;
                btS8: FJumpFlag := tbts8(Vd.p^) <> 0;
                btU16: FJumpFlag := tbtu16(Vd.p^) <> 0;
                btS16: FJumpFlag := tbts16(Vd.p^) <> 0;
                btU32: FJumpFlag := tbtu32(Vd.p^) <> 0;
                btS32: FJumpFlag := tbts32(Vd.p^) <> 0;
              else begin
                  CMD_Err2(erInvalidType, TbtString(RPS_InvalidType));
                  if vd.FreeType <> vtNone then
                    FTempVars.Pop;
                  break;
                end;
              end;
              if p <> 0 then
                FJumpFlag := not FJumpFlag;
              if vd.FreeType <> vtNone then
                FTempVars.Pop;
            end;
          cm_fg:
            begin
              if FCurrentPosition + 3 >= FDataLength then
              begin
                CMD_Err2(erOutOfRange, TbtString(RPS_OutOfRange));
                Break;
              end;
              p := {$ifdef FPC_REQUIRES_PROPER_ALIGNMENT}unaligned{$endif}(
                Cardinal((@FData[FCurrentPosition])^)
              );
              Inc(FCurrentPosition, 4);
              if FJumpFlag then
                FCurrentPosition := FCurrentPosition + p;
            end;
          cm_puexh:
            begin
              pp := TPSExceptionHandler.Create;
              pp.CurrProc := FCurrProc;
              pp.BasePtr := FCurrStackBase;
              pp.StackSize := FStack.Count;
              if not ReadLong(pp.FinallyOffset) then begin
                CMD_Err2(erOutOfRange, TbtString(RPS_OutOfRange));
                pp.Free;
                Break;
              end;
              if not ReadLong(pp.ExceptOffset) then begin
                CMD_Err2(erOutOfRange, TbtString(RPS_OutOfRange));
                pp.Free;
                Break;
              end;
              if not ReadLong(pp.Finally2Offset) then begin
                CMD_Err2(erOutOfRange, TbtString(RPS_OutOfRange));
                pp.Free;
                Break;
              end;
              if not ReadLong(pp.EndOfBlock) then begin
                CMD_Err2(erOutOfRange, TbtString(RPS_OutOfRange));
                pp.Free;
                Break;
              end;
              if pp.FinallyOffset <> InvalidVal then
                pp.FinallyOffset := pp.FinallyOffset + FCurrentPosition;
              if pp.ExceptOffset <> InvalidVal then
                pp.ExceptOffset := pp.ExceptOffset + FCurrentPosition;
              if pp.Finally2Offset <> InvalidVal then
                pp.Finally2Offset := pp.Finally2Offset + FCurrentPosition;
              if pp.EndOfBlock <> InvalidVal then
                pp.EndOfBlock := pp.EndOfBlock + FCurrentPosition;
              if ((pp.FinallyOffset <> InvalidVal) and (pp.FinallyOffset >= FDataLength)) or
                ((pp.ExceptOffset <> InvalidVal) and (pp.ExceptOffset >= FDataLength)) or
                ((pp.Finally2Offset <> InvalidVal) and (pp.Finally2Offset >= FDataLength)) or
                ((pp.EndOfBlock <> InvalidVal) and (pp.EndOfBlock >= FDataLength)) then
                begin
                  CMD_Err2(erOutOfRange, TbtString(RPS_OutOfRange));
                  pp.Free;
                  Break;
                end;
                FExceptionStack.Add(pp);
            end;
          cm_poexh:
            begin
              if FCurrentPosition >= FDataLength then
              begin
                CMD_Err2(erOutOfRange, TbtString(RPS_OutOfRange)); // Error
                break;
              end;
              p := FData[FCurrentPosition];
              Inc(FCurrentPosition);
              case p of
                2:
                  begin
                    if (FExceptionStack.Count = 0) then
                    begin
                      CMD_Err2(erOutOfRange, TbtString(RPS_OutOfRange));
                      Break;
                    end;
                    pp := FExceptionStack.Data^[FExceptionStack.Count-1];
                    if pp = nil then begin
                      CMD_Err2(erOutOfRange, TbtString(RPS_OutOfRange));
                      Break;
                    end;
                    pp.ExceptOffset := InvalidVal;
                    if pp.Finally2Offset <> InvalidVal then
                    begin
                      FCurrentPosition := pp.Finally2Offset;
                      pp.Finally2Offset := InvalidVal;
                    end else begin
                      p := pp.EndOfBlock;
                      pp.Free;
                      FExceptionStack.DeleteLast;
                      if FExitPoint <> InvalidVal then
                      begin
                        FCurrentPosition := FExitPoint;
                      end else begin
                        FCurrentPosition := p;
                      end;
                    end;
                  end;
                0:
                  begin
                    pp := FExceptionStack.Data^[FExceptionStack.Count-1];
                    if pp = nil then begin
                      CMD_Err2(erOutOfRange, TbtString(RPS_OutOfRange));
                      Break;
                    end;
                    if pp.FinallyOffset <> InvalidVal then
                    begin
                      FCurrentPosition := pp.FinallyOffset;
                      pp.FinallyOffset := InvalidVal;
                    end else if pp.Finally2Offset <> InvalidVal then
                    begin
                       FCurrentPosition := pp.Finally2Offset;
                       pp.ExceptOffset := InvalidVal;
                    end else begin
                      p := pp.EndOfBlock;
                      pp.Free;
                      FExceptionStack.DeleteLast;
                      if ExEx <> eNoError then
                      begin
                        Tmp := ExObject;
                        ExObject := nil;
                        ExceptionProc(ExProc, ExPos, ExEx, ExParam, Tmp);
                      end else
                      if FExitPoint <> InvalidVal then
                      begin
                        FCurrentPosition := FExitPoint;
                      end else begin
                        FCurrentPosition := p;
                      end;
                    end;
                  end;
                1:
                  begin
                    pp := FExceptionStack.Data^[FExceptionStack.Count-1];
                    if pp = nil then begin
                      CMD_Err2(erOutOfRange, TbtString(RPS_OutOfRange));
                      Break;
                    end;
                    if (ExEx <> ENoError) and (pp.ExceptOffset <> InvalidVal) and (pp.ExceptOffset <> InvalidVal-1) then
                    begin
                      FCurrentPosition := pp.ExceptOffset;
                      pp.ExceptOffset := Cardinal(InvalidVal-1);
                      pp.ExceptionData := ExEx;
                      pp.ExceptionObject := ExObject;
                      pp.ExceptionParam := ExParam;
                      ExEx := ErNoError;
                      ExObject := nil;
                    end else if (pp.Finally2Offset <> InvalidVal) then
                    begin
                      FCurrentPosition := pp.Finally2Offset;
                      pp.Finally2Offset := InvalidVal;
                    end else begin
                      p := pp.EndOfBlock;
                      pp.Free;
                      FExceptionStack.DeleteLast;
                      if (ExEx <> eNoError) and (p <> InvalidVal) then
                      begin
                        Tmp := ExObject;
                        ExObject := nil;
                        ExceptionProc(ExProc, ExPos, ExEx, ExParam, Tmp);
                      end else
                      if FExitPoint <> InvalidVal then
                      begin
                        FCurrentPosition := FExitPoint;
                      end else begin
                        FCurrentPosition := p;
                      end;
                    end;
                  end;
                3:
                  begin
                    pp := FExceptionStack.Data^[FExceptionStack.Count-1];
                    if pp = nil then begin
                      CMD_Err2(erOutOfRange, TbtString(RPS_OutOfRange));
                      Break;
                    end;
                    p := pp.EndOfBlock;
                    pp.Free;
                    FExceptionStack.DeleteLast;
                    if ExEx <> eNoError then
                    begin
                      Tmp := ExObject;
                      ExObject := nil;
                      ExceptionProc(ExProc, ExPos, ExEx, ExParam, Tmp);
                    end else
                    if FExitPoint <> InvalidVal then
                    begin
                      FCurrentPosition := FExitPoint;
                    end else begin
                      FCurrentPosition := p;
                    end;
                 end;
              end;
            end;
          cm_spc:
            begin
              if not ReadVariable(vd, False) then
                Break;
              if vd.FreeType <> vtNone then
              begin
                FTempVars.Pop;
                CMD_Err2(erInvalidOpcodeParameter, TbtString(RPS_InvalidOpcodeParameter));
                break;
              end;
              if (Vd.aType.BaseType <> btPointer) then
              begin
                CMD_Err2(erInvalidOpcodeParameter, TbtString(RPS_InvalidOpcodeParameter));
                break;
              end;
              if not ReadVariable(vs, False) then
                Break;
              if Pointer(Pointer(IPointer(vD.P)+PointerSize2)^) <> nil then
                DestroyHeapVariant2(Pointer(vD.P^), Pointer(Pointer(IPointer(vd.P)+PointerSize)^));
              if vs.aType.BaseType = btPointer then
              begin
                if Pointer(vs.P^) <> nil then
                begin
                  Pointer(vd.P^) := CreateHeapVariant2(Pointer(Pointer(IPointer(vs.P) + PointerSize)^));
                  Pointer(Pointer(IPointer(vd.P) + PointerSize)^) := Pointer(Pointer(IPointer(vs.P) + PointerSize)^);
                  Pointer(Pointer(IPointer(vd.P) + PointerSize2)^) := Pointer(1);
                  if not CopyArrayContents(Pointer(vd.P^), Pointer(vs.P^), 1, Pointer(Pointer(IPointer(vd.P) + PointerSize)^)) then
                  begin
                    if vs.FreeType <> vtNone then
                      FTempVars.Pop;
                    CMD_Err2(erTypeMismatch, TbtString(RPS_TypeMismatch));
                    break;
                  end;
                end else
                begin
                  Pointer(vd.P^) := nil;
                  Pointer(Pointer(IPointer(vd.P) + PointerSize)^) := nil;
                  Pointer(Pointer(IPointer(vd.P) + PointerSize2)^) := nil;
                end;
              end else begin
                Pointer(vd.P^) := CreateHeapVariant2(vs.aType);
                Pointer(Pointer(IPointer(vd.P) + PointerSize)^) := vs.aType;
                LongBool(Pointer(IPointer(vd.P) + PointerSize2)^) := True;
                if not CopyArrayContents(Pointer(vd.P^), vs.P, 1, vs.aType) then
                begin
                  if vs.FreeType <> vtNone then
                    FTempVars.Pop;
                  CMD_Err2(erTypeMismatch, TbtString(RPS_TypeMismatch));
                  break;
                end;
              end;
              if vs.FreeType <> vtNone then
                FTempVars.Pop;

            end;
          cm_nop:;
          cm_dec:
            begin
              if not ReadVariable(vd, True) then
                Break;
              if vd.FreeType <> vtNone then
              begin
                FTempVars.Pop;
                CMD_Err2(erInvalidOpcodeParameter, TbtString(RPS_InvalidOpcodeParameter));
                break;
              end;
              case vd.aType.BaseType of
                btu8: dec(tbtu8(vd.P^));
                bts8: dec(tbts8(vd.P^));
                btu16: dec(tbtu16(vd.P^));
                bts16: dec(tbts16(vd.P^));
                btu32: dec(tbtu32(vd.P^));
                bts32: dec(tbts32(vd.P^));
                {$IFNDEF PS_NOINT64}
                bts64: dec(tbts64(vd.P^));
                {$ENDIF}
              else
                begin
                  CMD_Err2(erTypeMismatch, TbtString(RPS_TypeMismatch));
                  Break;
                end;
              end;
            end;
          cm_inc:
            begin
              if not ReadVariable(vd, True) then
                Break;
              if vd.FreeType <> vtNone then
              begin
                FTempVars.Pop;
                CMD_Err2(erInvalidOpcodeParameter, TbtString(RPS_InvalidOpcodeParameter));
                break;
              end;
              case vd.aType.BaseType of
                btu8: Inc(tbtu8(vd.P^));
                bts8: Inc(tbts8(vd.P^));
                btu16: Inc(tbtu16(vd.P^));
                bts16: Inc(tbts16(vd.P^));
                btu32: Inc(tbtu32(vd.P^));
                bts32: Inc(tbts32(vd.P^));
                {$IFNDEF PS_NOINT64}
                bts64: Inc(tbts64(vd.P^));
                {$ENDIF}
              else
                begin
                  CMD_Err2(erTypeMismatch, TbtString(RPS_TypeMismatch));
                  Break;
                end;
              end;
            end;
          cm_sp:
            begin
              if not ReadVariable(vd, False) then
                Break;
              if vd.FreeType <> vtNone then
              begin
                FTempVars.Pop;
                CMD_Err2(erInvalidOpcodeParameter, TbtString(RPS_InvalidOpcodeParameter));
                break;
              end;
              if (Vd.aType.BaseType <> btPointer) then
              begin
                CMD_Err2(erInvalidOpcodeParameter, TbtString(RPS_InvalidOpcodeParameter));
                Break;
              end;
              if not ReadVariable(vs, False) then
                Break;
              if vs.FreeType <> vtNone then
              begin
                FTempVars.Pop;
                CMD_Err2(erInvalidOpcodeParameter, TbtString(RPS_InvalidOpcodeParameter));
                Break;
              end;
              if vs.aType.BaseType = btPointer then
              begin
                Pointer(vd.P^) := Pointer(vs.p^);
                Pointer(Pointer(IPointer(vd.P)+PointerSize)^) := Pointer(Pointer(IPointer(vs.P)+PointerSize)^);
              end
              else
              begin
                Pointer(vd.P^) := vs.P;
                Pointer(Pointer(IPointer(vd.P)+PointerSize)^) := vs.aType;
              end;
            end;
          Cm_cv:
            begin
              if not ReadVariable(vd, True) then
                Break;
              if vd.aType.BaseType <> btProcPtr then
              begin
                if vd.FreeType <> vtNone then
                  FTempVars.Pop;
                CMD_Err2(erTypeMismatch, TbtString(RPS_TypeMismatch));
                break;
              end;
              p := tbtu32(vd.P^);
              if vd.FreeType <> vtNone then
                FTempVars.Pop;
              if (p = 0) and (Pointer(Pointer(IPointer(vd.p)+PointerSize2)^) <> nil) then
              begin
                if not InvokeExternalMethod(TPSTypeRec_ProcPtr(vd.aType),
                  Pointer(Pointer(IPointer(vd.p)+PointerSize)^),
                  Pointer(Pointer(IPointer(vd.p)+PointerSize2)^)
                ) then
                  Break;
              end else begin
                if (p >= FProcs.Count) or (p = FMainProc) then begin
                  CMD_Err2(erOutOfProcRange, TbtString(RPS_OutOfProcRange));
                  break;
                end;
                u := FProcs.Data^[p];
                if u.ClassType = TPSExternalProcRec then begin
                  try
                    if not TPSExternalProcRec(u).ProcPtr(Self, TPSExternalProcRec(u), FGlobalVars, FStack) then begin
                      if ExEx = erNoError then
                        //CMD_Err2(erCouldNotCallProc, TbtString(RPS_CouldNotCallProc));
                      {+}
                      begin // @dbg: GetCurrentPositionDebugInfo()
                        if FExceptionStack.Count = 0 then
                          CMD_Err2(erCouldNotCallProc, TbtString(RPS_CouldNotCallProc))
                        else
                        begin
                          pp := FExceptionStack.Data[FExceptionStack.Count-1];
                          if pp.ExceptionParam = '' then
                            CMD_Err2(erCustomError, TbtString(RPS_CouldNotCallProc) + TbtString(': ') + TPSExternalProcRec(u).fName)
                          else
                          begin
                            //-if TbtString(RPS_CouldNotCallProc) <> pp.ExceptionParam then
                            if Pos(RPS_CouldNotCallProc, string(pp.ExceptionParam)) <= 0 then
                              CMD_Err2(erCustomError, TbtString(RPS_CouldNotCallProc) + TbtString(': ') + pp.ExceptionParam)
                            else
                              CMD_Err2(erCustomError, TbtString(RPS_CouldNotCallProc));
                          end;
                        end;
                      end;
                      {+.}
                      Break;
                    end;
                  except
                    {$IFDEF DELPHI6UP}
                    Tmp := AcquireExceptionObject;
                    {$ELSE}
                    if RaiseList <> nil then
                    begin
                      Tmp := Exception(PRaiseFrame(RaiseList)^.ExceptObject);
                      PRaiseFrame(RaiseList)^.ExceptObject := nil;
                    end else
                      Tmp := nil;
                    {$ENDIF !DELPHI6UP}
                    if Tmp <> nil then
                    begin
                      if Tmp is EPSException then
                      begin
                        ExceptionProc(EPSException(tmp).ProcNo, EPSException(tmp).ProcPos, erCustomError, TbtString(EPSException(tmp).Message), nil);
                        break;
                      end else
                      if Tmp is EDivByZero then
                      begin
                        CMD_Err3(erDivideByZero, TbtString(Exception(Tmp).Message), Tmp);
                        break;
                      end;
                      if Tmp is EZeroDivide then
                      begin
                        CMD_Err3(erDivideByZero, TbtString(Exception(Tmp).Message), Tmp);
                        break;
                      end;
                      if Tmp is EMathError then
                      begin
                        CMD_Err3(erMathError, TbtString(Exception(Tmp).Message), Tmp);
                        break;
                      end;
                    end;
                    if (Tmp <> nil) and (Tmp is {+}ExceptionBase{+.}) then
                      CMD_Err3(erException, TbtString(Exception(Tmp).Message), Tmp) else
                      CMD_Err3(erException, '', Tmp);
                    Break;
                  end;
                end
                else begin
                  vtemp := FStack.PushType(FReturnAddressType);
                  PPSVariantReturnAddress(vtemp).Addr.ProcNo := FCurrProc;
                  PPSVariantReturnAddress(vtemp).Addr.Position := FCurrentPosition;
                  PPSVariantReturnAddress(vtemp).Addr.StackBase := FCurrStackBase;
                  FCurrStackBase := FStack.Count - 1;
                  FCurrProc := TPSInternalProcRec(u);
                  FData := FCurrProc.Data;
                  FDataLength := FCurrProc.Length;
                  FCurrentPosition := 0;
                end;
              end;
            end;
          CM_CO:
            begin
              if FCurrentPosition >= FDataLength then
              begin
                CMD_Err2(erOutOfRange, TbtString(RPS_OutOfRange)); // Error
                break;
              end;
              calctype := FData[FCurrentPosition];
              Inc(FCurrentPosition);
              if not ReadVariable({%H-}v3, True) then
                Break;
              if v3.FreeType <> vtNone then
              begin
                if v3.aType.BaseType in NeedFinalization then
                  FinalizeVariant(v3.P, v3.aType);
                p := IPointer(FTempVars.Data^[FtempVars.Count-1]) - IPointer(FtempVars.DataPtr);
                Dec(FTempVars.FCount);
                {$IFNDEF PS_NOSMARTLIST}
                Inc(FTempVars.FCheckCount);
                if FTempVars.FCheckCount > FMaxCheckCount then FTempVars.Recreate;
                {$ENDIF}
                FTempVars.FLength := P;
                if ((FTempVars.FCapacity - FTempVars.FLength) shr 12) > 2 then FTempVars.AdjustLength;
                CMD_Err2(erInvalidOpcodeParameter, TbtString(RPS_InvalidOpcodeParameter));
                break;
              end;
              if not ReadVariable(vs, True) then
                Break;
              if not ReadVariable(vd, True) then
              begin
                if vs.FreeType <> vtNone then
                begin
                  if vs.aType.BaseType in NeedFinalization then
                    FinalizeVariant(vs.P, vs.aType);
                  p := IPointer(FTempVars.Data^[FtempVars.Count-1]) - IPointer(FtempVars.DataPtr);
                  Dec(FTempVars.FCount);
                  {$IFNDEF PS_NOSMARTLIST}
                  Inc(FTempVars.FCheckCount);
                  if FTempVars.FCheckCount > FMaxCheckCount then FTempVars.Recreate;
                  {$ENDIF}
                  FTempVars.FLength := P;
                  if ((FTempVars.FCapacity - FTempVars.FLength) shr 12) > 2 then FTempVars.AdjustLength;
                end;
                Break;
              end;
              DoBooleanCalc(Vs.P, Vd.P, v3.P, vs.aType, vd.aType, v3.aType, CalcType);
              if vd.FreeType <> vtNone then
              begin
                if vd.aType.BaseType in NeedFinalization then
                  FinalizeVariant(vd.P, vd.aType);
                p := IPointer(FTempVars.Data^[FtempVars.Count-1]) - IPointer(FtempVars.DataPtr);
                Dec(FTempVars.FCount);
                {$IFNDEF PS_NOSMARTLIST}
                Inc(FTempVars.FCheckCount);
                if FTempVars.FCheckCount > FMaxCheckCount then FTempVars.Recreate;
                {$ENDIF}
                FTempVars.FLength := P;
                if ((FTempVars.FCapacity - FTempVars.FLength) shr 12) > 2 then FTempVars.AdjustLength;
              end;
              if vs.FreeType <> vtNone then
              begin
                if vs.aType.BaseType in NeedFinalization then
                  FinalizeVariant(vs.P, vs.aType);
                p := IPointer(FTempVars.Data^[FtempVars.Count-1]) - IPointer(FtempVars.DataPtr);
                Dec(FTempVars.FCount);
                {$IFNDEF PS_NOSMARTLIST}
                Inc(FTempVars.FCheckCount);
                if FTempVars.FCheckCount > FMaxCheckCount then FTempVars.Recreate;
                {$ENDIF}
                FTempVars.FLength := P;
                if ((FTempVars.FCapacity - FTempVars.FLength) shr 12) > 2 then FTempVars.AdjustLength;
              end;
            end;

        else
          begin
            {+}
            //-CMD_Err2(erInvalidOpcodeParameter, TbtString(RPS_InvalidOpcodeParameter)); // Error
            //-Exit;

            //@dbg: LoadDebugInfo();  FCurrentRow;  FCurrentFile
            // @dbg: TPSInternalProcRec(FProcs[ExProc]).FExportName
            // @dbg: TPSExceptionHandler(FExceptionStack.Data[FExceptionStack.Count-1]).ExceptionParam
            // @dbg: GetCurrentPositionDebugInfo();
            if (ExceptionCode <> erNoError) and (ExceptionCode <> erInvalidOpcodeParameter) then begin
              sError := PSErrorToString(ExceptionCode, ExceptionString)
                + '; ' + TbtString(RPS_InvalidOpcodeParameter);
              //raise EPSException.Create(sError + GetCurrentPositionDebugInfo('; '), Self, ExProc, ExPos);
              CMD_Err2(ExceptionCode, sError); // Error
            end else begin
              sError := TbtString(RPS_InvalidOpcodeParameter);
              //raise EPSException.Create(sError + GetCurrentPositionDebugInfo('; '), Self, ExProc, ExPos);
              CMD_Err2(erInvalidOpcodeParameter, sError); // Error
            end;
            {+.}
          end;
        end;
    end;
//    if cmd <> invalidval then ProfilerExitProc(Cmd+1);
//    if ExEx <> erNoError then FStatus := FOldStatus;
  until (FExceptionStack.Count = 0) or (Fstatus <> IsRunning);
  if FStatus = isLoaded then begin
    for I := Longint(FStack.Count) - 1 downto 0 do
      FStack.Pop;
    FStack.Clear;
    if FCallCleanup then Cleanup;
  end;
  Result := (ExEx = erNoError);
end; // function TPSExec.RunScript

function NVarProc(Caller: TPSExec; p: TPSExternalProcRec; {%H-}Global, Stack: TPSStack): Boolean;
var
  tmp: TPSVariantIFC;
begin
  case {+}NativeUInt{+.}(p.Ext1) of
    0:
      begin
        if @Caller.FOnSetNVariant = nil then begin
          Result := False;
          Exit;
        end;
        tmp := NewTPSVariantIFC(Stack.Items[Stack.Count - 2], True);
        if (Tmp.Dta = nil) or (tmp.aType.BaseType <> btVariant) then begin
          Result := False;
          Exit;
        end;
        Caller.FOnSetNVariant(Caller, Stack.GetAnsiString(-1), Variant(tmp.Dta^));
        Result := True;
      end;
    1:
      begin
        if @Caller.FOnGetNVariant = nil then begin
          Result := False;
          Exit;
        end;
        tmp := NewTPSVariantIFC(Stack.Items[Stack.Count - 1], False);
        if (Tmp.Dta = nil) or (tmp.aType.BaseType <> btVariant) then begin
          Result := False;
          Exit;
        end;
        Variant(tmp.Dta^) := Caller.FOnGetNVariant(Caller, Stack.GetAnsiString(-2));
        Result := True;
      end;
    else
      Result := False;
  end; // case
end; // function NVarProc

function DefProc(Caller: TPSExec; p: TPSExternalProcRec; {%H-}Global, Stack: TPSStack): Boolean;
var
  temp: TPSVariantIFC;
  I: Longint;
  b: Boolean;
  pex: TPSExceptionHandler;
  Tmp: TObject;
  SI, SI2: PPSVariant;
  C: Cardinal; AReplaceFlags: TReplaceFlags absolute C;
begin
  { The following needs to be in synch in these 3 functions:
    -UPSCompiler.TPSPascalCompiler.DefineStandardProcedures
    -UPSRuntime.DefProc
    -UPSRuntime.TPSExec.RegisterStandardProcs
  }
  case {+}NativeUInt{+.}(p.Ext1) of
    0: // IntToStr
      begin
        //{$if btCharIsWide}
        //Stack.SetUnicodeString(-1, TbtString(SysUtils.IntToStr(
        //  Stack.{$IFNDEF PS_NOINT64}GetInt64{$ELSE}GetInt{$ENDIF}(-2))));
        //{$else}
        Stack.SetAnsiString(-1, TbtString(SysUtils.IntToStr(
          Stack.{$IFNDEF PS_NOINT64}GetInt64{$ELSE}GetInt{$ENDIF}(-2))));
        //{$ifend}
      end;
    1: // StrToInt
      begin
        //{$if btCharIsWide}
        //Stack.SetInt(-1, StrToInt(Stack.GetUnicodeString(-2)));
        //{$else}
        Stack.SetInt(-1, StrToInt(Stack.GetAnsiString(-2)));
        //{$ifend}
      end;
    2: // StrTointDef
      begin
        //{$if btCharIsWide}
        //Stack.SetInt(-1, StrToIntDef(Stack.GetUnicodeString(-2), Stack.GetInt(-3)));
        //{$else}
        Stack.SetInt(-1, StrToIntDef(Stack.GetAnsiString(-2), Stack.GetInt(-3)));
        //{$ifend}
      end;
    3: // Pos
      begin
        SI := Stack.GetItem(Stack.Count-3); // analysis "BaseType" of the second parameter "p2" ( "Pos(p1, p2)" )
        case SI.FType.BaseType of
          btUnicodeString:
            I := uPSUtils.PosU(Stack.GetUnicodeString(-2), Stack.GetUnicodeString(-3));
          {$IFNDEF PS_NOWIDESTRING}
          btWideString, btWideChar:
            I := uPSUtils.PosW(Stack.GetWideString(-2), Stack.GetWideString(-3));
          {$ENDIF !PS_NOWIDESTRING}
          else begin // tsString, btChar
            {$if btCharIsWide}
            I := uPSUtils.PosU(Stack.GetUnicodeString(-2), Stack.GetUnicodeString(-3));
            {$else}
            I :=  uPSUtils.PosA(Stack.GetAnsiString(-2), Stack.GetAnsiString(-3));
            {$ifend}
          end;
        end; // case
        Stack.SetInt(-1, I);
      end;
    45: // PosEx
      begin
        SI := Stack.GetItem(Stack.Count-3); // analysis "BaseType" of the second parameter "p2" ( "PosEx(p1, p2, p3)" )
        case SI.FType.BaseType of
          btUnicodeString:
            I := uPSUtils.PosExU(Stack.GetUnicodeString(-2), Stack.GetUnicodeString(-3), Stack.GetInt(-4));
          {$IFNDEF PS_NOWIDESTRING}
          btWideString, btWideChar:
            I := uPSUtils.PosExW(Stack.GetWideString(-2), Stack.GetWideString(-3), Stack.GetInt(-4));
          {$ENDIF PS_NOWIDESTRING}
          else begin // tsString, btChar
            {$if btCharIsWide}
            I := uPSUtils.PosExU(Stack.GetUnicodeString(-2), Stack.GetUnicodeString(-3), Stack.GetInt(-4));
            {$else}
            I := uPSUtils.PosExA(Stack.GetAnsiString(-2), Stack.GetAnsiString(-3), Stack.GetInt(-4));
            {$ifend}
          end;
        end; // case
        Stack.SetInt(-1, I);
      end;
    4: // Copy
      begin
        SI := Stack.GetItem(Stack.Count-2);
        case SI.FType.BaseType of
          btUnicodeString:
          Stack.SetUnicodeString(-1, Copy(Stack.GetUnicodeString(-2), Stack.GetInt(-3), Stack.GetInt(-4)));
          {$IFNDEF PS_NOWIDESTRING}
          btWideChar, btWideString:
            Stack.SetWideString(-1, Copy(Stack.GetWideString(-2), Stack.GetInt(-3), Stack.GetInt(-4)));
          {$ENDIF !PS_NOWIDESTRING}
          else begin // tsString, btChar
            {$if btCharIsWide}
            Stack.SetUnicodeString(-1, Copy(Stack.GetUnicodeString(-2), Stack.GetInt(-3), Stack.GetInt(-4)))
            {$else}
            Stack.SetAnsiString(-1, Copy(Stack.GetAnsiString(-2), Stack.GetInt(-3), Stack.GetInt(-4)));
            {$ifend}
          end;
        end; // case
      end;
    5: // Delete
      begin
        temp := NewTPSVariantIFC(Stack[Stack.Count-1], True);
        if (temp.Dta = nil) then begin
          Result := False;
          Exit;
        end;
        case temp.aType.BaseType of
          btUnicodeString:
            Delete(TbtUnicodeString(temp.Dta^), Stack.GetInt(-2), Stack.GetInt(-3));
          {$IFNDEF PS_NOWIDESTRING}
          btWideString:
            Delete(tbtWideString(temp.Dta^), Stack.GetInt(-2), Stack.GetInt(-3));
          {$ENDIF !PS_NOWIDESTRING}
          btString:
            {$if btCharIsWide}
            Delete(TbtUnicodeString(temp.Dta^), Stack.GetInt(-2), Stack.GetInt(-3));
            {$else}
            Delete(TbtString(temp.Dta^), Stack.GetInt(-2), Stack.GetInt(-3));
            {$ifend}
          else begin
            Result := False;
            Exit;
          end;
        end; // case
      end;
    6: // Insert
      begin
        temp := NewTPSVariantIFC(Stack[Stack.Count-2], True);
        if (temp.Dta = nil) then begin
          Result := False;
          Exit;
        end;
        case temp.aType.BaseType of
          btUnicodeString:
            Insert(Stack.GetUnicodeString(-1), TbtUnicodeString(temp.Dta^), Stack.GetInt(-3));
          {$IFNDEF PS_NOWIDESTRING}
          btWideString:
            Insert(Stack.GetWideString(-1), tbtwidestring(temp.Dta^), Stack.GetInt(-3));
          {$ENDIF !PS_NOWIDESTRING}
          btString:
            {$if btCharIsWide}
            Insert(Stack.GetUnicodeString(-1), TbtUnicodeString(temp.Dta^), Stack.GetInt(-3));
            {$else}
            Insert(Stack.GetAnsiString(-1), TbtString(temp.Dta^), Stack.GetInt(-3));
            {$ifend}
          else begin
            Result := False;
            Exit;
          end;
        end; // case
      end;
    7: // StrGet
      begin
        temp :=  NewTPSVariantIFC(Stack[Stack.Count-2], True);
        if (temp.Dta = nil) or (not (temp.aType.BaseType in [btString, btUnicodeString
            {$IFNDEF PS_NOWIDESTRING},btWideString{$ENDIF}
          ])) then
        begin
          Result := False;
          Exit;
        end;
        I := Stack.GetInt(-3);
        if (i<1) then begin
          Caller.CMD_Err2(erCustomError, TbtString(RPS_OutOfStringRange));
          Result := False;
          Exit;
        end;
        case temp.aType.BaseType of
          btUnicodeString:
            begin
              if (i>length(TbtUnicodeString(temp.Dta^))) then begin
                Caller.CMD_Err2(erCustomError, TbtString(RPS_OutOfStringRange));
                Result := False;
                Exit;
              end;
              Stack.SetInt(-1, Ord(TbtUnicodeString(temp.Dta^)[i]));
            end;
          {$IFNDEF PS_NOWIDESTRING}
          btWideString:
            begin
              if (i>length(TbtWideString(temp.Dta^))) then begin
                Caller.CMD_Err2(erCustomError, TbtString(RPS_OutOfStringRange));
                Result := False;
                Exit;
              end;
              Stack.SetInt(-1, Ord(TbtWideString(temp.Dta^)[i]));
            end;
          {$ENDIF PS_NOWIDESTRING}
          btString:
            begin
              {$if btCharIsWide}
              if (i>length(TbtUnicodeString(temp.Dta^))) then begin
                Caller.CMD_Err2(erCustomError, TbtString(RPS_OutOfStringRange));
                Result := False;
                Exit;
              end;
              Stack.SetInt(-1, Ord(TbtUnicodeString(temp.Dta^)[i]));
              {$else}
              if (i>length(TbtString(temp.Dta^))) then begin
                Caller.CMD_Err2(erCustomError, TbtString(RPS_OutOfStringRange));
                Result := False;
                Exit;
              end;
              Stack.SetInt(-1, Ord(TbtString(temp.Dta^)[i]));
              {$ifend}
            end;
          else begin
            Result := False;
            Exit;
          end;
        end; // case
      end;
    8: // StrSet
      begin
        temp := NewTPSVariantIFC(Stack[Stack.Count -3], True);
        if (temp.Dta = nil) or (not (temp.aType.BaseType in [btString, btUnicodeString
            {$IFNDEF PS_NOWIDESTRING},btWideString{$ENDIF}
          ])) then
        begin
          Result := False;
          Exit;
        end;
        {
        if (i<1) or (i > Length(TbtString(temp.Dta^))) then begin
          Caller.CMD_Err2(erCustomError, TbtString(RPS_OutOfStringRange));
          Result := True;
          Exit;
        end;
        TbtString(temp.Dta^)[i] := TbtChar(Stack.GetInt(-1));
        }
        I := Stack.GetInt(-2); // index value
        if (i < 1) then begin
          Caller.CMD_Err2(erCustomError, TbtString(RPS_OutOfStringRange));
          Result := True;
          Exit;
        end;
        case temp.aType.BaseType of
          btUnicodeString:
            begin
              if (i > Length(TbtUnicodeString(temp.Dta^))) then begin
                Caller.CMD_Err2(erCustomError, TbtString(RPS_OutOfStringRange));
                Result := True;
                Exit;
              end;
              TbtUnicodeString(temp.Dta^)[i] := WideChar(Word(Stack.GetInt(-1)));
           end;
          {$IFNDEF PS_NOWIDESTRING}
          btWideString:
            begin
              if (i > Length(TbtWideString(temp.Dta^))) then begin
                Caller.CMD_Err2(erCustomError, TbtString(RPS_OutOfStringRange));
                Result := True;
                Exit;
              end;
              TbtWideString(temp.Dta^)[i] := WideChar(Word(Stack.GetInt(-1)));
            end;
          {$ENDIF !PS_NOWIDESTRING}
          btString:
            begin
              {$if btCharIsWide}
              TbtUnicodeString(temp.Dta^)[i] := WideChar(Word(Stack.GetInt(-1)));
              {$else}
              TbtString(temp.Dta^)[i] := TbtChar(Stack.GetInt(-1));
              {$ifend}
            end;
          else begin
            Result := True;
            Exit;
          end;
        end; // case
      end;
    10: // UpperCase
      begin
        SI := Stack.GetItem(Stack.Count-2);
        case SI.FType.BaseType of
          btUnicodeString:
            Stack.SetUnicodeString(-1, UpperCase(Stack.GetUnicodeString(-2)));
          {$IFNDEF PS_NOWIDESTRING}
          btWideString, btWideChar:
            Stack.SetWideString(-1, WideUpperCase(Stack.GetWideString(-2)));
          {$ENDIF !PS_NOWIDESTRING}
          else begin // tsString, btChar
            {$if btCharIsWide}
            Stack.SetUnicodeString(-1, UpperCase(Stack.GetUnicodeString(-2)));
            {$else}
            Stack.SetAnsiString(-1, FastUpperCase(Stack.GetAnsiString(-2)));
            {$ifend}
          end;
        end; // case
      end;
    11: // LowerCase
      begin
        SI := Stack.GetItem(Stack.Count-2);
        case SI.FType.BaseType of
          btUnicodeString:
            Stack.SetUnicodeString(-1, LowerCase(Stack.GetUnicodeString(-2)));
          {$IFNDEF PS_NOWIDESTRING}
          btWideString, btWideChar:
            Stack.SetWideString(-1, WideLowerCase(Stack.GetWideString(-2)));
          {$ENDIF !PS_NOWIDESTRING}
          else begin // tsString, btChar
            {$if btCharIsWide}
            Stack.SetUnicodeString(-1, LowerCase(Stack.GetUnicodeString(-2)));
            {$else}
            Stack.SetAnsiString(-1, FastLowercase(Stack.GetAnsiString(-2)));
            {$ifend}
          end;
        end; // case
      end;
    12: // function Trim(S: AnyString): AnyString;
      begin
        SI := Stack.GetItem(Stack.Count-2);
        case SI.FType.BaseType of
          btUnicodeString:
            Stack.SetUnicodeString(-1, SysUtils.Trim(Stack.GetUnicodestring(-2)));
          {$IFNDEF PS_NOWIDESTRING}
          btWideString, btWideChar:
            Stack.SetWideString(-1, SysUtils.Trim(Stack.GetWideString(-2)));
          {$ENDIF !PS_NOWIDESTRING}
          else begin // tsString, btChar
            {$if btCharIsWide}
            Stack.SetUnicodeString(-1, SysUtils.Trim(Stack.GetUnicodestring(-2)));
            {$else}
            Stack.SetAnsiString(-1, AnsiString(SysUtils.Trim(String(Stack.GetAnsiString(-2)))));
            {$ifend}
          end;
        end; // case
      end;
    {+}
    46: // function TrimLeft(S: AnyString): AnyString;
      begin
        SI := Stack.GetItem(Stack.Count-2);
        case SI.FType.BaseType of
          btUnicodeString:
            Stack.SetUnicodeString(-1, SysUtils.TrimLeft(Stack.GetUnicodestring(-2)));
          {$IFNDEF PS_NOWIDESTRING}
          btWideString, btWideChar:
            Stack.SetWideString(-1, SysUtils.TrimLeft(Stack.GetWideString(-2)))
          {$ENDIF !PS_NOWIDESTRING}
          else begin // tsString, btChar
            {$if btCharIsWide}
            Stack.SetUnicodeString(-1, SysUtils.TrimLeft(Stack.GetUnicodestring(-2)));
            {$else}
            Stack.SetAnsiString(-1, AnsiString(SysUtils.TrimLeft(String(Stack.GetAnsiString(-2)))));
            {$ifend}
          end;
        end; // case
      end;
    47: // function TrimRight(S: AnyString): AnyString;
      begin
        SI := Stack.GetItem(Stack.Count-2);
        case SI.FType.BaseType of
          btUnicodeString:
            Stack.SetUnicodeString(-1, SysUtils.TrimRight(Stack.GetUnicodestring(-2)));
          {$IFNDEF PS_NOWIDESTRING}
          btWideString, btWideChar:
            Stack.SetWideString(-1, SysUtils.TrimRight(Stack.GetWideString(-2)));
          {$ENDIF !PS_NOWIDESTRING}
          else begin // tsString, btChar
            {$if btCharIsWide}
            Stack.SetUnicodeString(-1, SysUtils.TrimRight(Stack.GetUnicodestring(-2)));
            {$else}
            Stack.SetAnsiString(-1, AnsiString(SysUtils.TrimRight(String(Stack.GetAnsiString(-2)))));
            {$ifend}
          end;
        end; // case
      end;
    {+.}
    13: // Length
      begin
        //Stack.SetInt(-1, Length(Stack.GetAnsiString(-2)));
        SI := Stack.GetItem(Stack.Count-2);
        case SI.FType.BaseType of
          btChar:
            Stack.SetInt(-1, 1);
          btUnicodeString:
            Stack.SetInt(-1, Length(Stack.GetUnicodeString(-2)));
          {$IFNDEF PS_NOWIDESTRING}
          btWideString:
            Stack.SetInt(-1, Length(Stack.GetWideString(-2)));
          btWideChar:
            Stack.SetInt(-1, 1);
          {$ENDIF PS_NOWIDESTRING}
          else begin // tsString, btChar
            {$if btCharIsWide}
            Stack.SetInt(-1, Length(Stack.GetUnicodeString(-2)));
            {$else}
            Stack.SetInt(-1, Length(Stack.GetAnsiString(-2)));
            {$ifend}
          end;
        end; // case
      end;
    14: // SetLength
      begin
        temp := NewTPSVariantIFC(Stack[Stack.Count-1], True);
        if (temp.Dta = nil) or (not (temp.aType.BaseType in [btString, btUnicodeString
            {$IFNDEF PS_NOWIDESTRING},btWideString{$ENDIF}
          ])) then
        begin
          Result := False;
          Exit;
        end;
        //SetLength(TbtString(temp.Dta^), STack.GetInt(-2));
        case temp.aType.BaseType of
          btUnicodeString:
            SetLength(TbtUnicodeString(temp.Dta^), STack.GetInt(-2));
          {$IFNDEF PS_NOWIDESTRING}
          btWideString:
            SetLength(TbtWideString(temp.Dta^), STack.GetInt(-2));
          {$ENDIF PS_NOWIDESTRING}
          else begin // tsString, btChar
            {$if btCharIsWide}
            SetLength(TbtUnicodeString(temp.Dta^), STack.GetInt(-2));
            {$else}
            SetLength(TbtString(temp.Dta^), STack.GetInt(-2));
            {$ifend}
          end;
        end; // case
      end;
    15: // Sin
      Stack.SetReal(-1, Sin(Stack.GetReal(-2)));
    16: // Cos
      Stack.SetReal(-1, Cos(Stack.GetReal(-2)));
    17: // Sqrt
      Stack.SetReal(-1, SQRT(Stack.GetReal(-2)));
    18: // Round
      Stack.SetInt(-1, Round(Stack.GetReal(-2)));
    {+}
    19: // Trunc
      //OLD: Stack.SetInt(-1, Trunc(Stack.GetReal(-2)));
      // NEW: Temporary:
      begin
        {$IFNDEF PS_HAVEVARIANT}
        Stack.SetReal(-1, Trunc(Stack.GetReal(-2)));
        {$ELSE  PS_HAVEVARIANT}
        temp := NewTPSVariantIFC(Stack[Stack.Count-1], True);
        if (temp.Dta <> nil) and (temp.aType.BaseType = btVariant) then
          Variant(temp.Dta^) := Trunc(Stack.GetReal(-2))
        else begin
          Result := False;
          Exit;
        end;
        {$ENDIF PS_HAVEVARIANT}
        {temp := NewTPSVariantIFC(Stack[Stack.Count-1], True);
        if (temp.Dta <> nil) and (temp.aType.BaseType in [btU32, btS32, btDouble, btExtended, btVariant]) then
        begin
          //Stack.SetReal(-1, Trunc(Stack.GetReal(-2)))
          case temp.aType.BaseType of
            btU32, btS32:
              tbtU32(temp.Dta^) := tbtU32(Trunc(Stack.GetReal(-2)));
            btDouble:
              tbtDouble(temp.Dta^) := Trunc(Stack.GetReal(-2));
            btExtended:
              tbtExtended(temp.Dta^) := Trunc(Stack.GetReal(-2));
            btVariant:
              Variant(temp.Dta^) := Trunc(Stack.GetReal(-2));
          end;
        end
        else
          Stack.SetInt(-1, Trunc(Stack.GetReal(-2)));//}
      end;
    {+.}
    20: // Int
      Stack.SetReal(-1, Int(Stack.GetReal(-2)));
    21: // Pi
      Stack.SetReal(-1, Pi);
    22: // Abs
      Stack.SetReal(-1, Abs(Stack.GetReal(-2)));
    23: // StrToFloat
      begin
        {$if btCharIsWide}
        Stack.SetReal(-1, StrToFloat(Stack.GetUnicodeString(-2)));
        {$else}
        Stack.SetReal(-1, StrToFloat(Stack.GetAnsiString(-2)));
        {$ifend}
      end;
    24: // FloatToStr
      begin
        {$if btCharIsWide}
        Stack.SetUnicodeString(-1, FloatToStr(Stack.GetReal(-2)));
        {$else}
        Stack.SetAnsiString(-1, FloatToStr(Stack.GetReal(-2)));
        {$ifend}
      end;
    25: //  PadL
      begin
        SI := Stack.GetItem(Stack.Count-2);
        case SI.FType.BaseType of
          btUnicodeString:
            Stack.SetUnicodeString(-1, upadL(Stack.GetUnicodeString(-2), Stack.GetInt(-3)));
          {$IFNDEF PS_NOWIDESTRING}
          btWideChar, btWideString:
            Stack.SetWideString(-1, wPadL(Stack.GetWideString(-2), Stack.GetInt(-3)));
          {$ENDIF !PS_NOWIDESTRING}
          else begin
            {$if btCharIsWide}
            Stack.SetUnicodeString(-1, upadL(Stack.GetUnicodeString(-2), Stack.GetInt(-3)));
            {$else}
            Stack.SetAnsiString(-1, PadL(Stack.GetAnsiString(-2), Stack.GetInt(-3)));
            {$ifend}
          end;
        end; // case
      end;
    26: // PadR
      begin
        SI := Stack.GetItem(Stack.Count-2);
        case SI.FType.BaseType of
          btUnicodeString:
            Stack.SetUnicodeString(-1, uPadR(Stack.GetUnicodeString(-2), Stack.GetInt(-3)));
          {$IFNDEF PS_NOWIDESTRING}
          btWideChar, btWideString:
            Stack.SetWideString(-1, wPadR(Stack.GetWideString(-2), Stack.GetInt(-3)));
          {$ENDIF !PS_NOWIDESTRING}
          else begin
            {$if btCharIsWide}
            Stack.SetUnicodeString(-1, uPadR(Stack.GetUnicodeString(-2), Stack.GetInt(-3)));
            {$else}
            Stack.SetAnsiString(-1, PadR(Stack.GetAnsiString(-2), Stack.GetInt(-3)));
            {$ifend}
          end;
        end; // case
      end;
    27: // PadZ
      begin
        SI := Stack.GetItem(Stack.Count-2);
        case SI.FType.BaseType of
          btUnicodeString:
            Stack.SetUnicodeString(-1, uPadZ(Stack.GetUnicodeString(-2), Stack.GetInt(-3)));
          {$IFNDEF PS_NOWIDESTRING}
          btWideChar, btWideString:
            Stack.SetWideString(-1, wPadZ(Stack.GetWideString(-2), Stack.GetInt(-3)));
          {$ENDIF !PS_NOWIDESTRING}
          else begin
            {$if btCharIsWide}
            Stack.SetUnicodeString(-1, uPadZ(Stack.GetUnicodeString(-2), Stack.GetInt(-3)));
            {$else}
            Stack.SetAnsiString(-1, PadZ(Stack.GetAnsiString(-2), Stack.GetInt(-3)));
            {$ifend}
          end;
        end; // case
      end;
    28: // Replicate/StrOfChar
      begin // function StringOfChar(ch: *Char; Count: Integer): *String; overload;
        {$IFNDEF PS_NOWIDESTRING}
        SI := Stack.GetItem(Stack.Count-2);
        if SI.FType.BaseType in [btWideChar, btWideString] then
          Stack.SetWideString(-1, StringOfChar(TbtWideChar(Stack.GetInt(-2)), Stack.GetInt(-3)))
        else
        {$ENDIF !PS_NOWIDESTRING}
        begin
          {$if btCharIsWide}
          Stack.SetUnicodeString(-1, StringOfChar(TbtChar(Stack.GetInt(-2)), Stack.GetInt(-3)));
          {$else}
          Stack.SetAnsiString(-1, StringOfChar(TbtChar(Stack.GetInt(-2)), Stack.GetInt(-3)));
          {$ifend}
        end;
      end;
    29: // Assigned
      begin
        temp := NewTPSVariantIFC(Stack[Stack.Count-2], True);
        if Temp.dta = nil then begin
          Result := False;
          Exit;
        end;
        case temp.aType.BaseType of
          btU8, btS8: b := tbtu8(temp.dta^) <> 0;
          btU16, btS16: b := tbtu16(temp.dta^) <> 0;
          btU32, btS32: b := tbtu32(temp.dta^) <> 0;
          btString, btPChar: b := Length(TbtString(temp.dta^)) > 0;
          {$IFNDEF PS_NOWIDESTRING}
          btWideString{+}{$if declared(btPWideChar)},btPWideChar{$ifend}{+.}:
            b := Length(TbtWideString(temp.dta^)) > 0;
          btUnicodeString: b := TbtUnicodeString(temp.dta^)<> '';
          {$ENDIF !PS_NOWIDESTRING}
          btArray, btClass{$IFNDEF PS_NOINTERFACES}, btInterface{$ENDIF}:
            b := Pointer(temp.dta^) <> nil;
          {+}
          btVariant:
            b := not VarIsEmptyOrNull(Variant(temp.dta^));
          {+.}
        else
          Result := False;
          Exit;
        end;
        if b then
          Stack.SetInt(-1, 1)
        else
          Stack.SetInt(-1, 0);
      end;
    30: // RaiseLastException
      begin
        if (Caller.FExceptionStack.Count > 0) then begin
          pex := Caller.FExceptionStack.Data[Caller.fExceptionStack.Count-1];
          if pex.ExceptOffset = Cardinal(InvalidVal-1) then begin
            Tmp := pex.ExceptionObject;
            pex.ExceptionObject := nil;
            Caller.ExceptionProc(Caller.ExProc, pex.ExceptOffset, pex.ExceptionData, pex.ExceptionParam, tmp);
          end;
        end;
      end;
    31: // RaiseExeption
      Caller.CMD_Err2(TPSError(Stack.GetInt(-1)), Stack.GetAnsiString(-2));
    32: // ExceptionType
      Stack.SetInt(-1, Ord(Caller.LastEx));
    33: // ExceptionParam
      Stack.SetAnsiString(-1, Caller.LastExParam);
    34: // ExceptionProc
      Stack.SetInt(-1, Caller.LastExProc);
    35: // ExceptionPos
      Stack.SetInt(-1, Caller.LastExPos);
    36: // ExceptionToString
      Stack.SetAnsiString(-1, PSErrorToString(TPSError(Stack.GetInt(-2)), Stack.GetAnsiString(-3)));
    37: // AnsiUppercase
      Stack.SetAnsiString(-1, TbtString(AnsiUpperCase(string(Stack.GetAnsiString(-2)))));
    38: // AnsiLowerCase
      Stack.SetAnsiString(-1, TbtString(AnsiLowercase(string(Stack.GetAnsiString(-2)))));
    {$IFNDEF PS_NOINT64}
    39: // StrToInt64
      Stack.SetInt64(-1, StrToInt64(string(Stack.GetAnsiString(-2))));
    40: // Int64ToStr
      Stack.SetAnsiString(-1, TbtString(SysUtils.IntToStr(Stack.GetInt64(-2))));
    41: // StrToInt64Def
      Stack.SetInt64(-1, StrToInt64Def(string(Stack.GetAnsiString(-2)), Stack.GetInt64(-3)));
    {$ENDIF !PS_NOINT64}
    42:  // SizeOf
      begin
        temp := NewTPSVariantIFC(Stack[Stack.Count -2], False);
        if Temp.aType = nil then
          Stack.SetInt(-1, 0)
        else
          Stack.SetInt(-1, Temp.aType.RealSize)
      end;
    {$IFNDEF PS_NOWIDESTRING}
    43: // WStrGet
      begin
        temp :=  NewTPSVariantIFC(Stack[Stack.Count -2], True);
        if temp.dta = nil then begin
          Result := False;
          Exit;
        end;
        case temp.aType.BaseType of
          btWideString:
            begin
              I := Stack.GetInt(-3);
              if (i<1) or (i>length(TbtWideString(temp.Dta^))) then begin
                Caller.CMD_Err2(erCustomError, TbtString(RPS_OutOfStringRange));
                Result := False;
                Exit;
              end;
              Stack.SetInt(-1, Ord(TbtWideString(temp.Dta^)[i]));
            end;
          btUnicodeString {$if btCharIsWide}, btString{$ifend}:
            begin
              I := Stack.GetInt(-3);
              if (i<1) or (i>length(TbtUnicodeString(temp.Dta^))) then begin
                Caller.CMD_Err2(erCustomError, TbtString(RPS_OutOfStringRange));
                Result := False;
                Exit;
              end;
              Stack.SetInt(-1, Ord(TbtUnicodeString(temp.Dta^)[i]));
            end;
          {$if not btCharIsWide}
          btString:
            begin
              I := Stack.GetInt(-3);
              if (i<1) or (i>length(TbtString(temp.Dta^))) then begin
                Caller.CMD_Err2(erCustomError, TbtString(RPS_OutOfStringRange));
                Result := False;
                Exit;
              end;
              Stack.SetInt(-1, Ord(TbtString(temp.Dta^)[i]));
            end;
          {$ifend}
          else begin
            Result := False;
            Exit;
          end;
        end; // case
      end;
    44: // WStrSet
      begin
        temp := NewTPSVariantIFC(Stack[Stack.Count -3], True);
        if (temp.Dta = nil) then begin
          Result := False;
          Exit;
        end;
        case temp.aType.BaseType of
          btWideString:
            begin
              I := Stack.GetInt(-2);
              if (i<1) or (i>length(TbtWidestring(temp.Dta^))) then begin
                Caller.CMD_Err2(erCustomError, TbtString(RPS_OutOfStringRange));
                Result := True;
                Exit;
              end;
              TbtWidestring(temp.Dta^)[i] := WideChar(Stack.GetInt(-1));
            end;
          btUnicodeString {$if btCharIsWide}, btString{$ifend}:
            begin
              I := Stack.GetInt(-2);
              if (i<1) or (i>length(TbtUnicodeString(temp.Dta^))) then begin
                Caller.CMD_Err2(erCustomError, TbtString(RPS_OutOfStringRange));
                Result := True;
                Exit;
              end;
              TbtUnicodeString(temp.Dta^)[i] := WideChar(Stack.GetInt(-1));
            end;
          {$if not btCharIsWide}
          btString:
            begin
              I := Stack.GetInt(-2);
              if (i<1) or (i>length(TbtString(temp.Dta^))) then begin
                Caller.CMD_Err2(erCustomError, TbtString(RPS_OutOfStringRange));
                Result := True;
                Exit;
              end;
              TbtString(temp.Dta^)[i] := TbtChar(Stack.GetInt(-1));
            end;
          {$ifend}
          else begin
            Result := False;
            Exit;
          end;
        end; // case
      end;
    {$ENDIF !PS_NOWIDESTRING}
    {+}
    48: // function TrimLen(S: AnyString): Integer;
      begin
        SI := Stack.GetItem(Stack.Count-2);
        if SI.FType.BaseType = btUnicodeString then
        begin
          {$if declared(TrimLenU)}
          I := uPSUtils.TrimLenU(Stack.GetUnicodeString(-2));
          {$else}
          I := uPSUtils.TrimLenW(Stack.GetUnicodeString(-2));
          {$ifend}
        end
        {$IFNDEF PS_NOWIDESTRING}
        else if SI.FType.BaseType in [btWideString, btWideChar] then
          I := uPSUtils.TrimLenW(Stack.GetWideString(-2))
        {$ENDIF !PS_NOWIDESTRING}
        else // tsString, btChar
        begin
          {$if btCharIsWide}
            {$if declared(TrimLenU)}
            I := uPSUtils.TrimLenU(Stack.GetUnicodeString(-2));
            {$else}
            I := uPSUtils.TrimLenW(Stack.GetUnicodeString(-2));
            {$ifend}
          {$else}
          I := uPSUtils.TrimLenA(Stack.GetAnsiString(-2));
          {$ifend}
        end;
        Stack.SetInt(-1, I);
      end;
    49: // function TrimLeftLen(S: AnyString): Integer;
      begin
        SI := Stack.GetItem(Stack.Count-2);
        case SI.FType.BaseType of
          btUnicodeString:
            begin
              {$if declared(TrimLeftLenU)}
              I := uPSUtils.TrimLeftLenU(Stack.GetUnicodeString(-2));
              {$else}
              I := uPSUtils.TrimLeftLenW(Stack.GetUnicodeString(-2));
              {$ifend}
            end;
          {$IFNDEF PS_NOWIDESTRING}
          btWideString, btWideChar:
            I := uPSUtils.TrimLeftLenW(Stack.GetWideString(-2));
          {$ENDIF !PS_NOWIDESTRING}
          else begin // tsString, btChar
            {$if btCharIsWide}
              {$if declared(TrimLeftLenU)}
              I := uPSUtils.TrimLeftLenU(Stack.GetUnicodeString(-2))
              {$else}
              I := uPSUtils.TrimLeftLenW(Stack.GetUnicodeString(-2))
              {$ifend}
            {$else}
            I := uPSUtils.TrimLeftLenA(Stack.GetAnsiString(-2));
            {$ifend}
          end;
        end; // case
        Stack.SetInt(-1, I);
      end;
    50: // function TrimRightLen(S: AnyString): Integer;
      begin
        SI := Stack.GetItem(Stack.Count-2);
        case SI.FType.BaseType of
          btUnicodeString:
            begin
              {$if declared(TrimRightLenU)}
              I := uPSUtils.TrimRightLenU(Stack.GetUnicodeString(-2));
              {$else}
              I := uPSUtils.TrimRightLenW(Stack.GetUnicodeString(-2));
              {$ifend}
            end;
          {$IFNDEF PS_NOWIDESTRING}
          btWideString, btWideChar:
            I := uPSUtils.TrimRightLenW(Stack.GetWideString(-2));
          {$ENDIF !PS_NOWIDESTRING}
          else begin // tsString, btChar
            {$if btCharIsWide}
              {$if declared(TrimRightLenU)}
              I := uPSUtils.TrimRightLenU(Stack.GetUnicodeString(-2));
              {$else}
              I := uPSUtils.TrimRightLenW(Stack.GetUnicodeString(-2));
              {$ifend}
            {$else}
            I := uPSUtils.TrimRightLenA(Stack.GetAnsiString(-2));
            {$ifend}
          end;
        end; // case
        Stack.SetInt(-1, I);
      end;
    {.$if declared(SameText)}
    51: // function SameText(S1, S2 : AnyString) : Boolean;
      begin
        SI := Stack.GetItem(Stack.Count-2);
        SI2 := Stack.GetItem(Stack.Count-3);
        if (SI.FType.BaseType = btUnicodeString) or (SI2.FType.BaseType = btUnicodeString) then
        begin
          {$if declared(SameTextU)}
          b := uPSUtils.SameTextU(Stack.GetUnicodeString(-2), Stack.GetUnicodeString(-3));
          {$else}
          b := uPSUtils.SameTextW(Stack.GetUnicodeString(-2), Stack.GetUnicodeString(-3));
          {$ifend}
        end
        {$IFNDEF PS_NOWIDESTRING}
        else if (SI.FType.BaseType in [btWideString, btWideChar])
          or (SI2.FType.BaseType in [btWideString, btWideChar]) then
          b := uPSUtils.SameTextW(Stack.GetWideString(-2), Stack.GetWideString(-3))
        {$ENDIF !PS_NOWIDESTRING}
        else begin // tsString, btChar
          {$if btCharIsWide}
            {$if declared(SameTextU)}
            b := uPSUtils.SameTextU(Stack.GetUnicodeString(-2), Stack.GetUnicodeString(-3));
            {$else}
            b := uPSUtils.SameTextW(Stack.GetUnicodeString(-2), Stack.GetUnicodeString(-3));
            {$ifend}
          {$else}
          b := {$IFDEF _ANSISTRINGS_}AnsiStrings.{$ENDIF}SameText(Stack.GetAnsiString(-2), Stack.GetAnsiString(-3));
          {$ifend}
        end;
        Stack.SetBool(-1, b);
      end;
    52: // function AnsiSameText(S1, S2 : AnyString) : Boolean;
      begin
        SI := Stack.GetItem(Stack.Count-2);
        SI2 := Stack.GetItem(Stack.Count-3);
        if (SI.FType.BaseType = btUnicodeString) or (SI2.FType.BaseType = btUnicodeString) then
        begin
          {$if declared(AnsiSameTextU)}
          b := uPSUtils.AnsiSameTextU(Stack.GetUnicodeString(-2), Stack.GetUnicodeString(-3));
          {$else}
          b := uPSUtils.AnsiSameTextW(Stack.GetUnicodeString(-2), Stack.GetUnicodeString(-3));
          {$ifend}
        end
        {$IFNDEF PS_NOWIDESTRING}
        else if (SI.FType.BaseType in [btWideString, btWideChar])
          or (SI2.FType.BaseType in [btWideString, btWideChar]) then
          b := uPSUtils.AnsiSameTextW(Stack.GetWideString(-2), Stack.GetWideString(-3))
        {$ENDIF !PS_NOWIDESTRING}
        else // tsString, btChar
        begin
          {$if btCharIsWide}
            {$if declared(AnsiSameTextU)}
            b := uPSUtils.AnsiSameTextU(Stack.GetUnicodeString(-2), Stack.GetUnicodeString(-3));
            {$else}
            b := uPSUtils.AnsiSameTextW(Stack.GetUnicodeString(-2), Stack.GetUnicodeString(-3));
            {$ifend}
          {$else}
            {$if declared(AnsiSameText)}
            b := {$IFDEF _ANSISTRINGS_}AnsiStrings.{$ENDIF}AnsiSameText(
              Stack.GetAnsiString(-2), Stack.GetAnsiString(-3));
            {$else}
            b := {$IFDEF _ANSISTRINGS_}AnsiStrings.{$ENDIF}SameText(
              Stack.GetAnsiString(-2), Stack.GetAnsiString(-3));
            {$ifend}
          {$ifend !btCharIsWide}
        end;
        Stack.SetBool(-1, b);
      end;
    {.$ifend} // declared(SameText)
    53: // function StringReplace_(Source, OldPattern, NewPattern: AnyString; Flags : TReplaceFlags_): AnyString;
      begin // NB: !!!: "StringReplace" may be busy, so "sysStringReplace" !!!
        C := Stack.GetUInt(-5); // Flag. Sample: writeln( sysStringReplace('a___a', 'a', 'x,', [srfReplaceAll,srfIgnoreCase]) );
        if C > 256 then C := 0; // Check Flag.
        SI := Stack.GetItem(Stack.Count-2);
        if (SI.FType.BaseType = btUnicodeString) then
          Stack.SetUnicodeString(-1, {$IFDEF FPC}StrUtils.{$ELSE}SysUtils.{$ENDIF}StringReplace(
            Stack.GetUnicodestring(-2),
            Stack.GetUnicodestring(-3),
            Stack.GetUnicodestring(-4),
            AReplaceFlags))
        {$IFNDEF PS_NOWIDESTRING}
        else if (SI.FType.BaseType in [btWideString, btWideChar]) then
          Stack.SetWideString(-1, {$IFDEF FPC}StrUtils.{$ELSE}SysUtils.{$ENDIF}StringReplace(
            Stack.GetWideString(-2),
            Stack.GetWideString(-3),
            Stack.GetWideString(-4),
            AReplaceFlags))
        {$ENDIF !PS_NOWIDESTRING}
        else // tsString, btChar
        begin
          {$if btCharIsWide}
            Stack.SetUnicodeString(-1, {$IFDEF FPC}StrUtils.{$ELSE}SysUtils.{$ENDIF}StringReplace(
              Stack.GetUnicodestring(-2),
              Stack.GetUnicodestring(-3),
              Stack.GetUnicodestring(-4),
              AReplaceFlags));
          {$else}
            {$IFDEF UNICODE}
              Stack.SetUnicodeString(-1, {$IFDEF FPC}StrUtils.{$ELSE}SysUtils.{$ENDIF}StringReplace(
                Stack.GetUnicodeString(-2),
                Stack.GetUnicodeString(-3),
                Stack.GetUnicodeString(-4),
                AReplaceFlags));
            {$ELSE  !UNICODE}
            Stack.SetAnsiString(-1, AnsiString(SysUtils.StringReplace(
              string(Stack.GetAnsiString(-2)),
              string(Stack.GetAnsiString(-3)),
              string(Stack.GetAnsiString(-4)),
              AReplaceFlags)));
            {$ENDIF UNICODE}
          {$ifend}
        end;
      end;
//
// template:
//    ??: //
//      begin
//
//      end;
//
    {+.}
    else begin
      Result := False;
      Exit;
    end;
  end; // case
  Result := True;
end; // function DefProc

function GetArrayLength({%H-}Caller: TPSExec; {%H-}p: TPSExternalProcRec; {%H-}Global, Stack: TPSStack): Boolean;
var arr: TPSVariantIFC;
begin
  Arr := NewTPSVariantIFC(Stack[Stack.Count-2], True);
  if (arr.aType.BaseType <> btStaticArray) and ((arr.Dta = nil) or (arr.aType.BaseType <> btArray)) then begin
    Result := False;
    Exit;
  end;
  if arr.aType.BaseType = btStaticArray then
    Stack.SetInt(-1, TPSTypeRec_StaticArray(arr.aType).Size)
  else
    Stack.SetInt(-1, PSDynArrayGetLength(Pointer(arr.Dta^), arr.aType));
  Result := True;
end;

function SetArrayLength({%H-}Caller: TPSExec; {%H-}p: TPSExternalProcRec; {%H-}Global, Stack: TPSStack): Boolean;
var arr: TPSVariantIFC;
begin
  Arr := NewTPSVariantIFC(Stack[Stack.Count-1], True);
  if (arr.Dta = nil) or (arr.aType.BaseType <> btArray) then begin
    Result := False;
    Exit;
  end;
  PSDynArraySetLength(Pointer(arr.Dta^), arr.aType, Stack.GetInt(-2));
  Result := True;
end;

function InterfaceProc(Sender: TPSExec; p: TPSExternalProcRec; {%H-}Tag: Pointer): Boolean; forward;

procedure RegisterInterfaceLibraryRuntime(Se: TPSExec);
begin
  SE.AddSpecialProcImport('intf', InterfaceProc, nil);
end;

{$IFNDEF DELPHI6UP}
function Null: Variant;
begin
  Result := System.Null;
end;

function Unassigned: Variant;
begin
  Result := System.Unassigned;
end;
{$ENDIF !DELPHI6UP}

function Length_(Caller: TPSExec; {%H-}p: TPSExternalProcRec; {%H-}Global, Stack: TPSStack): Boolean;
var
  arr: TPSVariantIFC;
begin
  arr := NewTPSVariantIFC(Stack[Stack.Count-2], False);
  case arr.aType.BaseType of
    btArray:
      begin
        Stack.SetInt(-1, PSDynArrayGetLength(Pointer(arr.Dta^), arr.aType));
        Result := True;
      end;
    btStaticArray:
      begin
        Stack.SetInt(-1, TPSTypeRec_StaticArray(arr.aType).Size);
        Result := True;
      end;
    btString:
      begin
        Stack.SetInt(-1, length(TbtString(arr.Dta^)));
        Result := True;
      end;
    btChar:
      begin
        Stack.SetInt(-1, 1);
        Result := True;
      end;
    {$IFNDEF PS_NOWIDESTRING}
    btWideString:
      begin
        Stack.SetInt(-1, length(tbtWidestring(arr.Dta^)));
        Result := True;
      end;
    btUnicodeString:
      begin
        Stack.SetInt(-1, length(TbtUnicodeString(arr.Dta^)));
        Result := True;
      end;
    {+}
    btWideChar:
      begin
        Stack.SetInt(-1, 1);
        Result := True;
      end;
    {+.}
    {$ENDIF}
    btvariant:
      begin
        Stack.SetInt(-1, length(Variant(arr.Dta^)));
        Result := True;
      end;
  else
    begin
      Caller.CMD_Err2(erTypeMismatch, TbtString(RPS_TypeMismatch));
      Result := True;
    end;
  end;
end;

function SetLength_({%H-}Caller: TPSExec; {%H-}p: TPSExternalProcRec; {%H-}Global, Stack: TPSStack): Boolean;
var
  arr: TPSVariantIFC;
begin
  Result := False;
  arr := NewTPSVariantIFC(Stack[Stack.Count-1], True);
  if arr.aType.BaseType = btArray then begin
    PSDynArraySetLength(Pointer(arr.Dta^),arr.aType,Stack.GetInt(-2));
    Result := True;
  end else if arr.aType.BaseType=btString then begin
    SetLength(TbtString(arr.Dta^),STack.GetInt(-2));
    Result:=True;
  {$IFNDEF PS_NOWIDESTRING}
  end else if arr.aType.BaseType=btWideString then begin
    SetLength(tbtwidestring(arr.Dta^),STack.GetInt(-2));
    Result := True;
  end else if arr.aType.BaseType=btUnicodeString then begin
    SetLength(TbtUnicodeString(arr.Dta^),STack.GetInt(-2));
    Result := True;
  {$ENDIF !PS_NOWIDESTRING}
  end;
end;

function Low_({%H-}Caller: TPSExec; {%H-}p: TPSExternalProcRec; {%H-}Global, Stack: TPSStack): Boolean;
var
  arr: TPSVariantIFC;
begin
  Result := True;
  arr := NewTPSVariantIFC(Stack[Stack.Count-2], False);
  case arr.aType.BaseType of
    btArray:
      Stack.SetInt(-1,0);
    btStaticArray:
      Stack.SetInt(-1,TPSTypeRec_StaticArray(arr.aType).StartOffset);
    btString:
      Stack.SetInt(-1,1);
    btU8:
      Stack.SetInt(-1,Low(Byte));        //Byte: 0
    btS8:
      Stack.SetInt(-1,Low(ShortInt));    //ShortInt: -128
    btU16:
      Stack.SetInt(-1,Low(Word));        //Word: 0
    btS16:
      Stack.SetInt(-1,Low(SmallInt));    //SmallInt: -32768
    btU32:
      Stack.SetInt(-1,Low(Cardinal));    //Cardinal/LongWord: 0
    btS32:
      Stack.SetInt(-1,Low(Integer));     //Integer/LongInt: -2147483648
    {$IFNDEF PS_NOINT64}
    btS64:
      Stack.SetInt64(-1,Low(Int64));     //Int64: -9223372036854775808
    {$ENDIF !PS_NOINT64}
    else
      Result := False;
  end;
end;

function High_({%H-}Caller: TPSExec; {%H-}p: TPSExternalProcRec; {%H-}Global, Stack: TPSStack): Boolean;
var
  arr: TPSVariantIFC;
begin
  Result := True;
  arr := NewTPSVariantIFC(Stack[Stack.Count-2], False);
  case arr.aType.BaseType of
    btArray:
      Stack.SetInt(-1,PSDynArrayGetLength(Pointer(arr.Dta^),arr.aType)-1);
    btStaticArray:
      Stack.SetInt(-1,TPSTypeRec_StaticArray(arr.aType).StartOffset+TPSTypeRec_StaticArray(arr.aType).Size-1);
    btString:
      Stack.SetInt(-1,Length(TbtString(arr.Dta^)));
    btU8:
      Stack.SetInt(-1,High(Byte));       //Byte: 255
    btS8:
      Stack.SetInt(-1,High(ShortInt));   //ShortInt: 127
    btU16:
      Stack.SetInt(-1,High(Word));       //Word: 65535
    btS16:
      Stack.SetInt(-1,High(SmallInt));   //SmallInt: 32767
    btU32:
      Stack.SetUInt(-1,High(Cardinal));  //Cardinal/LongWord: 4294967295
    btS32:
      Stack.SetInt(-1,High(Integer));    //Integer/LongInt: 2147483647
    {$IFNDEF PS_NOINT64}
    btS64:
      Stack.SetInt64(-1,High(Int64));    //Int64: 9223372036854775807
    {$ENDIF !PS_NOINT64}
    else
      Result := False;
  end;
end;

function Dec_({%H-}Caller: TPSExec; {%H-}p: TPSExternalProcRec; {%H-}Global, Stack: TPSStack): Boolean;
var
  arr: TPSVariantIFC;
begin
  Result := True;
  arr := NewTPSVariantIFC(Stack[Stack.Count-1], True);
  case arr.aType.BaseType of
    btU8:
      Stack.SetInt(-1,Tbtu8(arr.dta^)-1);     //Byte
    btS8:
      Stack.SetInt(-1,Tbts8(arr.dta^)-1);     //ShortInt
    btU16:
      Stack.SetInt(-1,Tbtu16(arr.dta^)-1);    //Word
    btS16:
      Stack.SetInt(-1,Tbts16(arr.dta^)-1);    //SmallInt
    btU32:
      Stack.SetInt(-1,Tbtu32(arr.dta^)-1);    //Cardinal/LongWord
    btS32:
      Stack.SetInt(-1,Tbts32(arr.dta^)-1);    //Integer/LongInt
    {$IFNDEF PS_NOINT64}
    btS64:
      Stack.SetInt64(-1,Tbts64(arr.dta^)-1);
    {$ENDIF !PS_NOINT64}
    else
      Result := False;
  end;
end;

function Inc_({%H-}Caller: TPSExec; {%H-}p: TPSExternalProcRec; {%H-}Global, Stack: TPSStack): Boolean;
var
  arr: TPSVariantIFC;
begin
  Result := True;
  arr := NewTPSVariantIFC(Stack[Stack.Count-1],True);
  case arr.aType.BaseType of
    btU8:
      Stack.SetInt(-1,Tbtu8(arr.dta^)+1);     //Byte
    btS8:
      Stack.SetInt(-1,Tbts8(arr.dta^)+1);     //ShortInt
    btU16:
      Stack.SetInt(-1,Tbtu16(arr.dta^)+1);    //Word
    btS16:
      Stack.SetInt(-1,Tbts16(arr.dta^)+1);    //SmallInt
    btU32:
      Stack.SetInt(-1,Tbtu32(arr.dta^)+1);    //Cardinal/LongWord
    btS32:
      Stack.SetInt(-1,Tbts32(arr.dta^)+1);    //Integer/LongInt
    {$IFNDEF PS_NOINT64}
    btS64:
      Stack.SetInt64(-1,Tbts64(arr.dta^)+1);
    {$ENDIF !PS_NOINT64}
    else
      Result := False;
  end;
end;

function Include_({%H-}Caller: TPSExec; {%H-}p: TPSExternalProcRec; {%H-}Global, Stack: TPSStack): Boolean;
var
  TheSet, NewMember: TPSVariantIFC;
  SetData: PByteArray;
  Val: Tbtu8;
begin
  TheSet:=NewTPSVariantIFC(Stack[Stack.Count-1], True);
  NewMember:=NewTPSVariantIFC(Stack[Stack.Count-2], False);
  Result := (TheSet.aType.BaseType = btSet) and (NewMember.aType.BaseType = btU8);
  if not Result then
    Exit;
  SetData := TheSet.Dta;
  Val := Tbtu8(NewMember.dta^);
  SetData^[Val shr 3] := SetData^[Val shr 3] or (1 shl (Val and 7));
end;

function Exclude_({%H-}Caller: TPSExec; {%H-}p: TPSExternalProcRec; {%H-}Global, Stack: TPSStack): Boolean;
var
  TheSet, NewMember: TPSVariantIFC;
  SetData: PByteArray;
  Val: Tbtu8;
begin
  TheSet:=NewTPSVariantIFC(Stack[Stack.Count-1], True);
  NewMember:=NewTPSVariantIFC(Stack[Stack.Count-2], False);
  Result := (TheSet.aType.BaseType = btSet) and (NewMember.aType.BaseType = btU8);
  if not Result then
    Exit;
  SetData := TheSet.Dta;
  Val := Tbtu8(NewMember.dta^);
  SetData^[Val shr 3] := SetData^[Val shr 3] and not (1 shl (Val and 7));
end;

{+}
{$IFDEF _VARIANTS_}
function VarArrayGet_(var S : Variant; I : Integer) : Variant;
begin
  Result := VarArrayGet(S, [I]);
end;

procedure VarArraySet_(const c : Variant; I : Integer; var s : Variant);
begin
  VarArrayPut(s, c, [i]);
end;

function VarIsArray_(const A: Variant): Boolean;
begin
  Result := {Variants.}VarIsArray(A);
end;
{$ENDIF _VARIANTS_}

function VarToStr_(const V: Variant): string;
begin
  if not VarIsEmptyOrNull(V) then
    //-Result := Variants.VarToStr(V)
    Result := V
  else
    Result := '';
end;

function Format_(const sFormat: string; const Args: array of const): string;
begin
  Result := Format(sFormat, Args);
end;

function FileExists_(const FileName: string): Boolean;
begin
  Result := SysUtils.FileExists(FileName {, FollowLink: True});
end;

function DirectoryExists_(const Dir: string): Boolean;
begin
  Result := SysUtils.DirectoryExists(Dir {, FollowLink: True});
end;

function GetFileSize_(const FileName: string): Int64;
var
  {$IFDEF MSWINDOWS}
  Handle: THandle;
  FindData: TWin32FindData;
  {$ELSE}
  stm: {Classes.}TFileStream;
  {$ENDIF}
begin
  {$IFDEF MSWINDOWS}
  Handle := FindFirstFile(PChar(FileName), {%H-}FindData);
  if Handle <> INVALID_HANDLE_VALUE then begin
    Windows.FindClose(Handle);
    if (FindData.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY) = 0 then begin
      Result := FindData.nFileSizeLow or Int64(FindData.nFileSizeHigh) shl 32;
      Exit;
    end;
  end;
  Result := -1;
  {$ELSE !MSWINDOWS}
  stm := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    Result := stm.Size;
  except
    Result := -1;
  end;
  stm.Free;
  {$ENDIF !MSWINDOWS}
end;
{+.}

procedure TPSExec.RegisterStandardProcs;
begin
  { The following needs to be in synch in these 3 functions:
    -UPSCompiler.TPSPascalCompiler.DefineStandardProcedures
    -UPSRuntime.DefProc
    -UPSRuntime.TPSExec.RegisterStandardProcs
  }
  RegisterFunctionName('!NOTIFICATIONVARIANTSET', NVarProc, Pointer(0), nil);
  RegisterFunctionName('!NOTIFICATIONVARIANTGET', NVarProc, Pointer(1), nil);

  RegisterFunctionName('IntToStr', DefProc, Pointer(0), nil);
  RegisterFunctionName('StrToInt', DefProc, Pointer(1), nil);
  RegisterFunctionName('StrToIntDef', DefProc, Pointer(2), nil);
  RegisterFunctionName('Pos', DefProc, Pointer(3), nil);
  RegisterFunctionName('Copy', DefProc, Pointer(4), nil);
  RegisterFunctionName('Delete', DefProc, Pointer(5), nil);
  RegisterFunctionName('Insert', DefProc, Pointer(6), nil);

  RegisterFunctionName('StrGet', DefProc, Pointer(7), nil);
  RegisterFunctionName('StrSet', DefProc, Pointer(8), nil);
  RegisterFunctionName('UpperCase', DefProc, Pointer(10), nil);
  RegisterFunctionName('LowerCase', DefProc, Pointer(11), nil);
  RegisterFunctionName('Trim', DefProc, Pointer(12), nil);

  RegisterFunctionName('Length',Length_,nil,nil);
  RegisterFunctionName('SetLength',SetLength_,nil,nil);
  RegisterFunctionName('Low',Low_,nil,nil);
  RegisterFunctionName('High',High_,nil,nil);
  RegisterFunctionName('Dec',Dec_,nil,nil);
  RegisterFunctionName('Inc',Inc_,nil,nil);
  RegisterFunctionName('Include',Include_,nil,nil);
  RegisterFunctionName('Exclude',Exclude_,nil,nil);

  RegisterFunctionName('Sin', DefProc, Pointer(15), nil);
  RegisterFunctionName('Cos', DefProc, Pointer(16), nil);
  RegisterFunctionName('Sqrt', DefProc, Pointer(17), nil);
  RegisterFunctionName('Round', DefProc, Pointer(18), nil);
  RegisterFunctionName('Trunc', DefProc, Pointer(19), nil);
  RegisterFunctionName('Int', DefProc, Pointer(20), nil);
  RegisterFunctionName('Pi', DefProc, Pointer(21), nil);
  RegisterFunctionName('Abs', DefProc, Pointer(22), nil);
  RegisterFunctionName('StrToFloat', DefProc, Pointer(23), nil);
  RegisterFunctionName('FloatToStr', DefProc, Pointer(24), nil);
  RegisterFunctionName('PadL', DefProc, Pointer(25), nil);
  RegisterFunctionName('PadR', DefProc, Pointer(26), nil);
  RegisterFunctionName('PadZ', DefProc, Pointer(27), nil);
  RegisterFunctionName('Replicate', DefProc, Pointer(28), nil);
  RegisterFunctionName('StringOfChar', DefProc, Pointer(28), nil);
  RegisterFunctionName('!ASSIGNED', DefProc, Pointer(29), nil);

  {.$IFDEF PS_HAVEVARIANT}
  RegisterDelphiFunction(@Unassigned, 'Unassigned', cdRegister);
  RegisterDelphiFunction(@VarIsEmpty, 'VarIsEmpty', cdRegister);
  {$IFDEF DELPHI7UP}
  RegisterDelphiFunction(@VarIsClear, 'VarIsClear', cdRegister);
  {$ENDIF}
  RegisterDelphiFunction(@Null, 'Null', cdRegister);
  RegisterDelphiFunction(@VarIsNull, 'VarIsNull', cdRegister);
  RegisterDelphiFunction(@{$IFDEF FPC}Variants.{$ENDIF}VarType, 'VarType', cdRegister);
  {+}
  RegisterDelphiFunction(@VarToStr_, 'VarToStr', cdRegister);
  {$IFDEF _VARIANTS_}
  RegisterDelphiFunction(@VarIsArray_, 'VarIsArray', cdRegister);
  RegisterDelphiFunction(@VarArrayDimCount, 'VarArrayDimCount', cdRegister);
  RegisterDelphiFunction(@VarArrayLowBound, 'VarArrayLowBound', cdRegister);
  RegisterDelphiFunction(@VarArrayHighBound, 'VarArrayHighBound', cdRegister);
  RegisterDelphiFunction(@VarArrayGet_, 'VarArrayGet', cdRegister);
  RegisterDelphiFunction(@VarArraySet_, 'VarArraySet', cdRegister);
  {$ENDIF _VARIANTS_}
  {.$ENDIF PS_HAVEVARIANT}
  {+.}
  {$IFNDEF PS_NOIDISPATCH}
  RegisterDelphiFunction(@IDispatchInvoke, 'IDispatchInvoke', cdRegister);
  {$ENDIF}

  RegisterFunctionName('GetArrayLength', GetArrayLength, nil, nil);
  RegisterFunctionName('SetArrayLength', SetArrayLength, nil, nil);

  RegisterFunctionName('RaiseLastException', DefPRoc, Pointer(30), nil);
  {+}
  RegisterFunctionName('raise', DefPRoc, Pointer(30), nil); // TODO: modify parser for "raise Exception.Create()". Map to RaiseException...
  {+.}
  RegisterFunctionName('RaiseException', DefPRoc, Pointer(31), nil);
  RegisterFunctionName('ExceptionType', DefPRoc, Pointer(32), nil);
  RegisterFunctionName('ExceptionParam', DefPRoc, Pointer(33), nil);
  RegisterFunctionName('ExceptionProc', DefPRoc, Pointer(34), nil);
  RegisterFunctionName('ExceptionPos', DefPRoc, Pointer(35), nil);
  RegisterFunctionName('ExceptionToString', DefProc, Pointer(36), nil);
  RegisterFunctionName('AnsiUpperCase', DefProc, Pointer(37), nil);
  RegisterFunctionName('AnsiLowerCase', DefProc, Pointer(38), nil);

  {$IFNDEF PS_NOINT64}
  RegisterFunctionName('StrToInt64', DefProc, Pointer(39), nil);
  RegisterFunctionName('Int64ToStr', DefProc, Pointer(40), nil);
  RegisterFunctionName('StrToInt64Def', DefProc, Pointer(41), nil);
  {$ENDIF}
  RegisterFunctionName('SizeOf', DefProc, Pointer(42), nil);

  {$IFNDEF PS_NOWIDESTRING}
  RegisterFunctionName('WStrGet', DefProc, Pointer(43), nil);
  RegisterFunctionName('WStrSet', DefProc, Pointer(44), nil);
  {$ENDIF}

  {+}
  RegisterFunctionName('PosEx', DefProc, Pointer(45), nil);
  RegisterFunctionName('TrimLeft', DefProc, Pointer(46), nil);
  RegisterFunctionName('TrimRight', DefProc, Pointer(47), nil);

  RegisterFunctionName('TrimLen', DefProc, Pointer(48), nil);
  RegisterFunctionName('TrimLeftLen', DefProc, Pointer(49), nil);
  RegisterFunctionName('TrimRightLen', DefProc, Pointer(50), nil);

  {.$ifend} // declared(SameText)
  RegisterFunctionName('SameText', DefProc, Pointer(51), nil);
  RegisterFunctionName('AnsiSameText', DefProc, Pointer(52), nil);
  {.$ifend} // declared(SameText)

  // NB: !!!: "StringReplace" may be busy, so "SysStringReplace" !!!
  RegisterFunctionName('sysStringReplace', DefProc, Pointer(53), nil);
  {+.}

  {+}
  RegisterDelphiFunction(@Format_, 'Format', cdRegister);
  {$if declared(FormatFloat)}
  RegisterDelphiFunction(@FormatFloat, 'FormatFloat', cdRegister);
  {$ifend}

  RegisterDelphiFunction(@SysUtils.ChangeFileExt, 'ChangeFileExt', cdRegister);
  RegisterDelphiFunction(@SysUtils.ExtractFilePath, 'ExtractFilePath', cdRegister);
  RegisterDelphiFunction(@SysUtils.ExtractFileName, 'ExtractFileName', cdRegister);
  RegisterDelphiFunction(@SysUtils.ExtractFileExt, 'ExtractFileExt', cdRegister);
  RegisterDelphiFunction(@SysUtils.IncludeTrailingPathDelimiter, 'IncludeTrailingPathDelimiter', cdRegister);
  RegisterDelphiFunction(@SysUtils.ExcludeTrailingPathDelimiter, 'ExcludeTrailingPathDelimiter', cdRegister);

  RegisterDelphiFunction(@FileExists_, 'FileExists', cdRegister);
  RegisterDelphiFunction(@DirectoryExists_, 'DirectoryExists', cdRegister);

  RegisterDelphiFunction(@GetFileSize_, 'GetFileSize', cdRegister);

  RegisterDelphiFunction(@SysUtils.ForceDirectories, 'ForceDirectories', cdRegister);
  RegisterDelphiFunction(@SysUtils.RenameFile, 'RenameFile', cdRegister);
  RegisterDelphiFunction(@SysUtils.DeleteFile, 'DeleteFile', cdRegister);

  {+.}

  //--RegisterDelphiFunction(@AAA, 'AAA', cdRegister);
  RegisterInterfaceLibraryRuntime(Self);
end;

function ToString(p: PAnsiChar): TbtString;
begin
  SetString(Result, p, {+}uPSUtils.StrLenA(p) {+.});
end;

{+}
{$IFNDEF PS_NOWIDESTRING}
function ToWideString(p: PWideChar): tbtWideString;
begin
  //SetString(Result, p, uPSUtils.StrLenW(p));
  Result := p;
end;
{$ENDIF}
{+.}

function IntPIFVariantToVariant(Src: pointer; aType: TPSTypeRec; var Dest: Variant): Boolean;

  function BuildArray(P: Pointer; aType: TPSTypeRec; Len: Longint): Boolean;
  var
    i, elSize: Longint;
    v: Variant;
  begin
    elSize := aType.RealSize;
    Dest := VarArrayCreate([0, Len-1], varVariant);
    for i := 0 to Len-1 do
    begin
      Result := IntPIFVariantToVariant(p, aType, {%H-}v);
      if not Result then
        Exit;
      Dest[i] := v;
      {+}
      //p := Pointer(IPointer(p) + Cardinal(elSize));
      p := PointerShift(p, elSize);
      {+.}
    end;
    Result := True;
  end;

begin
  if aType = nil then begin {+}{@dbg@:hook.variant}{+.} // dbg.cond:
    Dest := null;
    Result := True;
    Exit;
  end;
  if aType.BaseType = btPointer then begin
    aType := TPSTypeRec(Pointer(IPointer(src)+PointerSize)^);
    Src := Pointer(Pointer(Src)^);
  end;
  case aType.BaseType of
    btVariant: Dest := variant(src^);
    btArray: begin
      Result := BuildArray(Pointer(Src^), TPSTypeRec_Array(aType).ArrayType,
        PSDynArrayGetLength(Pointer(src^), aType));
      if not Result then
        Exit;
    end;
    btStaticArray: begin
      Result := BuildArray(Pointer(Src), TPSTypeRec_StaticArray(aType).ArrayType,
        PSDynArrayGetLength(Pointer(src^), aType));
      if not Result then
        Exit;
    end;
    btU8: begin
      if aType.ExportName = 'BOOLEAN' then
        Dest := Boolean(TbtU8(Src^) <> 0)
      else
        Dest := TbtU8(Src^);
    end;
    btS8: Dest := tbts8(Src^);
    btU16: Dest := tbtu16(Src^);
    btS16: Dest := tbts16(Src^);
    btU32: Dest := {$IFDEF DELPHI6UP}tbtu32{$ELSE}tbts32{$ENDIF}(Src^);
    btS32: Dest := tbts32(Src^);
    btSingle: Dest := tbtsingle(Src^);
    btCurrency: Dest:=tbtCurrency(Src^);
    btDouble: begin
      if aType.ExportName = 'TDATETIME' then
        Dest := TDateTime(tbtDouble(Src^))
      else
        Dest := tbtDouble(Src^);
    end;
    btExtended: Dest := tbtExtended(Src^);
    btString: Dest := TbtString(Src^);
    btPChar: Dest := ToString(PansiChar(Src^));
    {$IFNDEF PS_NOINT64}
    {$IFDEF DELPHI6UP} // and FPC
    btS64: Dest := TbtS64(Src^);
    {$ELSE !DELPHI6UP}
    bts64: begin
      Result := False;
      Exit;
    end;
    {$ENDIF DELPHI6UP}
    {$ENDIF !PS_NOINT64}
    btChar: Dest := TbtString(tbtchar(src^));
    {+}
    {$IFDEF CPU32}
    {?}btPointer, btClass:
      Dest := tbtu32(src^);
    {$ENDIF}
    {.$IFNDEF PS_NOINT64}
    {$IFDEF CPU64}
    {?}btPointer, btClass:
      Dest := IPointer(tbts64(src^));
    {$ENDIF}
    {.$ENDIF}
    {?}btInterface:
      Dest := IUnknown(src^);
    {+.}
    {$IFNDEF PS_NOWIDESTRING}
    {+}
    {$if declared(btPWideChar)}
    btPWideChar: Dest := ToWideString(PWideChar(Src^));
    {$ifend}
    {+.}
    btWideString: Dest := tbtWideString(src^);
    btWideChar: Dest := tbtwidestring(TbtWideChar(src^));
    btUnicodeString: Dest := TbtUnicodeString(src^);
    {$ENDIF !PS_NOWIDESTRING}
    else begin
      Result := False;
      Exit;
    end;
  end; // case
  Result := True;
end; // function IntPIFVariantToVariant

function PIFVariantToVariant(Src: PIFVariant; var Dest: Variant): Boolean;
begin
  Result := IntPIFVariantToVariant(@PPSVariantData(src).Data, Src.FType, Dest);
end;

function VariantToPIFVariant(Exec: TPSExec; const Src: Variant; Dest: PIFVariant): Boolean;
var TT: PIFTypeRec;
begin
  if Dest = nil then begin
    Result := False;
    Exit;
  end;
  tt := Exec.FindType2(btVariant);
  if tt = nil then begin
    Result := False;
    Exit;
  end;
  if Dest.FType.BaseType = btPointer then
    Result := Exec.SetVariantValue(PPSVariantPointer(Dest).DataDest, @Src, PPSVariantPointer(Dest).DestType, tt)
  else
    Result := Exec.SetVariantValue(@PPSVariantData(Dest).Data, @Src, Dest.FType, tt);
end;

type
  TPSOpenArray = record
    AType       : Byte; {0}
    OrgVar      : PPSVariantIFC;
    FreeIt      : Boolean;
    ElementSize : Longint;
    ItemCount   : Longint;
    Data        : Pointer;
    VarParam    : Boolean;
  end;
  TOpenArray = TPSOpenArray; // deprecated
  POpenArray = ^TOpenArray;  // deprecated
  PPSOpenArray = POpenArray;
function CreateOpenArray(VarParam: Boolean; Sender: TPSExec; val: PPSVariantIFC): PPSOpenArray;
var
  datap, p: Pointer;
  ctype: TPSTypeRec;
  cp: Pointer;
  i: Longint;
begin
  if (Val.aType.BaseType <> btArray) and (val.aType.BaseType <> btStaticArray) then begin
    Result := nil;
    exit;
  end;
  New(Result);
  Result.AType := 0;
  Result.OrgVar := Val;
  Result.VarParam := VarParam;

  if val.aType.BaseType = btStaticArray then begin
    Result^.ItemCount := TPSTypeRec_StaticArray(val.aType).Size;
    datap := Val.Dta;
  end else begin
    Result^.ItemCount := PSDynArrayGetLength(Pointer(Val.Dta^), val.aType);
    datap := Pointer(Val.Dta^);
  end;
  if TPSTypeRec_Array(Val.aType).ArrayType.BaseType <> btPointer then begin
    Result.FreeIt := False;
    Result.ElementSize := 0;
    Result.Data := datap;
    exit;
  end;
  Result.FreeIt := True;
  Result.ElementSize := SizeOf(TVarRec);
  GetMem(Result.Data, Result.ItemCount * Result.ElementSize);
  P := Result.Data;
  FillChar(p^, Result^.ItemCount * Result^.ElementSize, 0);
  for i := 0 to Result^.ItemCount-1 do begin
    ctype := Pointer(Pointer(IPointer(datap)+PointerSize)^);
    cp := Pointer(Datap^);
    if cp = nil then begin
      TVarRec(p^).VType := vtPointer;
      TVarRec(p^).VPointer := nil;
    end else begin
      case ctype.BaseType of
        btVariant: begin
          TVarRec(p^).VType := vtVariant;
          TVarRec(p^).VVariant := cp;
        end;
        btChar: begin
          {$if btCharIsWide}
          TVarRec(p^).VType := vtPWideChar;
          TVarRec(p^).VPWideChar := Pointer(cp^);
          {$else}
          TVarRec(p^).VType := vtChar;
          TVarRec(p^).VChar := tbtChar(tbtChar(cp^));
          {$ifend}
        end;
        btSingle: begin
          TVarRec(p^).VType := vtExtended;
          New(TVarRec(p^).VExtended);
          TVarRec(p^).VExtended^ := tbtsingle(cp^);
        end;
        btExtended: begin
          TVarRec(p^).VType := vtExtended;
          New(TVarRec(p^).VExtended);
          TVarRec(p^).VExtended^ := tbtextended(cp^);;
        end;
        btDouble: begin
          TVarRec(p^).VType := vtExtended;
          New(TVarRec(p^).VExtended);
          TVarRec(p^).VExtended^ := tbtdouble(cp^);
        end;
        {$IFNDEF PS_NOWIDESTRING}
        {+}
        {$if declared(btPWideChar)}
        btPWideChar: begin
          TVarRec(p^).VType := vtPWideChar;
          TVarRec(p^).VPWideChar := Pointer(cp^);
        end;
        {$ifend}
        {+.}
        btWideChar: begin
          TVarRec(p^).VType := vtWideChar;
          TVarRec(p^).VWideChar := TbtWideChar(cp^);
        end;
        {$IFDEF UNICODE}
        btUnicodeString: begin
          TVarRec(p^).VType := vtUnicodeString;
          TbtUnicodeString(TVarRec(p^).VUnicodeString) := TbtUnicodeString(cp^);
        end;
        {$ELSE !UNICODE}
        btUnicodeString,
        {$ENDIF !UNICODE}
        btWideString: begin
          TVarRec(p^).VType := vtWideString;
          TbtWideString(TVarRec(p^).VWideString) := TbtWideString(cp^);
        end;
        {$ENDIF !PS_NOWIDESTRING}
        btU8: begin
          TVarRec(p^).VType := vtInteger;
          TVarRec(p^).VInteger := tbtu8(cp^);
        end;
        btS8: begin
          TVarRec(p^).VType := vtInteger;
          TVarRec(p^).VInteger := tbts8(cp^);
        end;
        btU16: begin
          TVarRec(p^).VType := vtInteger;
          TVarRec(p^).VInteger := tbtu16(cp^);
        end;
        btS16: begin
          TVarRec(p^).VType := vtInteger;
          TVarRec(p^).VInteger := tbts16(cp^);
        end;
        btU32: begin
          TVarRec(p^).VType := vtInteger;
          TVarRec(p^).VInteger := tbtu32(cp^);
        end;
        btS32: begin
          TVarRec(p^).VType := vtInteger;
          TVarRec(p^).VInteger := tbts32(cp^);
        end;
        {$IFNDEF PS_NOINT64}
        btS64: begin
          TVarRec(p^).VType := vtInt64;
          New(TVarRec(p^).VInt64);
          TVarRec(p^).VInt64^ := tbts64(cp^);
        end;
        {$ENDIF}
        btString: begin
          TVarRec(p^).VType := vtAnsiString;
          TbtString(TVarRec(p^).VAnsiString) := TbtString(cp^);
        end;
        btPChar: begin
          TVarRec(p^).VType := vtPchar;
          TVarRec(p^).VPChar := pointer(cp^);
        end;
        btClass: begin
          TVarRec(p^).VType := vtObject;
          TVarRec(p^).VObject := Pointer(cp^);
        end;
        {$IFNDEF PS_NOINTERFACES}
        {$IFDEF Delphi3UP} // and FPC
        btInterface: begin
          TVarRec(p^).VType := vtInterface;
          IUnknown(TVarRec(p^).VInterface) := IUnknown(cp^);
        end;
        {$ENDIF Delphi3UP}
        {$ENDIF !PS_NOINTERFACES}
      end;
    end;
    datap := Pointer(IPointer(datap)+ (3*SizeOf(Pointer)));
    p := PAnsiChar(p) + Result^.ElementSize;
  end;
end; // function CreateOpenArray

procedure DestroyOpenArray(Sender: TPSExec; V: PPSOpenArray);
var
  cp, datap: pointer;
  ctype: TPSTypeRec;
  p: PVarRec;
  i: Longint;
begin
  if v.FreeIt then begin // basetype = btPointer
    p := v^.Data;
    if v.OrgVar.aType.BaseType = btStaticArray then
      datap := v.OrgVar.Dta
    else
      datap := Pointer(v.OrgVar.Dta^);
    for i := 0 to v^.ItemCount-1 do begin
      ctype := Pointer(Pointer(IPointer(datap)+PointerSize)^);
      cp := Pointer(Datap^);
      case ctype.BaseType of
        btU8: begin
          if v^.varParam then
            tbtU8(cp^) := TVarRec(p^).VInteger
        end;
        btS8: begin
          if v^.varParam then
            tbtS8(cp^) := TVarRec(p^).VInteger
        end;
        btU16: begin
          if v^.varParam then
            tbtU16(cp^) := TVarRec(p^).VInteger
        end;
        btS16: begin
          if v^.varParam then
            tbtS16(cp^) := TVarRec(p^).VInteger
        end;
        btU32: begin
          if v^.varParam then
            tbtu32(cp^) := TVarRec(p^).VInteger
        end;
        btS32: begin
          if v^.varParam then
            tbtS32(cp^) := TVarRec(p^).VInteger
        end;
        btChar: begin
          if v^.VarParam then
            tbtChar(cp^) := tbtChar(TVarRec(p^).VChar)
        end;
        btSingle: begin
          if v^.VarParam then
            tbtSingle(cp^) := TVarRec(p^).VExtended^;
          Dispose(TVarRec(p^).vExtended);
        end;
        btDouble: begin
          if v^.VarParam then
            tbtDouble(cp^) := TVarRec(p^).VExtended^;
          Dispose(TVarRec(p^).VExtended);
        end;
        btExtended: begin
          if v^.VarParam then
            tbtExtended(cp^) := TVarRec(p^).VExtended^;
          Dispose(TVarRec(p^).VExtended);
        end;
        {$IFNDEF PS_NOINT64}
        btS64: begin
          if v^.VarParam then
            tbtS64(cp^) := TVarRec(p^).vInt64^;
          Dispose(TVarRec(p^).VInt64);
        end;
        {$ENDIF !PS_NOINT64}
        {$IFNDEF PS_NOWIDESTRING}
        btWideChar: begin
          if v^.VarParam then
            TbtWideChar(cp^) := TVarRec(p^).VWideChar;
        end;
        {$IFDEF UNICODE}
        btUnicodeString: begin
          if v^.VarParam then
            TbtUnicodeString(cp^) := TbtUnicodeString(TVarRec(p^).VUnicodeString);
          Finalize(TbtUnicodeString(TVarRec(p^).VUnicodeString));
        end;
        {$ELSE !UNICODE}
        btUnicodeString,
        {$ENDIF !UNICODE}
        btWideString: begin
          if v^.VarParam then
            tbtWideString(cp^) := tbtWideString(TVarRec(p^).VWideString);
          Finalize(WideString(TVarRec(p^).VWideString));
        end;
        {$ENDIF !PS_NOWIDESTRING}
        btString: begin
          if v^.VarParam then
            TbtString(cp^) := TbtString(TVarRec(p^).VString);
          Finalize(TbtString(TVarRec(p^).VAnsiString));
        end;
        btClass: begin
          if v^.VarParam then
            Pointer(cp^) := TVarRec(p^).VObject;
        end;
        {$IFNDEF PS_NOINTERFACES}
        {$IFDEF Delphi3UP} // and FPC
        btInterface: begin
          if v^.VarParam then
            IUnknown(cp^) := IUnknown(TVarRec(p^).VInterface);
          Finalize(TbtString(TVarRec(p^).VAnsiString));
        end;
        {$ENDIF Delphi3UP}
        {$ENDIF !PS_NOINTERFACES}
      end;
      {+}
      //datap := Pointer(IPointer(datap)+ (3*SizeOf(Pointer)));
      datap := PointerShift(datap, 3*SizeOf(Pointer));
      //p := Pointer(IPointer(p) + Cardinal(v^.ElementSize));
      p := PointerShift(p, v^.ElementSize);
      {+.}
    end;
    FreeMem(v.Data, v.ElementSize * v.ItemCount);
  end;
  Dispose(V);
end; // procedure DestroyOpenArray

{+}
{$UNDEF _INVOKECALL_INC_} { not change }
{$IFNDEF FPC}
  //
  // DELPHI:
  //
  //{$IFNDEF _INVOKECALL_IMPL_}
  //  {$IFDEF DELPHI23UP}    // DELPHI2010UP == DELPHI14UP // TODO: not compiled for all delphi compilers
  //    //{$IFDEF AUTOREFCOUNT}
  //    //  {$MESSAGE FATAL 'Pascal Script does not supports compilation with AUTOREFCOUNT at the moment!'}
  //    //{$ENDIF !AUTOREFCOUNT}
  //
  //    {$DEFINE _INVOKECALL_IMPL_} { not change }
  //  {$ENDIF DELPHI23UP}
  //{$ENDIF _INVOKECALL_IMPL_}

  {$IFNDEF PS_USECLASSICINVOKE}
    {$IFDEF _INVOKECALL_IMPL_}
      {$IFDEF USEINVOKECALL}
        //{$if defined(CPUX64)}
        //  {--$DEFINE _INVOKECALL_INC_} { optional }  // TODO: currentry not all parameter types supported
        //{$elseif defined(CPUX86)}
        //  {--$DEFINE _INVOKECALL_INC_} { optional }  // TODO: currentry not all parameter types supported
        //{$else}
          {$DEFINE _INVOKECALL_INC_}  { not change } // TODO: currentry not all parameter types supported
        //{$ifend}
      {$ENDIF USEINVOKECALL}
    {$ENDIF _INVOKECALL_IMPL_}
  {$ENDIF !PS_USECLASSICINVOKE}

  {$IFNDEF _INVOKECALL_INC_}
    {$IFDEF DELPHI16UP} // DELPHI16UP == DELPHIXE2UP
      {$if defined(CPUX64)}
        {$include x64.inc}
      {$elseif defined(CPU386) or defined(CPUX86)}
        {$include x86.inc}
      {$else}
        {$MESSAGE FATAL 'Pascal Script does not implemented "function TPSExec.InnerfuseCall" for selected CPU!'}
      {$ifend}
    {$ELSE DELPHI11UP}
      {$include x86.inc}
    {$ENDIF DELPHI11UP}
  {$ENDIF !_INVOKECALL_INC_}

{$ELSE FPC}   //fpc includes left unchanged.
  //
  // FPC:
  //
  {$IFNDEF _INVOKECALL_IMPL_}
    {-$DEFINE _INVOKECALL_IMPL_} { optional }
  {$ENDIF !_INVOKECALL_IMPL_}

  {$IFDEF PS_USECLASSICINVOKE}
    {$UNDEF _INVOKECALL_INC_}
  {$ELSE}
    {$IFDEF USEINVOKECALL}
      {$IFDEF _INVOKECALL_IMPL_}
        {$DEFINE _INVOKECALL_INC_} { optional }  // TODO: currentry not all parameter types supported
      {$ENDIF _INVOKECALL_IMPL_}
    {$ENDIF USEINVOKECALL}
  {$ENDIF !PS_USECLASSICINVOKE}

  {$IFNDEF _INVOKECALL_INC_}
    {$if defined(cpu86) or defined(CPUX86)}
      {$include x86.inc}
    {$elseif defined(cpux86_64) or defined(CPUX64)}
      {$include x64.inc}
    {$elseif defined(cpupowerpc)}
      {$include powerpc.inc}
    {$elseif defined(cpuarm)}
      //{$ifdef cpu64} // TODO: FPC check for ARM64
      {$include arm.inc}
      //{$else}
      //{$include arm64.inc}
      //{$endif}
    {$else}
      {$fatal Pascal Script is not supported for your architecture at the moment!}
    {$ifend}
  {$ENDIF !_INVOKECALL_INC_}

{$ENDIF FPC}
{$IFDEF _INVOKECALL_INC_}
  {$include InvokeCall.inc}
{$ENDIF}
{+.}

type
  PScriptMethodInfo = ^TScriptMethodInfo;
  TScriptMethodInfo = record
    Se: TPSExec;
    ProcNo: Cardinal;
  end;

function MkMethod(FSE: TPSExec; No: Cardinal): TMethod;
begin
  if (no = 0) or (no = InvalidVal) then begin
    Result.Code := nil;
    Result.Data := nil;
  end else begin
    Result.Code := @MyAllMethodsHandler;
    Result.Data := GetMethodInfoRec(FSE, No);
  end;
end;

procedure PFree(Sender: TPSExec; P: PScriptMethodInfo);
begin
  Dispose(p);
end;

function GetMethodInfoRec(SE: TPSExec; ProcNo: Cardinal): Pointer;
var
  I: Longint;
  pp: PScriptMethodInfo;
begin
  if (ProcNo = 0) or (ProcNo = InvalidVal) then begin
    Result := nil;
    Exit;
  end;
  I := 2147483647;
  repeat
    pp := Se.FindProcResource2(@PFree, I);
    if (i <> -1) and (pp^.ProcNo = ProcNo) then begin
      Result := Pp;
      Exit;
    end;
  until i = -1;
  New(pp);
  pp^.Se := TPSExec(Se);
  pp^.ProcNo := Procno;
  Se.AddResource(@PFree, pp);
  Result := pp;
end;

type
  TPtrArr = array[0..1000] of Pointer;
  PPtrArr = ^TPtrArr;
  PPointer = ^Pointer;

function VirtualMethodPtrToPtr(Ptr, FSelf: Pointer): Pointer;
{$IFDEF FPC}
var
 x : PPtrArr;
{$ENDIF}
begin
 {$IFDEF FPC}
 x := Pointer(TObject(FSelf).ClassType) + vmtMethodStart;
 Result := x^[{+}NativeUInt{+.}(Ptr)];
 {$ELSE}
 Result := PPtrArr(PPointer(FSelf)^)^[{+}NativeUInt{+.}(Ptr)];
 {$ENDIF}
end;

function VirtualClassMethodPtrToPtr(Ptr, FSelf: Pointer): Pointer;
{$IFDEF FPC}
var
 x : PPtrArr;
{$ENDIF}
begin
  {$IFDEF FPC}
  x := Pointer(FSelf) + vmtMethodStart;
  Result := x^[{+}NativeUInt{+.}(Ptr)];
  {$ELSE}
  Result := PPtrArr(FSelf)^[{+}NativeUInt{+.}(Ptr)];
  {$ENDIF}
end;

{$IFDEF VER90}{$DEFINE NO_vmtSelfPtr}{$ENDIF}
{$IFDEF FPC}{$DEFINE NO_vmtSelfPtr}{$ENDIF}

{$IFNDEF FPC}
procedure CheckPackagePtr(var P: PByteArray);
begin
  {$ifdef Win32} {TODO: ?: IFDEF CPU32}
  if (Word((@p[0])^) = $25FF) and (Word((@p[6])^)=$C08B)then begin
    p := PPointer((@p[2])^)^;
  end;
  {$endif Win32}
  {$ifdef Win64} {TODO: ?: IFDEF CPU64}
  if (Word((@p[0])^) = $25FF) {and (Word((@p[6])^)=$C08B)}then begin
    p := PPointer(NativeUInt(@P[0]) + Cardinal((@p[2])^) + 6{Instruction Size})^;
  end;
  {$endif Win64}
end;

function FindVirtualMethodPtr(Ret: TPSRuntimeClass; FClass: TClass; Ptr: Pointer): Pointer;
// Idea of getting the number of VMT items from GExperts
var
  p: PPtrArr;
  I: Longint;
begin
  p := Pointer(FClass);
  CheckPackagePtr(PByteArray(Ptr));
  if Ret.FEndOfVMT = MaxInt then begin
    I := {$IFDEF NO_vmtSelfPtr}-48{$ELSE}vmtSelfPtr{$ENDIF} div SizeOf(Pointer) + 1;
    while I < 0 do begin
      if I < 0 then begin
        if I <> ({$IFDEF VER90}-44{$ELSE}vmtTypeInfo{$ENDIF} div SizeOf(Pointer)) then begin
          // from GExperts code
          if (IPointer(p^[I]) > IPointer(p)) and ((IPointer(p^[I]) - IPointer(p))
            div
            //PointerSize < Ret.FEndOfVMT) then
            PointerSize < Cardinal(Ret.FEndOfVMT)) then
          begin
            Ret.FEndOfVMT := (IPointer(p^[I]) - IPointer(p)) div SizeOf(Pointer);
          end;
        end;
      end;
      Inc(I);
    end;
    if Ret.FEndOfVMT = MaxInt then begin
      Ret.FEndOfVMT := 0; // cound not find EndOfVMT
      {+}
      //Result := nil;
      Result := Pointer(-1);
      {++.}
      Exit;
    end;
  end;
  I := 0;
  while I < Ret.FEndOfVMT do begin
    if p^[I] = Ptr then begin
      Result := Pointer(I);
      Exit;
    end;
    I := I + 1;
  end;
  {+}
  //Result := nil;
  Result := Pointer(-1);
  {+.}
end; // function FindVirtualMethodPtr

{$ELSE FPC}
function FindVirtualMethodPtr({%H-}Ret: TPSRuntimeClass; FClass: TClass; Ptr: Pointer): Pointer;
var
  p: PPtrArr;
  I: Longint;
  {$IFDEF DEBUG_IDE}
  x: PPtrArr;
  t: Pointer;
  {$ENDIF}
begin
  p := Pointer(FClass) + vmtMethodStart;
  I := 0;
  while (p^[I]<>nil) and (I < 10000) do begin
    if p^[I] = Ptr then begin
      Result := Pointer(I);
      {$IFDEF DEBUG_IDE}
      x := Pointer(FClass) + vmtMethodStart;
      t := x^[I];
      Assert(t=Ptr,'Computation of virtual method pointer fail : t<>Ptr');
      {$ENDIF}
      Exit;
    end;
    I := I + 1;
  end;
  {+}
  //Result := nil;
  Result := Pointer(-1);
  {+.}
end; // function FindVirtualMethodPtr
{$ENDIF FPC}

function NewTPSVariantIFC(avar: PPSVariant; VarParam: boolean): TPSVariantIFC;
begin
  Result.VarParam := VarParam;
  if avar = nil then begin
    Result.aType := nil;
    Result.Dta := nil;
  end else begin
    Result.aType := avar.FType;
    Result.Dta := @PPSVariantData(avar).Data;
    if Result.aType.BaseType = btPointer then begin
      Result.aType := Pointer(Pointer(IPointer(Result.dta)+ PointerSize)^);
      Result.Dta := Pointer(Result.dta^);
    end;
  end;
end;

function NewTPSVariantRecordIFC(avar: PPSVariant; Fieldno: Longint): TPSVariantIFC;
var offs: Cardinal;
begin
  Result := NewTPSVariantIFC(avar, False);
  if Result.aType.BaseType = btRecord then begin
    Offs := Cardinal(TPSTypeRec_Record(Result.aType).RealFieldOffsets[FieldNo]);
    {+}
    //Result.Dta := Pointer(IPointer(Result.dta) + Offs);
    Result.Dta := PointerShift(Result.dta, Offs);
    {+.}
    Result.aType := TPSTypeRec_Record(Result.aType).FieldTypes[FieldNo];
  end else begin
    Result.Dta := nil;
    Result.aType := nil;
  end;
end;

function PSGetArrayField(const avar: TPSVariantIFC; Fieldno: Longint): TPSVariantIFC;
var offs: Cardinal; n: Longint;
begin
  Result := aVar;
  case Result.aType.BaseType of
    btStaticArray, btArray: begin
      if Result.aType.BaseType = btStaticArray then
        n := TPSTypeRec_StaticArray(Result.aType).Size
      else
        n := PSDynArrayGetLength(Pointer(Result.Dta^), Result.aType);
      if (FieldNo <0) or (FieldNo >= n) then begin
        Result.Dta := nil;
        Result.aType := nil;
        Exit;
      end;
      Offs := TPSTypeRec_Array(Result.aType).ArrayType.RealSize * Cardinal(FieldNo);
      if Result.aType.BaseType = btStaticArray then begin
        {+}
        //Result.Dta := Pointer(IPointer(Result.dta) + Offs)
        Result.Dta := PointerShift(Result.dta, Offs)
        {+.}
      end else begin
        {+}
        //Result.Dta := Pointer(IPointer(Result.dta^) + Offs);
        Result.Dta := PointerShift(Pointer(Result.dta^), Offs);
        {+.}
      end;
      Result.aType := TPSTypeRec_Array(Result.aType).ArrayType;
    end // btStaticArray, btArray
    else begin
      Result.Dta := nil;
      Result.aType := nil;
    end;
  end; // case
end; // function PSGetArrayField

function PSGetRecField(const avar: TPSVariantIFC; Fieldno: Longint): TPSVariantIFC;
var offs: Cardinal;
begin
  Result := aVar;
  if Result.aType.BaseType = btRecord then begin
    Offs := Cardinal(TPSTypeRec_Record(Result.aType).RealFieldOffsets[FieldNo]);
    Result.aType := TPSTypeRec_Record(Result.aType).FieldTypes[FieldNo];
    {+}
    //Result.Dta := Pointer(IPointer(Result.dta) + Offs);
    Result.Dta := PointerShift(Result.dta, Offs);
    {+.}
  end else begin
    Result.Dta := nil;
    Result.aType := nil;
  end;
end;

function NewPPSVariantIFC(avar: PPSVariant; VarParam: boolean): PPSVariantIFC;
begin
  New(Result);
  Result^ := NewTPSVariantIFC(avar, VarParam);
end;

procedure DisposePPSVariantIFC(aVar: PPSVariantIFC);
begin
  if avar <> nil then
    Dispose(avar);
end;

procedure DisposePPSVariantIFCList(list: TPSList);
var i: Longint;
begin
  for i := list.Count-1 downto 0 do
    DisposePPSVariantIFC(list[i]);
  list.free;
end;

function ClassCallProcMethod(Caller: TPSExec; p: TPSExternalProcRec; {%H-}Global, Stack: TPSStack): Boolean;
var
  i: Integer;
  MyList: TPSList;
  n: PIFVariant;
  v: PPSVariantIFC;
  FSelf: Pointer;
  CurrStack: Cardinal;
  cc: TPSCallingConvention;
  S: TbtString;
  MAddr: Pointer;
begin
  Result := False;
  S := p.Decl;
  if Length(S) < 2 then
    Exit;
  cc := TPSCallingConvention(S[1]);
  Delete(S, 1, 1);
  if S[1] = #0 then
    n := Stack[Stack.Count-1]
  else
    n := Stack[Stack.Count-2];
  if (n = nil) or (n^.FType.BaseType <> btClass)
    {+}
    //or (PPSVariantClass(n).Data = nil)
    {+.}
  then begin
    Caller.CMD_Err2(erNullPointerException, TbtString(RPS_NullPointerException));
    Exit;
  end;
  FSelf := PPSVariantClass(n).Data; // @dbg: TMBCSEncoding(PPSVariantClass(n).Data),r
  if (FSelf = nil)
    {.$IFDEF DELPHI} // ClassType == nil:  We need a new "correst uPSR_std.pas" with the corrected TObject_Free )
    or (TObject(FSelf).ClassType = nil)
    {.$ENDIF DELPHI}
  then begin
    if (p.FName = 'FREE') or (p.FName = 'DESTROY') then begin
      Result := True;
      Exit;
    end;
    if (FSelf = nil) then
      Caller.CMD_Err2(erNullPointerException, TbtString(RPS_NullPointerException))
    else
      raise EPSError.Create('Invalid Object Reference'); // EPSError InvalidPointer
    Exit;
  end;
  {+.}
  CurrStack := Cardinal(Stack.Count) - Cardinal(Length(S)) - 1;
  if S[1] = #0 then
    Inc(CurrStack);
  MyList := TPSList.Create;
  for i := 2 to Length(S) do begin
    MyList.Add(nil);
  end;
  for i := Length(S) downto 2 do begin
    n := Stack[CurrStack];
    MyList[i - 2] := NewPPSVariantIFC(n, S[i] <> #0);
    inc(CurrStack);
  end;
  if S[1] <> #0 then
    v := NewPPSVariantIFC(Stack[CurrStack + 1], True)
  else
    v := nil;
  try
    if p.Ext2 = nil then
      MAddr := p.Ext1
    else
      MAddr := VirtualMethodPtrToPtr(p.Ext1, FSelf);
    Result := Caller.InnerfuseCall(FSelf, MAddr, cc, MyList, v)
  finally
    DisposePPSVariantIFC(v);
    DisposePPSVariantIFCList(mylist);
  end;
end; // function ClassCallProcMethod

function ClassCallProcConstructor(Caller: TPSExec; p: TPSExternalProcRec; {%H-}Global, Stack: TPSStack): Boolean;
var
  i, h: Longint;
  v: PPSVariantIFC;
  MyList: TPSList;
  n: PIFVariant;
  FSelf: Pointer;
  CurrStack: Cardinal;
  cc: TPSCallingConvention;
  s: TbtString;
  FType: PIFTypeRec;
  x: TPSRuntimeClass;
  IntVal: PIFVariant;
begin //@dbg: Caller.GetCurrentPositionDebugInfo()  ;  Caller.GetCallStack(CurrStack)
  n := Stack[Stack.Count -2];
  if (n = nil) or (n^.FType.BaseType <> btU32) then begin
    Result := False;
    Exit;
  end;
  FType := Caller.GetTypeNo(PPSVariantU32(N).Data);
  if (FType = nil) then begin
    Result := False;
    Exit;
  end;
  h := MakeHash(FType.ExportName);
  FSelf := nil;
  for i := 0 to TPSRuntimeClassImporter(p.Ext2).FClasses.Count-1 do begin
    x:= TPSRuntimeClassImporter(p.Ext2).FClasses[i];
    if (x.FClassNameHash = h) and (x.FClassName = FType.ExportName) then begin
      FSelf := x.FClass;
    end;
  end;
  if FSelf = nil then begin
    Result := False;
    Exit;
  end;
  s := p.Decl;
  if length(S) < 2 then begin
    Result := False;
    Exit;
  end;
  cc := TPSCallingConvention(s[1]);
  Delete(s, 1, 1);
  CurrStack := Cardinal(Stack.Count) - Cardinal(length(s)) - 1;
  if s[1] = #0 then inc(CurrStack);
  {$IFDEF CPU64}
  IntVal := CreateHeapVariant(Caller.FindType2(btS64));
  {$ELSE}
  IntVal := CreateHeapVariant(Caller.FindType2(btU32));
  {$ENDIF}
  if IntVal = nil then begin
    Result := False;
    Exit;
  end;
  {$IFDEF FPC}
  // under FPC a constructor it's called with self=0 (EAX) and
  // the VMT class pointer in EDX so they are effectively swaped
  // using register calling convention
  {+}
  {$IFDEF CPU64}
  PPSVariantS64(IntVal).Data := tbts64(FSelf); // TODO: unchecked
  {$ELSE}
  PPSVariantU32(IntVal).Data := {+}tbtU32{+.}(FSelf);
  {$ENDIF !CPU64}
  {+.}
  FSelf := Pointer(1);
  {$ELSE !FPC}
  PPSVariantU32(IntVal).Data := 1;
  {$ENDIF !FPC}
  MyList := TPSList.Create;
  MyList.Add(NewPPSVariantIFC(intval, False));
  for i := 2 to length(s) do begin
    MyList.Add(nil);
  end;
  for i := length(s) downto 2 do begin
    n :=Stack[CurrStack];
    //if s[i] <> #0 then MyList[i - 2] := NewPPSVariantIFC(n, s[i] <> #0);
    MyList[i - 1] := NewPPSVariantIFC(n, s[i] <> #0);
    inc(CurrStack);
  end;
  if s[1] <> #0 then begin
    v := NewPPSVariantIFC(Stack[CurrStack + 1], True);
  end else
    v := nil;
  try
    {+}
    {$IFDEF _INVOKECALL_INC_}
    Result := Caller.InnerfuseCall(FSelf, p.Ext1, TPSCallingConvention(Integer(cc) or 64), MyList, v);
    {$ELSE}
    Result := Caller.InnerfuseCall(FSelf, p.Ext1,
      {$IFDEF FPC}TPSCallingConvention(Integer(cc) or 64){$ELSE}cc{$ENDIF}, MyList, v);
    {$ENDIF}
    {+.}
  finally
    DisposePPSVariantIFC(v);
    DisposePPSVariantIFCList(mylist);
    DestroyHeapVariant(intval);
  end;
end; // function ClassCallProcConstructor

function ClassCallProcVirtualConstructor(Caller: TPSExec; p: TPSExternalProcRec; {%H-}Global, Stack: TPSStack): Boolean;
var
  i, h: Longint;
  v: PPSVariantIFC;
  MyList: TPSList;
  n: PIFVariant;
  FSelf: TClass;
  FAddress: Pointer;
  CurrStack: Cardinal;
  cc: TPSCallingConvention;
  s: TbtString;
  FType: PIFTypeRec;
  x: TPSRuntimeClass;
  IntVal: PIFVariant;
begin
  n := Stack[Stack.Count-2];
  if (n = nil) or (n^.FType.BaseType <> btU32) then begin
    Caller.CMD_Err2(erNullPointerException, TbtString(RPS_NullPointerException));
    Result := False;
    Exit;
  end;
  FType := Caller.GetTypeNo(PPSVariantU32(n).Data);
  if (FType = nil) then begin
    Caller.CMD_Err2(erNullPointerException, TbtString(RPS_NullPointerException));
    Result := False;
    Exit;
  end;
  {+}
  h := FType.FExportNameHash; // == MakeHash(FType.ExportName);
  FSelf := nil;
  for i := TPSRuntimeClassImporter(p.Ext2).FClasses.Count-1 downto 0 do begin
    x:= TPSRuntimeClassImporter(p.Ext2).FClasses[i];
    if (x.FClassNameHash = h) and (x.FClassName = FType.ExportName) then begin
      FSelf := x.FClass;
      Break;
    end;
  end;//}
  {+.}
  if FSelf = nil then begin
    Result := False;
    Exit;
  end;
  s := p.Decl;
  if length(S) < 2 then begin
    Result := False;
    Exit;
  end;
  cc := TPSCallingConvention(s[1]);
  delete(s, 1, 1);
  CurrStack := Cardinal(Stack.Count) - Cardinal(length(s)) - 1;
  if s[1] = #0 then inc(CurrStack);
  IntVal := CreateHeapVariant(Caller.FindType2(btU32));
  if IntVal = nil then begin // @dbg: IntVal^.FType,r
    Result := False;
    Exit;
  end;
  PPSVariantU32(IntVal).Data := 1;
  MyList := TPSList.Create;
  MyList.Add(NewPPSVariantIFC(IntVal, False));
  for i := 2 to length(s) do begin
    MyList.Add(nil);
  end;
  for i := length(s) downto 2 do begin
    n :=Stack[CurrStack];
    MyList[i - 1] := NewPPSVariantIFC(n, s[i] <> #0);
    inc(CurrStack);
  end;
  if s[1] <> #0 then
    v := NewPPSVariantIFC(Stack[CurrStack + 1], True)
  else
    v := nil;
  try
    {+}
    if p.FWrap then // @dbg: TPSRuntimeClassImporter(p.FExt2).FClasses,r
      FAddress := p.FExt1
    else
      FAddress := VirtualClassMethodPtrToPtr(p.Ext1, FSelf);
    {$IFDEF _INVOKECALL_INC_}
    Result := Caller.InnerfuseCall(FSelf, FAddress,
      TPSCallingConvention(Integer(cc) or 128),
      MyList, v);
    {$ELSE}
    Result := Caller.InnerfuseCall(FSelf, FAddress,
      {$IFDEF FPC}TPSCallingConvention(Integer(cc) or 128){$ELSE}cc{$ENDIF},
      MyList, v);
    {$ENDIF} // @dbg: TObject(v^.Dta^).ClassName  ;  TMBCSEncoding(v^.Dta^),r
    {+.}
  finally
    DisposePPSVariantIFC(v);
    DisposePPSVariantIFCList(MyList);
    DestroyHeapVariant(IntVal);
  end;
end; // function ClassCallProcVirtualConstructor

function CastProc(Caller: TPSExec; p: TPSExternalProcRec; {%H-}Global, Stack: TPSStack): Boolean;
var
  TypeNo, InVar, ResVar: TPSVariantIFC;
  FSelf: TClass;
  FType: PIFTypeRec;
  H, I: Longint;
  x: TPSRuntimeClass;
  {+}
  S: TbtString;
  {+.}
begin
  TypeNo := NewTPSVariantIFC(Stack[Stack.Count-3], False);
  InVar := NewTPSVariantIFC(Stack[Stack.Count-2], False);
  ResVar := NewTPSVariantIFC(Stack[Stack.Count-1], True);
  if (TypeNo.Dta = nil) or (InVar.Dta = nil) or (ResVar.Dta = nil)
    or (TypeNo.aType.BaseType <> btu32)
    or (resvar.aType <> Caller.FTypes[tbtu32(Typeno.dta^)]) then
  begin
    Result := False;
    Exit;
  end;
  {$IFNDEF PS_NOINTERFACES}
  if (invar.atype.BaseType = btInterface) and (resvar.aType.BaseType = btInterface) then begin
    {$IFNDEF Delphi3UP}
    if IUnknown(resvar.Dta^) <> nil then
      IUnknown(resvar.Dta^).Release;
    {$ENDIF}
    IUnknown(resvar.Dta^) := nil;
    if (IUnknown(invar.Dta^) = nil)
      or (IUnknown(invar.Dta^).QueryInterface(TPSTypeRec_Interface(ResVar.aType).Guid,
        IUnknown(resvar.Dta^)) <> 0) then
    begin
      {+}
      //Caller.CMD_Err2(erCustomError, TbtString(RPS_CannotCastInterface));
      S := TbtString(RPS_CannotCastInterface);
      if ResVar.aType.FExportName <> '' then
        S := S + TbtString(' ') + ResVar.aType.FExportName
      else
        S := S + TbtString(' ') + TbtString(GUIDToString(TPSTypeRec_Interface(ResVar.aType).Guid));
      //
      raise EPSIntf.Create(S);
      //
      {Caller.CMD_Err2(erCustomError, S);
      //
      Result := False;
      Exit;//}
      {+.}
    end;
  {$IFDEF Delphi3UP}
  end else if (Invar.aType.BaseType = btclass) and (resvar.aType.BaseType = btInterface) then begin
    {$IFNDEF Delphi3UP}
    if IUnknown(resvar.Dta^) <> nil then
      IUnknown(resvar.Dta^).Release;
    {$ENDIF !PS_NOINTERFACES}
    IUnknown(resvar.Dta^) := nil;
    if (TObject(invar.Dta^)= nil)
      or (not TObject(invar.dta^).GetInterface(TPSTypeRec_Interface(ResVar.aType).Guid,
        IUnknown(resvar.Dta^))) then
    begin
      {+}
      //Caller.CMD_Err2(erCustomError, TbtString(RPS_CannotCastInterface));
      S := TbtString(RPS_CannotCastInterface);
      if ResVar.aType.FExportName <> '' then
        S := S + TbtString(' ') + ResVar.aType.FExportName
      else
        S := S + TbtString(' ') + TbtString(GUIDToString(TPSTypeRec_Interface(ResVar.aType).Guid));
      //
      raise EPSIntf.Create(S);
      //
      {Caller.CMD_Err2(erCustomError, S);
      //
      Result := False;
      Exit;//}
      {+.}
    end;
  {$ENDIF !Delphi3UP}
  end else
  {$ENDIF !PS_NOINTERFACES}
  if (invar.aType.BaseType = btClass) and (resvar.aType.BaseType = btClass ) then begin
    FType := Caller.GetTypeNo(tbtu32(TypeNo.Dta^));
    if (FType = nil) then begin
      Result := False;
      Exit;
    end;
    h := MakeHash(FType.ExportName);
    FSelf := nil;
    for i := 0 to TPSRuntimeClassImporter(p.Ext2).FClasses.Count-1 do begin
      x:= TPSRuntimeClassImporter(p.Ext2).FClasses[i];
      if (x.FClassNameHash = h) and (x.FClassName = FType.ExportName) then begin
        FSelf := x.FClass;
      end;
    end;
    if FSelf = nil then begin
      Result := False;
      Exit;
    end;
    try
      TObject(ResVar.Dta^) := TObject(InVar.Dta^) as FSelf;
    except
      Result := False;
      Caller.CMD_Err2(erCustomError, TbtString(RPS_CannotCastObject));
      Exit;
    end;
  end else begin
    Result := False;
    Exit;
  end;
  Result := True;
end; // function CastProc

function NilProc(Caller: TPSExec; {%H-}p: TPSExternalProcRec; {%H-}Global, Stack: TPSStack): Boolean;
var
  n: TPSVariantIFC;
begin
  n := NewTPSVariantIFC(Stack[Stack.Count-1], True);
  if (n.Dta = nil) or ((n.aType.BaseType <> btClass) and (n.aType.BaseType <> btInterface)) then begin
    Result := False;
    Caller.CMD_Err2(erNullPointerException, TbtString(RPS_NullPointerException));
    Exit;
  end;
  {$IFNDEF PS_NOINTERFACES}
  if n.aType.BaseType = btInterface then begin
    {$IFNDEF Delphi3UP}
    if IUnknown(n.Dta^) <> nil then
      IUnknown(n.Dta^).Release;
    {$ENDIF}
    IUnknown(n.Dta^) := nil;
  end else
  {$ENDIF !PS_NOINTERFACES}
    Pointer(n.Dta^) := nil;
  Result := True;
end; // function NilProc

function IntfCallProc(Caller: TPSExec; p: TPSExternalProcRec; {%H-}Global, Stack: TPSStack): Boolean;
var
  i: Integer;
  MyList: TPSList;
  n: TPSVariantIFC;
  n2: PPSVariantIFC;
  FSelf, FAddress: Pointer;
  CurrStack: Cardinal;
  cc: TPSCallingConvention;
  s: TbtString;
begin
  s := p.Decl;
  if Length(S) < 2 then begin
    Result := False;
    Exit;
  end;
  cc := TPSCallingConvention(s[1]);
  Delete(s, 1, 1);
  if s[1] = #0 then
    n := NewTPSVariantIFC(Stack[Stack.Count-1], False)
  else
    n := NewTPSVariantIFC(Stack[Stack.Count -2], False);
  if (n.dta = nil) or (n.atype.BaseType <> btInterface) or (Pointer(n.Dta^) = nil) then begin
    Caller.CMD_Err2(erNullPointerException, TbtString(RPS_NullPointerException));
    Result := False;
    Exit;
  end;
  FSelf := Pointer(n.dta^);
  CurrStack := Cardinal(Stack.Count) - Cardinal(length(s)) - 1;
  if s[1] = #0 then
    inc(CurrStack);
  MyList := TPSList.Create;
  n2 := nil;
  try
    for i := 2 to Length(s) do
      MyList.Add(nil);
    for i := length(s) downto 2 do begin
      MyList[i - 2] := NewPPSVariantIFC(Stack[CurrStack], s[i] <> #0);
      inc(CurrStack);
    end;
    if s[1] <> #0 then
      n2 := NewPPSVariantIFC(Stack[CurrStack + 1], True);
    FAddress := Pointer(Pointer(IPointer(FSelf^) + (Cardinal(p.Ext1) * SizeOf(Pointer)))^);
    Caller.InnerfuseCall(FSelf, FAddress, cc, MyList, n2);
    Result := True;
  finally
    DisposePPSVariantIFC(n2);
    DisposePPSVariantIFCList(MyList);
  end;
end; // function IntfCallProc

function InterfaceProc(Sender: TPSExec; p: TPSExternalProcRec; Tag: Pointer): Boolean;
var
  s: TbtString;
begin
  s := p.Decl;
  Delete(s,1,5); // delete 'intf:'
  if s = '' then begin
    Result := False;
    Exit;
  end;
  if s[1] = '.' then begin
    Delete(s,1,1);
    if Length(S) < 6 then begin
      Result := False;
      Exit;
    end;
    p.ProcPtr := IntfCallProc;
    p.Ext1 := Pointer((@s[1])^); // Proc Offset
    Delete(s,1,4);
    P.Decl := s;
    Result := True;
  end else
    Result := False;
end; // function InterfaceProc

function getMethodNo(P: TMethod; SE: TPSExec): Cardinal;
begin
  if (P.Code <> @MyAllMethodsHandler) or (P.Data = nil) or (PScriptMethodInfo(P.Data)^.SE <> SE)  then
    Result := 0
  else
    Result := PScriptMethodInfo(P.Data)^.ProcNo;
end;

function ClassCallProcProperty(Caller: TPSExec; p: TPSExternalProcRec; {%H-}Global, Stack: TPSStack): Boolean;
var
  n: TPSVariantIFC;
  ltemp: Longint;
  FSelf: Pointer;
  m: TMethod;
begin
  try
    if p.Ext2 = Pointer(0) then begin
      n := NewTPSVariantIFC(Stack[Stack.Count-1], False);
      if (n.Dta = nil) or (n.aType.BaseType <> btclass) then begin
        Result := False;
        Caller.CMD_Err2(erNullPointerException, TbtString(RPS_NullPointerException));
        Exit;
      end;
      FSelf := Pointer(n.dta^);
      if FSelf = nil then begin
        Caller.CMD_Err2(erCouldNotCallProc, TbtString(RPS_CouldNotCallProc));
        Result := False;
        Exit;
      end;
      n := NewTPSVariantIFC(Stack[Stack.Count -2], False);
      if (PPropInfo(p.Ext1)^.PropType^.Kind = tkMethod)
        and ((n.aType.BaseType = btu32) or (n.aType.BaseType = btProcPtr)) then
      begin
        SetMethodProp(TObject(FSelf), PPropInfo(p.Ext1), MkMethod(Caller, tbtu32(n.dta^)));
      end else
      case n.aType.BaseType of
        btSet: begin
          ltemp := 0;
          Move(Byte(n.Dta^), ltemp, TPSTypeRec_Set(n.aType).aByteSize);
          SetOrdProp(TObject(FSelf), PPropInfo(p.Ext1), ltemp);
        end;
        btChar, btU8: SetOrdProp(TObject(FSelf), PPropInfo(p.Ext1), TbtU8(n.Dta^));
        btS8: SetOrdProp(TObject(FSelf), PPropInfo(p.Ext1), tbts8(n.Dta^));
        {$IFNDEF PS_NOWIDESTRING}
        btWideChar,
        {$ENDIF}
        btU16: SetOrdProp(TObject(FSelf), PPropInfo(p.Ext1), TbtU16(n.Dta^));
        btS16: SetOrdProp(TObject(FSelf), PPropInfo(p.Ext1), TbtS16(n.Dta^));
        btU32: SetOrdProp(TObject(FSelf), PPropInfo(p.Ext1), TbtU32(n.Dta^));
        btS32: SetOrdProp(TObject(FSelf), PPropInfo(p.Ext1), TbtS32(n.Dta^));
        btSingle: SetFloatProp(TObject(FSelf), p.Ext1, TbtSingle(n.Dta^));
        btDouble: SetFloatProp(TObject(FSelf), p.Ext1, TbtDouble(n.Dta^));
        btExtended: SetFloatProp(TObject(FSelf), p.Ext1, TbtExtended(n.Dta^));
        btString: SetStrProp(TObject(FSelf), p.Ext1, string(TbtString(n.Dta^)));
        btPChar: SetStrProp(TObject(FSelf), p.Ext1, string(PAnsiChar(n.Dta^)));
        btClass: SetOrdProp(TObject(FSelf), P.Ext1, Longint(n.Dta^));
        {$IFDEF DELPHI6UP}
          {$IFNDEF PS_NOWIDESTRING}
        {+}
            {$if declared(btPWideChar)}
        btPWideChar:
              {$IFDEF UNICODE}
          SetWideStrProp(TObject(FSelf), p.Ext1, WideString(PWideChar(n.Dta^)));
              {$ELSE}
          SetStrProp(TObject(FSelf), p.Ext1, string(WideString(PWideChar(n.Dta^))));
              {$ENDIF}
            {$ifend declared(btPWideChar)}
        {+.}
        {$IFNDEF UNICODE}
        btUnicodeString,
        {$ENDIF !UNICODE}
        btWideString: SetWideStrProp(TObject(FSelf), P.Ext1, tbtWideString(n.dta^));
        {$IFDEF UNICODE}
        btUnicodeString:
        {$IFDEF DELPHI_TOKYO_UP}SetStrProp{$ELSE}SetUnicodeStrProp{$ENDIF}(TObject(FSelf),
            P.Ext1, TbtUnicodeString(n.dta^));
        {$ENDIF UNICODE}
        {$ENDIF !PS_NOWIDESTRING}
        {$ENDIF DELPHI6UP}
        else begin
          Result := False;
          Exit;
        end;
      end; // case
      Result := True;
    end else begin
      n := NewTPSVariantIFC(Stack[Stack.Count -2], False);
      if (n.dta = nil) or (n.aType.BaseType <> btClass)then
      begin
        Result := False;
        Caller.CMD_Err2(erNullPointerException, TbtString(RPS_NullPointerException));
        Exit;
      end;
      FSelf := Pointer(n.dta^);
      if FSelf = nil then
      begin
        Caller.CMD_Err2(erCouldNotCallProc, TbtString(RPS_CouldNotCallProc));
        Result := False;
        Exit;
      end;
      n := NewTPSVariantIFC(Stack[Stack.Count-1], False);
      if (PPropInfo(p.Ext1)^.PropType^.Kind = tkMethod) and ((n.aType.BaseType = btu32) or (n.aType.BaseType = btprocptr)) then
      begin
        m := GetMethodProp(TObject(FSelf), PPropInfo(p.Ext1));
        Cardinal(n.Dta^) := GetMethodNo(m, Caller);
        if Cardinal(n.dta^) = 0 then
        begin
          {+}
          //Pointer(Pointer((IPointer(n.dta)+PointerSize))^) := m.Data;
          Pointer(PointerShift(n.dta, PointerSize)^) := m.Data;
          //Pointer(Pointer((IPointer(n.dta)+PointerSize2))^) := m.Code;
          Pointer(PointerShift(n.dta, PointerSize2)^) := m.Code;
          {+.}
        end;
      end else
      case n.aType.BaseType of
        btSet:
          begin
            ltemp := GetOrdProp(TObject(FSelf), PPropInfo(p.Ext1));
            Move(ltemp, Byte(n.Dta^), TPSTypeRec_Set(n.aType).aByteSize);
          end;
        btU8: tbtu8(n.Dta^) := GetOrdProp(TObject(FSelf), p.Ext1);
        btS8: tbts8(n.Dta^) := GetOrdProp(TObject(FSelf), p.Ext1);
        btU16: tbtu16(n.Dta^) := GetOrdProp(TObject(FSelf), p.Ext1);
        btS16: tbts16(n.Dta^) := GetOrdProp(TObject(FSelf), p.Ext1);
        btU32: tbtu32(n.Dta^) := GetOrdProp(TObject(FSelf), p.Ext1);
        btS32: tbts32(n.Dta^) := GetOrdProp(TObject(FSelf), p.Ext1);
        btSingle: tbtsingle(n.Dta^) := GetFloatProp(TObject(FSelf), p.Ext1);
        btDouble: tbtdouble(n.Dta^) := GetFloatProp(TObject(FSelf), p.Ext1);
        btExtended: tbtextended(n.Dta^) := GetFloatProp(TObject(FSelf), p.Ext1);
        btString: TbtString(n.Dta^) := TbtString(GetStrProp(TObject(FSelf), p.Ext1));
        btClass: Longint(n.dta^) := GetOrdProp(TObject(FSelf), p.Ext1);
{$IFDEF DELPHI6UP}
{$IFNDEF PS_NOWIDESTRING}
        {$IFDEF UNICODE}
        btUnicodeString: TbtUnicodeString(n.dta^) := {$IFDEF DELPHI_TOKYO_UP}GetStrProp{$ELSE}GetUnicodeStrProp{$ENDIF}(TObject(FSelf), P.Ext1);
        {$ELSE}
        btUnicodeString,
        {$ENDIF !UNICODE}
        btWideString: tbtWidestring(n.dta^) := GetWideStrProp(TObject(FSelf), P.Ext1);
{$ENDIF !PS_NOWIDESTRING}
{$ENDIF DELPHI6UP}
      else
        begin
          Result := False;
          exit;
        end;
      end;
      Result := True;
    end;
  finally
  end;
end; // function ClassCallProcProperty

function ClassCallProcPropertyHelper(Caller: TPSExec; p: TPSExternalProcRec; {%H-}Global, Stack: TPSStack): Boolean;
var
  I, ParamCount: Longint;
  Params: TPSList;
  n: TPSVariantIFC;
  FSelf: Pointer;
begin
  if Length(P.Decl) < 4 then begin
    Result := False;
    Exit;
  end;
  ParamCount := Longint((@P.Decl[1])^);
  if Longint(Stack.Count) < ParamCount +1 then begin
    Result := False;
    Exit;
  end;
  Dec(ParamCount);
  if p.Ext1 <> nil then // read
  begin
    n := NewTPSVariantIFC(Stack[Longint(Stack.Count) - 2], False);
    if (n.Dta = nil) or (n.aType.BaseType <> btClass) then
    begin
      Result := False;
      Caller.CMD_Err2(erNullPointerException, TbtString(RPS_NullPointerException));
      exit;
    end;
    FSelf := pointer(n.Dta^);
    if FSelf = nil then
    begin
      Caller.CMD_Err2(erCouldNotCallProc, TbtString(RPS_CouldNotCallProc) + ' by nil object');
      Result := False;
      exit;
    end;
    //if TObject(FSelf).ClassType = nil then // TODO: check ...
    //begin
    //  Caller.CMD_Err2(erCouldNotCallProc, TbtString(RPS_CouldNotCallProc) + ' by destroyed object');
    //  Result := False;
    //  exit;
    //end;
    Params := TPSList.Create;
    Params.Add(NewPPSVariantIFC(Stack[Longint(Stack.Count) - 1], True));
    for i := Stack.Count -3 downto Longint(Stack.Count) - ParamCount -2 do
    begin
      Params.Add(NewPPSVariantIFC(Stack[I], False));
    end;
    try
      Result := Caller.InnerfuseCall(FSelf, p.Ext1, cdRegister, Params, nil);
    finally
      DisposePPSVariantIFCList(Params);
    end;
  end else begin
    n := NewTPSVariantIFC(Stack[Longint(Stack.Count) - 1], False);
    if (n.Dta = nil) or (n.aType.BaseType <> btClass) then
    begin
      Result := False;
      Caller.CMD_Err2(erNullPointerException, TbtString(RPS_NullPointerException));
      exit;
    end;
    FSelf := pointer(n.Dta^);
    if FSelf = nil then
    begin
      Caller.CMD_Err2(erCouldNotCallProc, TbtString(RPS_CouldNotCallProc) + ' by nil object');
      Result := False;
      exit;
    end;
    //if TObject(FSelf).ClassType = nil then // TODO: check ...
    //begin
    //  Caller.CMD_Err2(erCouldNotCallProc, TbtString(RPS_CouldNotCallProc) + ' by destroyed object');
    //  Result := False;
    //  exit;
    //end;
    Params := TPSList.Create;
    Params.Add(NewPPSVariantIFC(Stack[Longint(Stack.Count) - ParamCount - 2], False));

    for i := Stack.Count -2 downto Longint(Stack.Count) - ParamCount - 1 do
    begin
      Params.Add(NewPPSVariantIFC(Stack[I], False));
    end;
    try
      Result := Caller.InnerfuseCall(FSelf, p.Ext2, cdregister, Params, nil);
    finally
      DisposePPSVariantIFCList(Params);
    end;
  end;
end; // function ClassCallProcPropertyHelper

function ClassCallProcPropertyHelperName(Caller: TPSExec; p: TPSExternalProcRec; {%H-}Global, Stack: TPSStack): Boolean;
var
  I, ParamCount: Longint;
  Params: TPSList;
  tt: PIFVariant;
  n: TPSVariantIFC;
  FSelf: Pointer;
begin
  if Length(P.Decl) < 4 then begin
    Result := False;
    Exit;
  end;
  ParamCount := Longint((@P.Decl[1])^);
  if Longint(Stack.Count) < ParamCount +1 then begin
    Result := False;
    Exit;
  end;
  Dec(ParamCount);
  if p.Ext1 <> nil then // read
  begin
    n := NewTPSVariantIFC(Stack[Longint(Stack.Count) - 2], False);
    if (n.Dta = nil) or (n.aType.BaseType <> btClass) then
    begin
      Result := False;
      Caller.CMD_Err2(erNullPointerException, TbtString(RPS_NullPointerException));
      exit;
    end;
    FSelf := Tobject(n.dta^);
    Params := TPSList.Create;
    Params.Add(NewPPSVariantIFC(Stack[Longint(Stack.Count) - 1], True));
    for i := Stack.Count -3 downto Longint(Stack.Count) - ParamCount -2 do
      Params.Add(NewPPSVariantIFC(Stack[I], False));
    tt := CreateHeapVariant(Caller.FindType2(btString));
    if tt <> nil then
    begin
      PPSVariantAString(tt).Data := p.Name;
      Params.Add(NewPPSVariantIFC(tt, False));
    end;
    try
      Result := Caller.InnerfuseCall(FSelf, p.Ext1, cdRegister, Params, nil);
    finally
      DestroyHeapVariant(tt);
      DisposePPSVariantIFCList(Params);
    end;
  end else begin
    n := NewTPSVariantIFC(Stack[Longint(Stack.Count) - 1], False);
    if (n.Dta = nil) or (n.aType.BaseType <> btClass) then
    begin
      Result := False;
      Caller.CMD_Err2(erNullPointerException, TbtString(RPS_NullPointerException));
      exit;
    end;
    FSelf := TObject(n.dta^);
    Params := TPSList.Create;
    Params.Add(NewPPSVariantIFC(Stack[Longint(Stack.Count) - 2], True));

    for i := Stack.Count -2 downto Longint(Stack.Count) - ParamCount - 1 do
    begin
      Params.Add(NewPPSVariantIFC(Stack[I], False));
    end;
    tt := CreateHeapVariant(Caller.FindType2(btString));
    if tt <> nil then
    begin
      PPSVariantAString(tt).Data := p.Name;
      Params.Add(NewPPSVariantIFC(tt, False));
    end;
    try
      Result := Caller.InnerfuseCall(FSelf, p.Ext2, cdregister, Params, nil);
    finally
      DestroyHeapVariant(tt);
      DisposePPSVariantIFCList(Params);
    end;
  end;
end; // function ClassCallProcPropertyHelperName

function ClassCallProcEventPropertyHelper(Caller: TPSExec; p: TPSExternalProcRec; {%H-}Global, Stack: TPSStack): Boolean;
// Event property helper
var
  I, ParamCount: Longint;
  Params: TPSList;
  n: TPSVariantIFC;
  data: TMethod;
  n2: PIFVariant;
  FSelf: Pointer;
begin
  if Length(P.Decl) < 4 then begin
    Result := False;
    Exit;
  end;
  ParamCount := Longint((@P.Decl[1])^);
  if Longint(Stack.Count) < ParamCount +1 then begin
    Result := False;
    exit;
  end;
  Dec(ParamCount);
  if p.Ext1 <> nil then // read
  begin
    n := NewTPSVariantIFC(Stack[Longint(Stack.Count) - 2], False);
    if (n.Dta = nil) or (n.aType.BaseType <> btClass) then
    begin
      Result := False;
      Caller.CMD_Err2(erNullPointerException, TbtString(RPS_NullPointerException));
      exit;
    end;
    FSelf := TObject(n.dta^);
    n := NewTPSVariantIFC(Stack[Longint(Stack.Count) - 1], True); // Result
    if (n.aType.BaseType <> btU32) and (n.aType.BaseType <> btProcPtr) then
    begin
      Result := False;
      Caller.CMD_Err2(erNullPointerException, TbtString(RPS_NullPointerException));
      exit;
    end;
    n2 := CreateHeapVariant(Caller.FindType2(btPChar));
    if n2 = nil then
    begin
      Result := False;
      exit;
    end;
    Params := TPSList.Create;
    //{$IFDEF CPU64}
    //{$ELSE}
    data.Code := nil;
    data.Data := nil;
    //{$ENDIF}
    PPSVariantDynamicArray(n2)^.Data:= @data;
    Params.Add(NewPPSVariantIFC(n2, False));
    for i := Stack.Count -3 downto Longint(Stack.Count) - ParamCount -2 do
      Params.Add(NewPPSVariantIFC(Stack[i], False));
    try
      Result := Caller.InnerfuseCall(FSelf, p.Ext1, cdRegister, Params, nil);
    finally
      Cardinal(n.Dta^) := getMethodNo(data, Caller);
      if Cardinal(n.Dta^) = 0 then
      begin
        {+}
        //Pointer(Pointer((IPointer(n.dta)+PointerSize))^) := data.Data;
        Pointer(PointerShift(n.dta, PointerSize)^) := data.Code;
        //Pointer(Pointer((IPointer(n.dta)+PointerSize2))^) := data.Code;
        Pointer(PointerShift(n.dta, PointerSize2)^) := data.Code;
        {+.}
      end;
      DestroyHeapVariant(n2);
      DisposePPSVariantIFCList(Params);
    end;
  end else begin
    n := NewTPSVariantIFC(Stack[Longint(Stack.Count) - 1], False);
    if (n.Dta = nil) or (n.aType.BaseType <> btClass) then
    begin
      Result := False;
      Caller.CMD_Err2(erNullPointerException, TbtString(RPS_NullPointerException));
      exit;
    end;
    FSelf := TObject(n.dta^);
    n := NewTPSVariantIFC(Stack[Longint(Stack.Count) - 2], False);
    if (n.Dta = nil) or ((n.aType.BaseType <> btu32) and (n.aType.BaseType <> btProcPtr)) then
    begin
      Result := False;
      Caller.CMD_Err2(erNullPointerException, TbtString(RPS_NullPointerException));
      exit;
    end;
    (*n2 := CreateHeapVariant(Caller.FindType2(btPchar));
    if n2 = nil then
    begin
      Result := False;
      exit;
    end;*)

    //if (n.aType.BaseType = btProcPtr) and (cardinal(n.dta^) = 0) then
    //  data := TMethod(Pointer(IPointer(n.dta^)+4)^)
    //else
    //  data := MkMethod(Caller, Cardinal(n.dta^));

    Params := TPSList.Create;
    Params.Add(@n);

    //for i := Stack.Count -2 downto Longint(Stack.Count) - ParamCount - 1 do begin
    //  Params.Add(NewPPSVariantIFC(Stack[I], False));
    //end;
    try
      Result := Caller.InnerfuseCall(FSelf, p.Ext2, cdRegister, Params, nil);
    finally
      Params.Clear;
      {+}
      //DestroyHeapVariant(n2);
      {+.}
      DisposePPSVariantIFCList(Params);
    end;
  end;
end; // function ClassCallProcEventPropertyHelper

function SpecImport(Sender: TPSExec; p: TPSExternalProcRec; Tag: Pointer): Boolean;
//  #
//  # 'class:'+CLASSNAME+'|'+FUNCNAME+'|'+chr(CallingConv)+chr(hasresult)+params
//  #
//  # For property write functions there is an '@' after the funcname.
//  #
var
  H, I: Longint;
  S, s2: TbtString;
  //CI: TPSRuntimeClassImporter absolute Tag;
  //CC: TPSList;
  CL: TPSRuntimeClass;
  px: PClassItem;
  pp: PPropInfo;
  IsRead: Boolean;
begin
  s := p.Decl;
  delete(s, 1, 6);
  if Length(S) = 1 then
  begin
    if s = '-' then {nil function}
    begin
      p.ProcPtr := NilProc;
      Result := True;
      exit;
    end;
    if s = '+' then {cast function}
    begin
      p.ProcPtr := CastProc;
      p.Ext2 := Tag;
      Result := True;
      exit;
    end;
  end;
  s2 := copy(S, 1, pos(tbtchar('|'), s)-1);
  delete(s, 1, length(s2) + 1);
  {+}
  {
  H := MakeHash(s2);
  ISRead := False;
  CC := TPSRuntimeClassImporter(Tag).FClasses; // == CI.FClasses
  CL := nil;
  for I := CC.Count-1 downto 0 do
  begin
    CL := CC[I];
    if (Cl.FClassNameHash = h) and (cl.FClassName = s2) then
    begin
      IsRead := True;
      break;
    end;
  end;
  }
  CL := TPSRuntimeClassImporter(Tag).FindClass(s2);
  IsRead := Assigned(CL); // @dbg: CL.FClassName='TORACLESESSION'
  {+.}
  if not isRead then begin
    Result := False;
    exit;
  end;
  s2 := copy(S, 1, pos(tbtChar('|'), s)-1);
  delete(s, 1, length(s2) + 1);
  if (s2 <> '') and (s2[length(s2)] = '@') then
  begin
    IsRead := False;
    Delete(S2, length(s2), 1);
  end else
    isRead := True;
  p.Name := s2;
  {+}
  p.FRCL := CL;
  {+.}
  H := MakeHash(s2);
  for i := CL.FClassItems.Count-1 downto 0 do
  begin
    px := CL.FClassItems[I];
    if (px^.FNameHash = h) and (px^.FName = s2) then
    begin
      p.Decl := s;
      {+}
      case px^.b of
        RTCLRG_METHOD: // ext1=ptr
          begin
            p.ProcPtr := ClassCallProcMethod;
            p.Ext1 := px^.Ptr;
            if p.Ext1 = nil then begin
              Result := False;
              exit;
            end;
            p.Ext2 := nil;
          end;
        RTCLRG_METHOD_VIRT{==RTCLRG_METHOD_VIRT_ABSTRACT}: // ext1=pointerinlist
          begin
            p.ProcPtr := ClassCallProcMethod;
            p.Ext1 := px^.PointerInList;
            //if p.Ext1 = nil then begin
            //  Result := False; Exit; end;
            if (p.Ext1 = Pointer(-1)) then begin
              Sender.CMD_Err2(erCustomError, TbtString('Failed Define/Load Procs: '+CL.FClassName+'.'+s2));
              Result := False;
              Exit;
            end;
            p.Ext2 := pointer(1);
            //p.FWrap := Assigned(px^.Ext1);
            p.FWrap := Assigned(px^.ClassTypeM1);
            //p.FWrap := Assigned(px^.Ext2) and (px^.Ext2 = px^.Ext1);
            //p.FWrap := Assigned(px^.ClassTypeM1) and (px^.ClassTypeM1 = px^.ClassTypeM2);
          end;
        //? RTCLRG_PROP_INFO: ext1=propertyinfo
        RTCLRG_PROP_HELPER: // ext1=readfunc; ext2=writefunc
          begin
            p.ProcPtr := ClassCallProcPropertyHelper;
            if IsRead then begin
              p.Ext1 := px^.FReadFunc;
              if p.Ext1 = nil then begin
                Result := False;
                Exit;
              end;
              p.Ext2 := nil;
            end else begin
              p.Ext1 := nil;
              p.Ext2 := px^.FWriteFunc;
              if p.Ext2 = nil then begin
                Result := False;
                Exit;
              end;
            end;
          end;
        RTCLRG_CONSTRUCTOR:
          begin
            p.ProcPtr := ClassCallProcConstructor;
            p.Ext1 := px^.Ptr;
            if p.Ext1 = nil then begin
              Result := False;
              Exit;
            end;
            p.Ext2 := Tag;
          end;
        RTCLRG_CONTRUCTOR_VIRT:
          begin
            p.ProcPtr := ClassCallProcVirtualConstructor;
            p.Ext1 := px^.Ptr;
            if (p.Ext1 = nil) or (p.Ext1 = Pointer(-1)) then begin
              Sender.CMD_Err2(erCustomError, TbtString('Failed Define/Load Procs: '+CL.FClassName+'.'+s2));
              Result := False;
              Exit;
            end;
            p.Ext2 := Tag;
            //p.FWrap := Assigned(px^.Ext1);
            p.FWrap := Assigned(px^.ClassTypeC1);
            //p.FWrap := Assigned(px^.Ext1) and (px^.Ext2 = px^.Ext1);
            //p.FWrap := Assigned(px^.ClassTypeC1) and (px^.ClassTypeC1 = px^.ClassTypeC2);
          end;
        RTCLRG_EVENT_PROP_HELPER:
          begin
            p.ProcPtr := ClassCallProcEventPropertyHelper;
            if IsRead then begin
              p.Ext1 := px^.FReadFunc;
              if p.Ext1 = nil then begin
                Result := False;
                Exit;
              end;
              p.Ext2 := nil;
            end else begin
              p.Ext1 := nil;
              p.Ext2 := px^.FWriteFunc;
              if p.Ext2 = nil then begin
                Result := False;
                Exit;
              end;
            end;
          end;
        RTCLRG_PROP_HELPER_NAME:
          begin
            p.ProcPtr := ClassCallProcPropertyHelperName;
            if IsRead then begin
              p.Ext1 := px^.FReadFunc;
              if p.Ext1 = nil then begin
                Result := False;
                Exit;
              end;
              p.Ext2 := nil;
            end else begin
              p.Ext1 := nil;
              p.Ext2 := px^.FWriteFunc;
              if p.Ext2 = nil then begin
                Result := False;
                Exit;
              end;
            end;
          end;
        {+} // https://github.com/remobjects/pascalscript/pull/210
        RTCLRG_METHOD_NAME:
          begin
            p.ProcPtr := px^.ProcPtr;
            p.Ext1 := px^.Ext1;
            p.Ext2 := px^.Ext2;
          end;
        RTCLRG_PROP_NAME_HELPER:
          begin
            if IsRead then begin
              p.ProcPtr := px^.ReadProcPtr;
              p.Ext1 := px^.ExtRead1;
              p.Ext2 := px^.ExtRead2;
            end else begin
              p.ProcPtr := px^.WriteProcPtr;
              p.Ext1 := px^.ExtWrite1;
              p.Ext2 := px^.ExtWrite2;
            end;
          end;
         {RTCLRG_CLASS_METHOD:
           begin
             // TODO: implementation
           end;}
         {RTCLRG_CLASS_PROP:
           begin
             // TODO: implementation
           end;}
        {+.}
        else
         begin
           Result := False;
           exit;
         end;
      end;
      {+.}
      Result := True;
      Exit;
    end;
  end;
  if CL.FClass.ClassInfo <> nil then begin
    pp := GetPropInfo(CL.FClass.ClassInfo, string(s2));
    if pp <> nil then begin
       p.ProcPtr := ClassCallProcProperty;
       p.Ext1 := pp;
       if IsRead then
         p.Ext2 := Pointer(1)
       else
         p.Ext2 := Pointer(0);
       Result := True;
    end else
      Result := False;
  end else
    Result := False;
end; // function SpecImport

procedure RegisterClassLibraryRuntime(SE: TPSExec; Importer: TPSRuntimeClassImporter);
begin
  SE.AddSpecialProcImport('class', SpecImport, Importer);
end;

procedure TPSExec.ClearspecialProcImports;
var
  I: Longint;
  P: PSpecialProc;
begin
  for I := FSpecialProcList.Count-1 downto 0 do
  begin
    P := FSpecialProcList[I];
    Dispose(p);
  end;
  FSpecialProcList.Clear;
end;

procedure TPSExec.RaiseCurrentException;
var
  ExObj: TObject;
begin
  if ExEx = erNoError then exit; // do nothing
  ExObj := Self.ExObject;
  if ExObj <> nil then
  begin
    Self.ExObject := nil;
    raise ExObj;
  end;
  raise EPSException.Create(PSErrorToString(ExceptionCode, ExceptionString), Self, ExProc, ExPos);
end;

procedure TPSExec.CMD_Err2(EC: TPSError; const Param: TbtString);
begin
  CMD_Err3(EC, Param, Nil);
end;

function TPSExec.GetProcAsMethod(const ProcNo: Cardinal): TMethod;
begin
  Result := MkMethod(Self, ProcNo);
end;

function TPSExec.GetProcAsMethodN(const ProcName: TbtString): TMethod;
var
  procno: Cardinal;
begin
  Procno := GetProc(ProcName);
  if Procno = InvalidVal then
  begin
    Result.Code := nil;
    Result.Data := nil;
  end
  else
    Result := MkMethod(Self, procno)
end;

procedure TPSExec.RegisterAttributeType(UseProc: TPSAttributeUseProc; const TypeName: TbtString);
var
  att: TPSAttributeType;
begin
  att := TPSAttributeType.Create;
  att.TypeName := TypeName;
  att.TypeNameHash := MakeHash(TypeName);
  att.UseProc := UseProc;
  FAttributeTypes.Add(att);
end;

function TPSExec.GetProcCount: Cardinal;
begin
  Result := FProcs.Count;
end;

function TPSExec.GetTypeCount: Longint;
begin
  Result := FTypes.Count;
end;

function TPSExec.GetVarCount: Longint;
begin
  Result := FGlobalVars.Count;
end;

function TPSExec.GetCallStack(var Count: Cardinal): TbtString;
begin
  Count := 0;
  Result := '';
end;

function TPSExec.FindSpecialProcImport(
  P: TPSOnSpecialProcImport): pointer;
var
  i: Longint;
  pr: PSpecialProc;
begin
  for i := FSpecialProcList.Count-1 downto 0 do
  begin
    pr := FSpecialProcList[i];
    if @pr.P = @p then
    begin
      Result := pr.tag;
      exit;
    end;
  end;
  Result := nil;
end;

function TPSExec.InvokeExternalMethod(At: TPSTypeRec_ProcPtr; Slf,
  Ptr: Pointer): Boolean;
var
  res: PPSVariantIFC;
  s: TbtString;
  CurrStack, i: Longint;
  n: PPSVariant;
  MyList: TPSList;
begin
  s := TPSTypeRec_ProcPtr(at).ParamInfo;
  CurrStack := Cardinal(FStack.Count) - Cardinal(length(s));
  if s[1] = #0 then inc(CurrStack);
  MyList := TPSList.Create;
  for i := 2 to length(s) do
  begin
    MyList.Add(nil);
  end;
  for i := length(s) downto 2 do
  begin
    n := FStack[CurrStack];
    MyList[i - 2] := NewPPSVariantIFC(n, s[i] <> #0);
    inc(CurrStack);
  end;
  if s[1] <> #0 then
  begin
    res := NewPPSVariantIFC(FStack[CurrStack + 1], True);
  end else res := nil;
  Result := InnerfuseCall(Slf, Ptr, cdRegister, MyList, Res);

  DisposePPSVariantIFC(res);
  DisposePPSVariantIFCList(mylist);
end;

function TPSExec.LastEx: TPSError;
var
  pp: TPSExceptionHandler;
begin
  if FExceptionStack.Count = 0 then begin
    Result := ExEx;
    exit;
  end;
  pp := fExceptionStack[fExceptionStack.Count-1];
  Result := pp.ExceptionData;
end;

function TPSExec.LastExParam: TbtString;
var
  pp: TPSExceptionHandler;
begin
  if FExceptionStack.Count = 0 then begin
    Result := ExParam;
    exit;
  end;
  pp := fExceptionStack[fExceptionStack.Count-1];
  Result := pp.ExceptionParam;
end;

function TPSExec.LastExPos: {+}Cardinal{+.};
var
  pp: TPSExceptionHandler;
begin
  if FExceptionStack.Count = 0 then begin
    Result := ExPos;
    exit;
  end;
  pp := fExceptionStack[fExceptionStack.Count-1];
  Result := pp.ExceptOffset;
end;

function TPSExec.LastExProc: Integer;
var
  pp: TPSExceptionHandler;
begin
  if FExceptionStack.Count = 0 then begin
    Result := ExProc;
    exit;
  end;
  pp := fExceptionStack[fExceptionStack.Count-1];
  Result := FProcs.IndexOf(pp.CurrProc);
end;

function TPSExec.LastExObject: TObject;
var
  pp: TPSExceptionHandler;
begin
  if FExceptionStack.Count = 0 then begin
    Result := ExObject;
    exit;
  end;
  pp := fExceptionStack[fExceptionStack.Count-1];
  Result := pp.ExceptionObject;
end;

{ TPSRuntimeClass }

constructor TPSRuntimeClass.Create(aClass: TClass; const AName: TbtString);
begin
  inherited Create;
  FClass := AClass;
  if AName = '' then
  begin
    FClassName := FastUpperCase(TbtString(aClass.ClassName));
    FClassNameHash := MakeHash(FClassName);
  end else begin
    FClassName := FastUpperCase(AName);
    FClassNameHash := MakeHash(FClassName);
  end;
  FClassItems:= TPSList.Create;
  FEndOfVmt := MaxInt;
end;

destructor TPSRuntimeClass.Destroy;
var
  I: Longint;
  P: PClassItem;
begin
  if Assigned(FClassItems) then begin
    for i:= FClassItems.Count-1 downto 0 do begin
      P := FClassItems.Data[I];
      if Assigned(p) then begin
        FClassItems.Data[I] := nil;
        Dispose(p);
      end;
    end;
    FreeAndNil(FClassItems);
  end;
  inherited;
end;

{+}
//var
//  NullClassItem: TClassItem;
procedure TPSRuntimeClass.NewPClassItem(var P: PClassItem);
begin
  New(P);
  FillChar(P^, SizeOf(P^), 0);
  //P^ := NullClassItem;
  //Move(NullClassItem, P^, SizeOF(P^));
end;
{+.}

procedure TPSRuntimeClass.RegisterVirtualAbstractMethod(ClassDef: TClass; ProcPtr: Pointer; const Name: TbtString);
var
  P: PClassItem;
begin
  //New({%H-}P); FillChar(P^, SizeOf(P^), 0);
  NewPClassItem({%H-}P);
  p^.FName := FastUpperCase(Name);
  p^.FNameHash := MakeHash(p^.FName);
  p^.b := RTCLRG_METHOD_VIRT_ABSTRACT;
  p^.PointerInList := FindVirtualMethodPtr(Self, ClassDef, ProcPtr);
  FClassItems.Add(p);
end;

procedure TPSRuntimeClass.RegisterConstructor(ProcPtr: Pointer; const Name: TbtString);
var P: PClassItem;
begin
  //New({%H-}P); FillChar(P^, SizeOf(P^), 0);
  NewPClassItem({%H-}P);
  p^.FName := FastUpperCase(Name);
  p^.FNameHash := MakeHash(p^.FName);
  p^.b := RTCLRG_CONSTRUCTOR;
  p^.Ptr := ProcPtr;
  FClassItems.Add(p);
end;

procedure TPSRuntimeClass.RegisterMethod(ProcPtr: Pointer; const Name: TbtString);
var P: PClassItem;
begin
  //New({%H-}P); FillChar(P^, SizeOf(P^), 0);
  NewPClassItem({%H-}P);
  p^.FName := FastUpperCase(Name);
  p^.FNameHash := MakeHash(p^.FName);
  p^.b := RTCLRG_METHOD;
  p^.Ptr := ProcPtr;
  FClassItems.Add(p);
end;

procedure TPSRuntimeClass.RegisterMethodName(const Name: TbtString; ProcPtr: TPSProcPtr; Ext1, Ext2: Pointer);
{+} // https://github.com/remobjects/pascalscript/pull/210 {+.}
var P: PClassItem;
begin
  //New({%H-}P); FillChar(P^, SizeOf(P^), 0);
  NewPClassItem({%H-}P);
  p^.FName := FastUppercase(Name);
  p^.FNameHash := MakeHash(p^.FName);
  p^.b := RTCLRG_METHOD_NAME;
  p^.ProcPtr := ProcPtr;
  p^.Ext1 := Ext1;
  p^.Ext2 := Ext2;
  FClassItems.Add(p);
end;

procedure TPSRuntimeClass.RegisterPropertyHelper(ReadFunc, WriteFunc: Pointer; const Name: TbtString);
var P: PClassItem;
begin
  //New({%H-}P); FillChar(P^, SizeOf(P^), 0);
  NewPClassItem({%H-}P);
  p^.FName := FastUpperCase(Name);
  p^.FNameHash := MakeHash(p^.FName);
  p^.b := RTCLRG_PROP_HELPER;
  p^.FReadFunc := ReadFunc;
  p^.FWriteFunc := WriteFunc;
  FClassItems.Add(p);
end;

procedure TPSRuntimeClass.RegisterVirtualConstructor(ProcPtr: Pointer; const Name: TbtString);
var P: PClassItem;
begin
  //New({%H-}P); FillChar(P^, SizeOf(P^), 0);
  NewPClassItem({%H-}P);
  p^.FName := FastUpperCase(Name);
  p^.FNameHash := MakeHash(p^.FName);
  p^.b := RTCLRG_CONTRUCTOR_VIRT;
  p^.PointerInList := FindVirtualMethodPtr(Self, FClass, ProcPtr);
  FClassItems.Add(p);
end;

{+}
procedure TPSRuntimeClass.RegisterVirtualConstructorWrapper(ProcPtr, AClassType: Pointer; const Name: TbtString);
var P: PClassItem;
begin
  //New({%H-}P); FillChar(P^, SizeOf(P^), 0);
  NewPClassItem({%H-}P);
  p^.FName := FastUpperCase(Name);
  p^.FNameHash := MakeHash(p^.FName);
  p^.b := RTCLRG_CONTRUCTOR_VIRT;
  p^.PtrCV := ProcPtr;
  P^.ClassTypeC1 := AClassType;
  P^.ClassTypeC2 := AClassType;
  FClassItems.Add(p);
end;
{+.}

procedure TPSRuntimeClass.RegisterVirtualMethod(ProcPtr: Pointer; const Name: TbtString);
var P: PClassItem;
begin
  //New({%H-}P); FillChar(P^, SizeOf(P^), 0);
  NewPClassItem({%H-}P);
  p^.FName := FastUpperCase(Name);
  p^.FNameHash := MakeHash(p^.FName);
  p^.b := RTCLRG_METHOD_VIRT;
  p^.PointerInList := FindVirtualMethodPtr(Self, FClass, ProcPtr);
  FClassItems.Add(p);
end;

procedure TPSRuntimeClass.RegisterVirtualMethodWrapper(ProcPtr, AClassType: Pointer; const Name: TbtString);
var P: PClassItem;
begin
  //New({%H-}P); FillChar(P^, SizeOf(P^), 0);
  NewPClassItem({%H-}P);
  p^.FName := FastUpperCase(Name);
  p^.FNameHash := MakeHash(p^.FName);
  p^.b := RTCLRG_METHOD_VIRT;
  p^.Ptr := ProcPtr;
  P^.ClassTypeM1 := AClassType;
  P^.ClassTypeM2 := AClassType;
  FClassItems.Add(p);
end;

procedure TPSRuntimeClass.RegisterEventPropertyHelper(ReadFunc, WriteFunc: Pointer; const Name: TbtString);
var P: PClassItem;
begin
  //New({%H-}P); FillChar(P^, SizeOf(P^), 0);
  NewPClassItem({%H-}P);
  p^.FName := FastUpperCase(Name);
  p^.FNameHash := MakeHash(p^.FName);
  p^.b := RTCLRG_EVENT_PROP_HELPER;
  p^.FReadFunc := ReadFunc;
  p^.FWriteFunc := WriteFunc;
  FClassItems.Add(p);
end;

procedure TPSRuntimeClass.RegisterPropertyHelperName(ReadFunc, WriteFunc: Pointer; const Name: TbtString);
var P: PClassItem;
begin
  //New({%H-}P); FillChar(P^, SizeOf(P^), 0);
  NewPClassItem({%H-}P);
  p^.FName := FastUpperCase(Name);
  p^.FNameHash := MakeHash(p^.FName);
  p^.b := RTCLRG_PROP_HELPER_NAME;
  p^.FReadFunc := ReadFunc;
  p^.FWriteFunc := WriteFunc;
  FClassItems.Add(p);
end;

procedure TPSRuntimeClass.RegisterPropertyNameHelper(const Name: TbtString;
  ProcPtr: TPSProcPtr; ExtRead1, ExtRead2, ExtWrite1, ExtWrite2: Pointer);
{+} // https://github.com/remobjects/pascalscript/pull/210 {+.}
var P: PClassItem;
begin
  //New({%H-}P); FillChar(P^, SizeOf(P^), 0);
  NewPClassItem({%H-}P);
  p^.FName := FastUppercase(Name);
  p^.FNameHash := MakeHash(p^.FName);
  p^.b := RTCLRG_PROP_NAME_HELPER;
  p^.ReadProcPtr := ProcPtr;
  p^.WriteProcPtr := ProcPtr;
  p^.ExtRead1 := ExtRead1;
  p^.ExtRead2 := ExtRead2;
  p^.ExtWrite1 := ExtWrite1;
  p^.ExtWrite2 := ExtWrite2;
  FClassItems.Add(p);
end;

procedure TPSRuntimeClass.RegisterPropertyNameHelper(const Name: TbtString;
  ProcReadPtr, ProcWritePtr: TPSProcPtr;
  ExtRead1, ExtRead2, ExtWrite1, ExtWrite2: Pointer);
{+} // https://github.com/remobjects/pascalscript/pull/210 {+.}
var P: PClassItem;
begin
  //New({%H-}P); FillChar(P^, SizeOf(P^), 0);
  NewPClassItem({%H-}P);
  p^.FName := FastUppercase(Name);
  p^.FNameHash := MakeHash(p^.FName);
  p^.b := RTCLRG_PROP_NAME_HELPER;
  p^.ReadProcPtr := ProcReadPtr;
  p^.WriteProcPtr := ProcWritePtr;
  p^.ExtRead1 := ExtRead1;
  p^.ExtRead2 := ExtRead2;
  p^.ExtWrite1 := ExtWrite1;
  p^.ExtWrite2 := ExtWrite2;
  FClassItems.Add(p);
end;

{ TPSRuntimeClassImporter }

function TPSRuntimeClassImporter.Add(aClass: TClass): TPSRuntimeClass;
begin
  Result := FindClass(TbtString(aClass.ClassName));
  if Result <> nil then
    Exit;
  Result := TPSRuntimeClass.Create(aClass, '');
  FClasses.Add(Result);
end;

function TPSRuntimeClassImporter.Add2(aClass: TClass; const Name: TbtString): TPSRuntimeClass;
begin
  Result := FindClass(Name);
  if Result <> nil then
    Exit;
  Result := TPSRuntimeClass.Create(aClass, Name);
  FClasses.Add(Result);
end;

procedure TPSRuntimeClassImporter.Clear;
var
  I: Longint;
  O: TPSRuntimeClass;
begin
  for i := 0 to FClasses.Count-1 do begin
    O := TPSRuntimeClass(FClasses.Data[I]);
    FClasses.Data[I] := nil;
    O.Free;
  end;
  FClasses.Clear;
end;

constructor TPSRuntimeClassImporter.Create;
begin
  inherited Create;
  FClasses := TPSList.Create;
end;

constructor TPSRuntimeClassImporter.CreateAndRegister(Exec: TPSExec; AutoFree: Boolean);
begin
  inherited Create;
  FClasses := TPSList.Create;
  RegisterClassLibraryRuntime(Exec, Self);
  if AutoFree then
    Exec.AddResource(@RCIFreeProc, Self);
end;

destructor TPSRuntimeClassImporter.Destroy;
begin
  if Assigned(FClasses) then begin
    Clear;
    FreeAndNil(FClasses);
  end;
  inherited;
end;

{$IFNDEF PS_NOINTERFACES}
procedure SetVariantToInterface(V: PIFVariant; Cl: IUnknown);
begin
  if (v <> nil) and (v.FType.BaseType = btInterface) then
  begin
    PPSVariantinterface(v).Data := cl;
    {$IFNDEF Delphi3UP}
    if PPSVariantinterface(v).Data <> nil then
      PPSVariantinterface(v).Data.AddRef;
    {$ENDIF}
  end;
end;
{$ENDIF}

procedure SetVariantToClass(V: PIFVariant; Cl: TObject);
begin
  if (v <> nil) and (v.FType.BaseType = btClass) then
  begin
    PPSVariantclass(v).Data := cl;
  end;
end;

function BGRFW(var s: TbtString): TbtString;
var
  l: Longint;
begin
  l := Length(s);
  while l >0 do
  begin
    if s[l] = ' ' then
    begin
      Result := copy(s, l + 1, Length(s) - l);
      Delete(s, l, Length(s) - l + 1);
      exit;
    end;
    Dec(l);
  end;
  Result := s;
  s := '';
end;

{$ifdef CPUX64}
{.$DEFINE empty_methods_handler}
{$endif}
{$ifdef fpc}
  {+}
  //{$if defined(cpupowerpc) or defined(cpuarm) or defined(cpu64)}
  {$if defined(cpupowerpc) or defined(cpuarm)} // cpu64 include cpuX64
    {$define empty_methods_handler}
  {$ifend}
  //{$if defined(cpu64)}
  //  {$IFNDEF MSWINDOWS}
  //    {$define empty_methods_handler}
  //  {$ENDIF}
  //{$ifend}
  {+.}
{$endif fpc}

{$ifdef empty_methods_handler}
procedure MyAllMethodsHandler;
begin
end;
{$else !empty_methods_handler}

function MyAllMethodsHandler2({RCX:}Self: PScriptMethodInfo; const {RDX:}Stack: PPointer;
  {R8:}_EDX, {R9:}_ECX: Pointer): Integer; forward;

procedure MyAllMethodsHandler;
// TODO: describe the logic
{$ifdef CPUX64}
{$IFDEF FPC}{$ASMMODE INTEL}{$ENDIF}
//  On entry:
//  RCX = Self pointer
//  RDX, R8, R9 = param1 .. param3
//  STACK = param4... paramcount
asm
  PUSH  R9
  MOV   R9,R8     // R9  := param: _ECX
  MOV   R8,RDX    // R8  := param: _EDX
  MOV   RDX, RSP  // RDX := param: Stack
  SUB   RSP, 20h  // # 'stack' size: == $20 == 32 == "param count" * 8 = 4 *8
  CALL  MyAllMethodsHandler2
  ADD   RSP, 20h  // # Restore 'stack' == $20 == 32 == "param count" * 8 = 4 *8
  POP   R9
end;
{$else !CPUX64}
//  On entry:
//     EAX = Self pointer
//     EDX, ECX = param1 and param2
//     STACK = param3... paramcount
asm
  {+}
  {$IFDEF FPC}
  push DWORD(0)
  {$ELSE}
  push 0
  {$ENDIF}
  {+.}
  push ecx
  push edx
  mov edx, esp
  add edx, 16 // was 12
  pop ecx
  call MyAllMethodsHandler2
  pop ecx
  mov edx, [esp]
  add esp, eax
  mov [esp], edx
  mov eax, ecx
end;
{$endif !CPUX64}

function ResultAsRegister(b: TPSTypeRec): Boolean;
begin
  case b.BaseType of
    btSingle,
    btDouble,
    btExtended,
    btU8,
    btS8,
    btS16,
    btU16,
    btS32,
    btU32,
    {$IFDEF PS_FPCSTRINGWORKAROUND}
    btString,
    {$ENDIF}
    {$IFNDEF PS_NOINT64}
    btS64,
    {$ENDIF}
    btPChar,
    {$IFNDEF PS_NOWIDESTRING}
    {+}
    {$if declared(btPWideChar)}btPWideChar,{$ifend}
    {+.}
    btWideChar,
    {$ENDIF}
    btChar,
    btClass,
    btEnum:
      Result := True;
    btSet:
      Result := b.RealSize <= PointerSize;
    btStaticArray:
      Result := b.RealSize <= PointerSize;
  else
    Result := False;
  end;
end;

function SupportsRegister(b: TPSTypeRec): Boolean;
begin
  case b.BaseType of
    btU8,
    btS8,
    btS16,
    btU16,
    btS32,
    btU32,
    btString,
    btClass,
    {$IFNDEF PS_NOINTERFACES}
    btInterface,
    {$ENDIF}
    btPChar,
    {$IFNDEF PS_NOWIDESTRING}
    {+}
    {$if declared(btPWideChar)}btPWideChar,{$ifend}
    {+.}
    btWideString,
    btUnicodeString,
    btWideChar,
    {$ENDIF}
    btChar,
    btArray,
    btEnum:
      Result := True;
    btSet:
      Result := b.RealSize <= PointerSize;
    btStaticArray:
      Result := b.RealSize <= PointerSize;
  else
    Result := False;
  end;
end;

function AlwaysAsVariable(aType: TPSTypeRec): Boolean;
begin
  case atype.BaseType of
    btVariant:
      Result := True;
    btSet:
      Result := atype.RealSize > PointerSize;
    btRecord:
      Result := atype.RealSize > PointerSize;
    btStaticArray:
      Result := atype.RealSize > PointerSize;
  else
    Result := False;
  end;
end;

procedure PutOnFPUStackExtended(ft: extended);
{$if defined(FPC) and defined(CPU64)}
  {$ASMMODE ATT}
{$ifend}
asm
  //fstp tbyte ptr [ft]
  {$if defined(FPC) and defined(CPU64)}
  fld %st(1) //TODO: FPC Test
  {$else}
  fld tbyte ptr [ft]
  {$ifend}
end;

function MyAllMethodsHandler2(Self: PScriptMethodInfo; const Stack: PPointer; _EDX, _ECX: Pointer): Integer;
var
  Decl: TbtString;
  I, C, RegNo: Integer;
  Params: TPSList;
  Res, Tmp: PIFVariant;
  cpt: PIFTypeRec;
  fmod: TbtChar;
  s,e: TbtString;
  FStack: Pointer;
  ex: TPSExceptionHandler;
  sError: TbtString;
begin
  Result := 0;
  Decl := TPSInternalProcRec(Self^.Se.FProcs[Self^.ProcNo]).ExportDecl;
  FStack := Stack;
  Params := TPSList.Create;
  s := decl;
  grfw(s);
  while (Length(s) > 0) do begin
    Params.Add(nil);
    grfw(s);
  end;
  s := Decl;
  grfw(s);
  RegNo := 0;
  C := Params.Count;
  for i := C-1 downto 0 do begin
    e := grfw(s);
    fmod := e[1];
    Delete(e, 1, 1);
    cpt := Self.Se.GetTypeNo(StrToInt(e));
    if ((fmod = '%') or (fmod = '!') or (AlwaysAsVariable(cpt))) and (RegNo < 2) then begin
      tmp := CreateHeapVariant(self.Se.FindType2(btPointer));
      PPSVariantPointer(tmp).DestType := cpt;
      Params[i] := tmp;
      case RegNo of
        0: begin
          PPSVariantPointer(tmp).DataDest := Pointer(_EDX);
          Inc(RegNo);
        end;
        1: begin
          PPSVariantPointer(tmp).DataDest := Pointer(_ECX);
          Inc(RegNo);
        end;
        //else begin
        //  PPSVariantPointer(tmp).DataDest := Pointer(FStack^);
        //  FStack := Pointer(IPointer(FStack) + 4);
        //end;
      end;
    end else if SupportsRegister(cpt) and (RegNo < 2) then begin
      tmp := CreateHeapVariant(cpt);
      Params[i] := tmp;
      case RegNo of
        0: begin
          CopyArrayContents(@PPSVariantData(tmp)^.Data, @_EDX, 1, cpt);
          Inc(RegNo);
        end;
        1: begin
          CopyArrayContents(@PPSVariantData(tmp)^.Data, @_ECX, 1, cpt);
          Inc(RegNo);
        end;
        //else begin
        //  CopyArrayContents(@PPSVariantData(tmp)^.Data, Pointer(FStack), 1, cpt);
        //  FStack := Pointer(IPointer(FStack) + 4);
        //end;
      end;
    //end else begin
    //  tmp := CreateHeapVariant(cpt);
    //  Params[i] := tmp;
    //  CopyArrayContents(@PPSVariantData(tmp)^.Data, Pointer(FStack), 1, cpt);
    //  FStack := Pointer(IPointer(FStack) + cpt.RealSize + 3 and not 3);
    end;
  end; // for i
  s := Decl;
  e := grfw(s);

  if e <> '-1' then begin
    cpt := Self.Se.GetTypeNo(StrToInt(e));
    if not ResultAsRegister(cpt) then begin
      Res := CreateHeapVariant(Self.Se.FindType2(btPointer));
      PPSVariantPointer(Res).DestType := cpt;
      Params.Add(Res);
      case RegNo of
        0: begin
          PPSVariantPointer(Res).DataDest := Pointer(_EDX);
        end;
        1: begin
          PPSVariantPointer(Res).DataDest := Pointer(_ECX);
        end;
        else begin
          PPSVariantPointer(Res).DataDest := Pointer(FStack^);
          Inc(Result, PointerSize);
        end;
      end; // case
    end else begin
      Res := CreateHeapVariant(cpt);
      Params.Add(Res);
    end;
  end else begin
    Res := nil;
  end;
  s := decl;
  grfw(s);
  for i := 0 to c-1 do begin
    e := grlw(s);
    fmod := e[1];
    Delete(e, 1, 1);
    if Params[i] <> nil then
      Continue;
    cpt := Self.Se.GetTypeNo(StrToInt(e));
    if (fmod = '%') or (fmod = '!') or (AlwaysAsVariable(cpt)) then begin
      tmp := CreateHeapVariant(self.Se.FindType2(btPointer));
      PPSVariantPointer(tmp).DestType := cpt;
      Params[i] := tmp;
      PPSVariantPointer(tmp).DataDest := Pointer(FStack^);
      FStack := Pointer(IPointer(FStack) + PointerSize);
      Inc(Result, PointerSize);
    end
    //else if SupportsRegister(cpt) then begin
    //  tmp := CreateHeapVariant(cpt);
    //  Params[i] := tmp;
    //  CopyArrayContents(@PPSVariantData(tmp)^.Data, Pointer(FStack), 1, cpt);
    //  FStack := Pointer(IPointer(FStack) + 4);
    //  end;
    //end
    else begin
      tmp := CreateHeapVariant(cpt);
      Params[i] := tmp;
      CopyArrayContents(@PPSVariantData(tmp)^.Data, Pointer(FStack), 1, cpt);
      FStack := Pointer((IPointer(FStack) + cpt.RealSize + 3) and not 3);
      Inc(Result, (cpt.RealSize + 3) and not 3);
    end;
  end; // for i
  ex := TPSExceptionHandler.Create;
  ex.FinallyOffset := InvalidVal;
  ex.ExceptOffset := InvalidVal;
  ex.Finally2Offset := InvalidVal;
  ex.EndOfBlock := InvalidVal;
  ex.CurrProc := nil;
  ex.BasePtr := Self.Se.FCurrStackBase;
  Ex.StackSize := Self.Se.FStack.Count;
  i :=  Self.Se.FExceptionStack.Add(ex);
  Self.Se.RunProc(Params, Self.ProcNo); //@dbg: Self.Se.LoadDebugInfo();  Self.Se.FCurrentRow;  Self.Se.FCurrentFile
  if Self.Se.FExceptionStack[i] = ex then begin
    Self.Se.FExceptionStack.Remove(ex);
    ex.Free;
  end;

  if (Res <> nil) then begin
    Params.DeleteLast;
    if (ResultAsRegister(Res.FType)) then begin
      if (res^.FType.BaseType = btSingle) or (res^.FType.BaseType = btDouble) or
        (res^.FType.BaseType = btCurrency) or (res^.Ftype.BaseType = btExtended) then
      begin
        case Res^.FType.BaseType of
          btSingle:
            PutOnFPUStackExtended(PPSVariantSingle(res).Data);
          btDouble:
            PutOnFPUStackExtended(PPSVariantDouble(res).Data);
          btExtended:
            PutOnFPUStackExtended(PPSVariantExtended(res).Data);
          btCurrency:
            PutOnFPUStackExtended(PPSVariantCurrency(res).Data);
        end;
        DestroyHeapVariant(Res);
        Res := nil;
      end else begin
        {$IFNDEF PS_NOINT64}
        if res^.FType.BaseType <> btS64 then
        {$ENDIF}
        begin
          //CopyArrayContents(Pointer(Longint(Stack)-PointerSize2), @PPSVariantData(res)^.Data, 1, Res^.FType);
          {+}
          //CopyArrayContents(Pointer(Longint(Stack)-Longint(PointerSize2)), @PPSVariantData(res)^.Data, 1, Res^.FType);
          CopyArrayContents(PointerShift(Stack,-NativeInt(PointerSize2)), @PPSVariantData(res)^.Data, 1, Res^.FType);
          {+.}
        end;
      end;
    end;
    DestroyHeapVariant(res);
  end;
  for i := 0 to Params.Count-1 do
    DestroyHeapVariant(Params[i]);
  Params.Free;
  if Self.Se.ExEx <> erNoError then begin
    if Self.Se.ExObject <> nil then begin
      FStack := Self.Se.ExObject;
      Self.Se.ExObject := nil;
      raise TObject(FStack);
    end else begin
      // @dbg: TPSInternalProcRec(Self^.Se.FProcs[Self^.ProcNo]).FExportName
      // @dbg: TPSInternalProcRec(Self^.Se.FProcs[Self.Se.ExProc]).FExportName
      // @dbg: TPSExceptionHandler(Self.Se.FExceptionStack.Data[Self.Se.FExceptionStack.Count-1]).ExceptionParam
      sError := Self.Se.GetCurrentPositionDebugInfo('; ');
      sError := PSErrorToString(Self.SE.ExceptionCode, Self.Se.ExceptionString) + sError;
      raise EPSException.Create(sError, Self.Se, Self.Se.ExProc, Self.Se.ExPos);
    end;
  end;
end; // function MyAllMethodsHandler2
{$endif !empty_methods_handler}

function TPSRuntimeClassImporter.FindClass(const Name: TbtString): TPSRuntimeClass;
var
  h, i: Longint;
  lName: TbtString;
  p: TPSRuntimeClass;
begin
  lName := FastUpperCase(Name);
  h := MakeHash(lName);
  for i := FClasses.Count-1 downto 0 do
  begin
    p := FClasses[i];
    if (p.FClassNameHash = h) and (p.FClassName = lName) then
    begin
      Result := P;
      exit;
    end;
  end;
  Result := nil;
end;

function DelphiFunctionProc(Caller: TPSExec; p: TPSExternalProcRec; {%H-}Global, Stack: TPSStack; CC: TPSCallingConvention): Boolean;
var
  i: Integer;
  MyList: TPSList;
  n: PPSVariantIFC;
  CurrStack: Cardinal;
  s: TbtString;
begin
  i := length(P.Decl);
  if i = 0 then begin Result := False; exit; end;
  s := P.Decl;
  CurrStack := Cardinal(Stack.Count) - Cardinal(i);
  if s[1] = #0 then inc(CurrStack);
  n := nil;
  MyList := TPSList.Create;
  try
    for i := 2 to length(s) do
    begin
      MyList.Add(nil);
    end;
    for i := length(s) downto 2 do
    begin // @dbg: Stack[CurrStack].FType,r
      MyList[i - 2] := NewPPSVariantIFC(Stack[CurrStack], s[i] <> #0); // TODO: when "Stack[CurrStack].FType <> uPSComliler.pas:TPSParametersDecl.Params[CurrStack].FType"
      inc(CurrStack);                                                  //       sample: "10(string) <> 28(unicode)" => need convertation
    end;
    if s[1] <> #0 then
      n := NewPPSVariantIFC(Stack[CurrStack], True);
    Result := Caller.InnerfuseCall({self:}p.Ext2, {addr:}p.Ext1, cc, {params:}MyList, {res:}n); //@dbg: p.FName
  finally
    DisposePPSVariantIFC(n);
    DisposePPSVariantIFCList(mylist);
  end;
end;

function DelphiFunctionProc_CDECL(Caller: TPSExec; p: TPSExternalProcRec; Global, Stack: TPSStack): Boolean;
begin
  Result := DelphiFunctionProc(Caller, p, Global, Stack, cdCdecl);
end;
function DelphiFunctionProc_Register(Caller: TPSExec; p: TPSExternalProcRec; Global, Stack: TPSStack): Boolean;
begin
  Result := DelphiFunctionProc(Caller, p, Global, Stack, cdRegister);
end;
function DelphiFunctionProc_Pascal(Caller: TPSExec; p: TPSExternalProcRec; Global, Stack: TPSStack): Boolean;
begin
  Result := DelphiFunctionProc(Caller, p, Global, Stack, cdPascal);
end;
function DelphiFunctionProc_Stdcall(Caller: TPSExec; p: TPSExternalProcRec; Global, Stack: TPSStack): Boolean;
begin
  Result := DelphiFunctionProc(Caller, p, Global, Stack, cdStdCall);
end;
function DelphiFunctionProc_Safecall(Caller: TPSExec; p: TPSExternalProcRec; Global, Stack: TPSStack): Boolean;
begin
  Result := DelphiFunctionProc(Caller, p, Global, Stack, cdSafeCall);
end;

procedure TPSExec.RegisterDelphiFunction(ProcPtr: Pointer; const Name: TbtString; CC: TPSCallingConvention);
begin
  RegisterDelphiMethod(nil, ProcPtr, Name, CC);
end;

procedure TPSExec.RegisterDelphiMethod(ASelf, ProcPtr: Pointer; const Name: TbtString; CC: TPSCallingConvention);
const
  ADelphiFunctionProc: array [TPSCallingConvention] of TPSProcPtr = (
    //cdRegister:
    DelphiFunctionProc_Register,
    //cdPascal:
    DelphiFunctionProc_Pascal,
    //cdCDecl:
    DelphiFunctionProc_CDECL,
    //cdStdCall:
    DelphiFunctionProc_Stdcall,
    //cdSafeCall:
    DelphiFunctionProc_Safecall
  );
begin
  RegisterFunctionName(FastUpperCase(Name), ADelphiFunctionProc[CC], ProcPtr, ASelf);
end;

{ EPSException }

constructor EPSException.Create(const Error: TbtString; Exec: TPSExec;
  Procno, ProcPos: Cardinal);
begin
 inherited Create(string(Error));
 FExec := Exec;
 FProcNo := Procno;
 FProcPos := ProcPos;
end;

{ TPSRuntimeAttribute }

function TPSRuntimeAttribute.AddValue(aType: TPSTypeRec): PPSVariant;
begin
  Result := FValues.PushType(aType);
end;

procedure TPSRuntimeAttribute.AdjustSize;
begin
  FValues.Capacity := FValues.Length;
end;

constructor TPSRuntimeAttribute.Create(Owner: TPSRuntimeAttributes);
begin
  inherited Create;
  FOwner := Owner;
  FValues := TPSStack.Create;
end;

procedure TPSRuntimeAttribute.DeleteValue(i: Longint);
begin
  if Cardinal(i) <> Cardinal(FValues.Count-1) then
    raise Exception.Create(RPS_CanOnlySendLastItem);
  FValues.Pop;
end;

destructor TPSRuntimeAttribute.Destroy;
begin
  FreeAndNil(FValues);
  inherited;
end;

function TPSRuntimeAttribute.GetValue(I: Longint): PIFVariant;
begin
  Result := FValues[i];
end;

function TPSRuntimeAttribute.GetValueCount: Longint;
begin
  Result := FValues.Count;
end;

{ TPSRuntimeAttributes }

function TPSRuntimeAttributes.Add: TPSRuntimeAttribute;
begin
  Result := TPSRuntimeAttribute.Create(Self);
  FAttributes.Add(Result);
end;

constructor TPSRuntimeAttributes.Create(AOwner: TPSExec);
begin
  inherited Create;
  FAttributes := TPSList.Create;
  FOwner := AOwner;
end;

procedure TPSRuntimeAttributes.Delete(I: Longint);
begin
  TPSRuntimeAttribute(FAttributes[i]).Free;
  FAttributes.Delete(i);
end;

destructor TPSRuntimeAttributes.Destroy;
var
  i: Longint;
begin
  if Assigned(FAttributes) then begin
    for i := FAttributes.Count-1 downto 0 do begin
      TPSRuntimeAttribute(FAttributes[i]).Free;
      FAttributes[i] := nil;
    end;
    FreeAndNil(FAttributes);
  end;
  inherited;
end;

function TPSRuntimeAttributes.FindAttribute(
  const Name: TbtString): TPSRuntimeAttribute;
var
  n: TbtString;
  i, h: Longint;
begin
  n := FastUpperCase(Name);
  h := MakeHash(n);
  for i := 0 to FAttributes.Count-1 do
  begin
    Result := FAttributes[i];
    if (Result.AttribTypeHash = h) and (Result.AttribType = n) then
      exit;
  end;
  Result := nil;
end;

function TPSRuntimeAttributes.GetCount: Longint;
begin
   Result := FAttributes.Count;
end;

function TPSRuntimeAttributes.GetItem(I: Longint): TPSRuntimeAttribute;
begin
  Result := FAttributes[i];
end;

{ TPSInternalProcRec }

destructor TPSInternalProcRec.Destroy;
begin
  if Assigned(FData) then begin
    FreeMem(FData, FLength);
    FData := nil;
  end;
  inherited;
end;

{ TPsProcRec }

constructor TPSProcRec.Create(Owner: TPSExec);
begin
  inherited Create;
  FAttributes := TPSRuntimeAttributes.Create(Owner);
end;

destructor TPSProcRec.Destroy;
begin
  FreeAndNil(FAttributes);
  inherited;
end;

{ TPSTypeRec_Array }

procedure TPSTypeRec_Array.CalcSize;
begin
  FrealSize := PointerSize;
end;

{ TPSTypeRec_StaticArray }

procedure TPSTypeRec_StaticArray.CalcSize;
begin
  FrealSize := Cardinal(FArrayType.RealSize) * Cardinal(Size);
end;

{ TPSTypeRec_Set }

procedure TPSTypeRec_Set.CalcSize;
begin
  FrealSize := FByteSize;
end;

const
  MemDelta = 4096;

{ TPSStack }

procedure TPSStack.AdjustLength;
var
  MyLen: Longint;
begin
  MyLen := ((FLength shr 12) + 1) shl 12;
  if fCapacity < MyLen then
    SetCapacity(((MyLen + MemDelta) div MemDelta) * MemDelta);
end;

procedure TPSStack.Clear;
var
  i: Longint;
  v: Pointer;
  r: TPSTypeRec;
begin
  for i := Count-1 downto 0 do begin
    v := FData[i];
    if Assigned(v) then begin
      r := TPSTypeRec(v^);
      if Assigned(r) and (r.BaseType in NeedFinalization) then begin
        FinalizeVariant(Pointer(IPointer(v)+PointerSize), r);
        //?? PPointer(v^) := nil;
      end;
    end;
  end; // for i
  inherited Clear;
  FLength := 0;
  SetCapacity(0);
end;

constructor TPSStack.Create;
begin
  inherited Create;
  GetMem(FDataPtr, MemDelta);
  FCapacity := MemDelta;
  FLength := 0;
end;

destructor TPSStack.Destroy;
var
  i: Longint;
  v: Pointer;
  r: TPSTypeRec;
begin
  for i := Count-1 downto 0 do begin
    v := FData[i];
    if Assigned(v) then begin
      r := TPSTypeRec(v^);
      if Assigned(r) and (r.BaseType in NeedFinalization) then begin
        FinalizeVariant(Pointer(IPointer(v)+PointerSize), Pointer(r));
        //?? PPointer(v^) := nil;
      end;
    end;
  end; // for i
  if Assigned(FDataPtr) then begin
    FreeMem(FDataPtr, FCapacity);
    FDataPtr := nil;
  end;
  inherited;
end;

function TPSStack.GetBool(ItemNo: Longint): Boolean;
var
  val: PPSVariant;
begin
  if ItemNo < 0 then
    val := Items[Longint(ItemNo) + Longint(Count)]
  else
    val := Items[ItemNo];
  Result := PSGetUInt(@PPSVariantData(val).Data, val.FType) <> 0;
end;

function TPSStack.GetClass(ItemNo: Longint): TObject;
var
  val: PPSVariant;
begin
  if ItemNo < 0 then
    val := Items[Longint(ItemNo) + Longint(Count)]
  else
    val := Items[ItemNo];
  Result := PSGetObject(@PPSVariantData(val).Data, val.FType);
end;

function TPSStack.GetCurrency(ItemNo: Longint): Currency;
var
  val: PPSVariant;
begin
  if ItemNo < 0 then
    val := Items[Longint(ItemNo) + Longint(Count)]
  else
    val := Items[ItemNo];
  Result := PSGetCurrency(@PPSVariantData(val).Data, val.FType);
end;

function TPSStack.GetInt(ItemNo: Longint): Longint;
var
  val: PPSVariant;
begin
  if ItemNo < 0 then
    val := items[Longint(ItemNo) + Longint(Count)]
  else
    val := items[ItemNo];
  Result := PSGetInt(@PPSVariantData(val).Data, val.FType);
end;

{$IFNDEF PS_NOINT64}
function TPSStack.GetInt64(ItemNo: Longint): Int64;
var
  val: PPSVariant;
begin
  if ItemNo < 0 then
    val := items[Longint(ItemNo) + Longint(Count)]
  else
    val := items[ItemNo];
  Result := PSGetInt64(@PPSVariantData(val).Data, val.FType);
end;
{$ENDIF}

function TPSStack.GetItem(I: Longint): PPSVariant;
begin
  if Cardinal(I) >= Cardinal(Count) then
    Result := nil
  else
    Result := Data[i];
end;

function TPSStack.GetReal(ItemNo: Longint): Extended;
var
  val: PPSVariant;
begin
  if ItemNo < 0 then
    val := items[Longint(ItemNo) + Longint(Count)]
  else
    val := items[ItemNo];
  Result := PSGetReal(@PPSVariantData(val).Data, val.FType);
end;

function TPSStack.GetAnsiString(ItemNo: Longint): TbtString;
var
  val: PPSVariant;
begin
  if ItemNo < 0 then
    val := items[Longint(ItemNo) + Longint(Count)]
  else
    val := items[ItemNo];
  Result := PSGetAnsiString(@PPSVariantData(val).Data, val.FType);
end;

function TPSStack.GetString(ItemNo: Longint): string; // calls the native method
begin
  Result :=
    {$IFNDEF PS_NOWIDESTRING}
      {$IFDEF UNICODE}
      string(GetUnicodeString(ItemNo))
      {$ELSE}
      GetAnsiString(ItemNo)
      {$ENDIF}
    {$ELSE}
    GetAnsiString(ItemNo)
    {$ENDIF}
  ;
end;

function TPSStack.GetUInt(ItemNo: Longint): Cardinal;
var
  val: PPSVariant;
begin
  if ItemNo < 0 then
    val := items[Longint(ItemNo) + Longint(Count)]
  else
    val := items[ItemNo];
  Result := PSGetUInt(@PPSVariantData(val).Data, val.FType);
end;

function TPSStack.GetUnicodeString(ItemNo: Integer): TbtUnicodeString;
var
  val: PPSVariant;
begin
  if ItemNo < 0 then
    val := items[Longint(ItemNo) + Longint(Count)]
  else
    val := items[ItemNo];
  Result := PSGetUnicodeString(@PPSVariantData(val).Data, val.FType);
end;

{$IFNDEF PS_NOWIDESTRING}
function TPSStack.GetWideString(ItemNo: Longint): tbtWideString;
var
  val: PPSVariant;
begin
  if ItemNo < 0 then
    val := items[Longint(ItemNo) + Longint(Count)]
  else
    val := items[ItemNo];
  Result := PSGetWideString(@PPSVariantData(val).Data, val.FType);
end;
{$ENDIF}

procedure TPSStack.Pop;
var
  p1: Pointer;
  c: Longint;
begin
  c := Count-1;
  p1 := Data[c];
  DeleteLast();
  FLength := IPointer(p1) - IPointer(FDataPtr);
  if TPSTypeRec(p1^).BaseType in NeedFinalization then begin
    FinalizeVariant(Pointer(IPointer(p1)+PointerSize), Pointer(p1^));
  end;
  if ((FCapacity - FLength) shr 12) > 2 then
    AdjustLength();
end;

function TPSStack.Push(TotalSize: Longint): PPSVariant;
var
  o: Cardinal;
  p: Pointer;
begin
  o := FLength;
  FLength := (FLength + TotalSize);
  //if FLength mod PointerSize <> 0 then
  if FLength mod Longint(PointerSize) <> 0 then
    //FLength := FLength + (PointerSize - (FLength mod PointerSize));
    FLength := FLength + (Longint(PointerSize) - Longint((FLength mod Longint(PointerSize))));
  if FLength > FCapacity then AdjustLength;
  p := Pointer(IPointer(FDataPtr) + IPointer(o));
  Add(p);
  Result := P;
end;

function TPSStack.PushType(aType: TPSTypeRec): PPSVariant;
begin
  Result := Push(aType.RealSize + SizeOf(Pointer));
  Result.FType := aType;
  InitializeVariant(Pointer(IPointer(Result)+PointerSize), aType);
end;

procedure TPSStack.SetBool(ItemNo: Longint; const Data: Boolean);
var
  val: PPSVariant;
  ok: Boolean;
begin
  if ItemNo < 0 then
    val := items[Longint(ItemNo) + Longint(Count)]
  else
    val := items[ItemNo];
  ok := True;
  if Data then
    PSSetUInt(@PPSVariantData(val).Data, val.FType, ok, 1)
  else
    PSSetUInt(@PPSVariantData(val).Data, val.FType, ok, 0);
  if not ok then
    raise Exception.Create(RPS_TypeMismatch);
end;

procedure TPSStack.SetCapacity(const Value: Longint);
var
  p: Pointer;
  OOFS: IPointer;
  I: Longint;
begin
  if Value < FLength then
    raise Exception.Create(RPS_CapacityLength);
  if Value = 0 then begin
    if FDataPtr <> nil then begin
      FreeMem(FDataPtr, FCapacity);
      FDataPtr := nil;
    end;
    FCapacity := 0;
  end;
  GetMem(p, Value);
  if FDataPtr <> nil then begin
    if FLength > FCapacity then
      OOFS := FCapacity
    else
      OOFS := FLength;
    Move(FDataPtr^, p^, OOFS);
    OOFS := IPointer(P) - IPointer(FDataPtr);

    for i := Count-1 downto 0 do begin
      Data[i] := Pointer(IPointer(Data[i]) + OOFS);
      if Items[i].FType.FBaseType = btPointer then begin // check if pointer points to moved stack data
        if (IPointer(PPSVariantPointer(Data[i]).DataDest) >= IPointer(FDataPtr)) and
           (IPointer(PPSVariantPointer(Data[i]).DataDest) <  IPointer(FDataPtr)+IPointer(FLength)) then
          PPSVariantPointer(Data[i]).DataDest := Pointer(IPointer(PPSVariantPointer(Data[i]).DataDest) + OOFS);
      end;
    end; // for i

    FreeMem(FDataPtr, FCapacity);
  end;
  FDataPtr := p;
  FCapacity := Value;
end;

procedure TPSStack.SetClass(ItemNo: Longint; const Data: TObject);
var
  val: PPSVariant;
  ok: Boolean;
begin
  if ItemNo < 0 then
    val := items[Longint(ItemNo) + Longint(Count)]
  else
    val := items[ItemNo];
  ok := True;
  PSSetObject(@PPSVariantData(val).Data, val.FType, ok, Data);
  if not ok then
    raise Exception.Create(RPS_TypeMismatch);
end;

procedure TPSStack.SetCurrency(ItemNo: Longint; const Data: Currency);
var
  val: PPSVariant;
  ok: Boolean;
begin
  if ItemNo < 0 then
    val := items[Longint(ItemNo) + Longint(Count)]
  else
    val := items[ItemNo];
  ok := True;
  PSSetCurrency(@PPSVariantData(val).Data, val.FType, ok, Data);
  if not ok then
    raise Exception.Create(RPS_TypeMismatch);
end;

procedure TPSStack.SetInt(ItemNo: Longint; const Data: Longint);
var
  val: PPSVariant;
  ok: Boolean;
begin
  if ItemNo < 0 then
    val := items[Longint(ItemNo) + Longint(Count)]
  else
    val := items[ItemNo];
  ok := True;
  PSSetInt(@PPSVariantData(val).Data, val.FType, ok, Data);
  if not ok then
    raise Exception.Create(RPS_TypeMismatch);
end;

{$IFNDEF PS_NOINT64}
procedure TPSStack.SetInt64(ItemNo: Longint; const Data: Int64);
var
  val: PPSVariant;
  ok: Boolean;
begin
  if ItemNo < 0 then
    val := items[Longint(ItemNo) + Longint(Count)]
  else
    val := items[ItemNo];
  ok := True;
  PSSetInt64(@PPSVariantData(val).Data, val.FType, ok, Data);
  if not ok then
    raise Exception.Create(RPS_TypeMismatch);
end;
{$ENDIF}

procedure TPSStack.SetReal(ItemNo: Longint; const Data: Extended);
var
  val: PPSVariant;
  ok: Boolean;
begin
  if ItemNo < 0 then
    val := items[Longint(ItemNo) + Longint(Count)]
  else
    val := items[ItemNo];
  ok := True;
  PSSetReal(@PPSVariantData(val).Data, val.FType, ok, Data);
  if not ok then
    raise Exception.Create(RPS_TypeMismatch);
end;

procedure TPSStack.SetAnsiString(ItemNo: Longint; const Data: TbtString);
var
  val: PPSVariant;
  ok: Boolean;
begin
  if ItemNo < 0 then
    val := items[Longint(ItemNo) + Longint(Count)]
  else
    val := items[ItemNo];
  ok := True;
  PSSetAnsiString(@PPSVariantData(val).Data, val.FType, ok, Data);
  if not ok then
    raise Exception.Create(RPS_TypeMismatch);
end;

procedure TPSStack.SetString(ItemNo: Longint; const Data: string);
begin
  {$IFNDEF PS_NOWIDESTRING}
    {$IFDEF FPC}
      {$if btCharSize=1}
      SetAnsiString(ItemNo, Data);
      {$else}
      SetUnicodeString(ItemNo, Data);
      {$ifend}
    {$ELSE}
      {$IFDEF UNICODE}
      SetUnicodeString(ItemNo, Data);
      {$ELSE}
      SetAnsiString(ItemNo, Data);
      {$ENDIF}
    {$ENDIF}
  {$ELSE}
  SetAnsiString(ItemNo, Data);
  {$ENDIF}
end;

procedure TPSStack.SetUInt(ItemNo: Longint; const Data: Cardinal);
var
  val: PPSVariant;
  ok: Boolean;
begin
  if ItemNo < 0 then
    val := items[Longint(ItemNo) + Longint(Count)]
  else
    val := items[ItemNo];
  ok := True;
  PSSetUInt(@PPSVariantData(val).Data, val.FType, ok, Data);
  if not ok then
    raise Exception.Create(RPS_TypeMismatch);
end;

procedure TPSStack.SetUnicodeString(ItemNo: Integer;
  const Data: TbtUnicodeString);
var
  val: PPSVariant;
  ok: Boolean;
begin
  if ItemNo < 0 then
    val := items[Longint(ItemNo) + Longint(Count)]
  else
    val := items[ItemNo];
  ok := True;
  PSSetUnicodeString(@PPSVariantData(val).Data, val.FType, ok, Data);
end;

{$IFNDEF PS_NOWIDESTRING}
procedure TPSStack.SetWideString(ItemNo: Longint;
  const Data: tbtWideString);
var
  val: PPSVariant;
  ok: Boolean;
begin
  if ItemNo < 0 then
    val := items[Longint(ItemNo) + Longint(Count)]
  else
    val := items[ItemNo];
  ok := True;
  PSSetWideString(@PPSVariantData(val).Data, val.FType, ok, Data);
  if not ok then
    raise Exception.Create(RPS_TypeMismatch);
end;
{$ENDIF}

{$IFNDEF PS_NOIDISPATCH}
var
  DispPropertyPut: Integer = DISPID_PROPERTYPUT;
const
  LOCALE_SYSTEM_DEFAULT = 2 shl 10; // Delphi 2 doesn't define this

{+}  // TODO: Win64 FPC 2.5.x
{$IFDEF FPC}{$IFDEF CPU64}
type
   //(*
   PPSafeArray = ^PSafeArray;
   PSafeArray = ^TSafeArray;

   tagVariant = record
   case Integer of
   0: (
     vt: TVarType;
     wReserved1: Word;
     wReserved2: Word;
     wReserved3: Word;
     case Integer of
       VT_UI1:                  (bVal: Byte);
       VT_UI2:                  (uiVal: Word);
       VT_UI4:                  (ulVal: LongWord);
       VT_UI8:                  (ullVal: QWord);
       VT_I1:                   (cVal: Char);  { shortint,perhaps? But it is Char both in PSDK and Delphi }
       VT_I2:                   (iVal: Smallint);
       VT_I4:                   (lVal: Longint);
       VT_I8:                   (llVal: Int64);
       VT_R4:                   (fltVal: Single);
       VT_R8:                   (dblVal: Double);
       VT_BOOL:                 (vbool: VARIANT_BOOL);
       VT_ERROR:                (scode: HResult);
       VT_CY:                   (cyVal: Currency);
       VT_DATE:                 (date: TOleDate);
       VT_BSTR:                 (bstrVal: POleStr{WideString});
       VT_UNKNOWN:              (unkVal: Pointer{IUnknown});
       VT_DISPATCH:             (dispVal: Pointer{IDispatch});
       VT_ARRAY:                (parray: PSafeArray);
       VT_BYREF or VT_UI1:      (pbVal: PByte);
       VT_BYREF or VT_UI2:      (puiVal: PWord);
       VT_BYREF or VT_UI4:      (pulVal: PInteger);
       VT_BYREF or VT_UI8:      (pullVal: PQWord);
       VT_BYREF or VT_I1:       (pcVal: PChar); { PShortInt?? }
       VT_BYREF or VT_I2:       (piVal: PSmallint);
       VT_BYREF or VT_I4:       (plVal: PLongint);
       VT_BYREF or VT_I8:       (pllVal: PInt64);
       VT_BYREF or VT_R4:       (pfltVal: PSingle);
       VT_BYREF or VT_R8:       (pdblVal: PDouble);
       VT_BYREF or VT_BOOL:     (pbool: PVARIANT_BOOL);
       VT_BYREF or VT_ERROR:    (pscode: ^HResult);
       VT_BYREF or VT_CY:       (pcyVal: PCurrency);
       VT_BYREF or VT_DATE:     (pdate: POleDate);
       VT_BYREF or VT_BSTR:     (pbstrVal: PPOleStr);
       VT_BYREF or VT_UNKNOWN:  (punkVal: ^IUnknown);
       VT_BYREF or VT_DISPATCH: (pdispVal: ^IDispatch);
       VT_BYREF or VT_ARRAY:    (pparray: PPSafeArray);
       VT_BYREF or VT_VARIANT:  (pvarVal: PVariant);
       VT_BYREF:                (byRef: Pointer);
       VT_INT:                  (intVal: Longint);
       VT_UINT:                 (uintVal: LongWord);
       VT_BYREF or VT_DECIMAL:  (pdecVal: PDecimal);

       VT_BYREF or VT_INT:      (pintVal: PLongint);
       VT_BYREF or VT_UINT:     (puintVal: PLongWord);
     );
   1: (decVal: TDECIMAL);
   end;
   //*)
   TVariantArg = tagVariant;
{$ENDIF CPU64}{$ENDIF FPC}
{+.}

function IDispatchInvoke(Self: IDispatch; PropertySet: Boolean; const Name: string;
  const Par: array of Variant): Variant;
var
  Param: Word;
  i, ArgErr: Longint;
  DispatchId: Longint;
  DispParam: TDispParams;
  ExceptInfo: TExcepInfo;
  {+}
  sName: TbtString;
  pName: PWideChar;
  WSFreeList: TPSList;
  ArgType: Longint;
  DispArg: {ActiveX.pas:}PVariantArg;
const
  { Parameter type masks - keep in sync with decl.h/ap* enumerations}
  atTypeMask = $7F;
  atByRef    = $80;
  {+.}
begin
  if Self = nil then begin
    raise EPSException.Create('Variant is null, cannot invoke', nil, 0, 0);
  end;
  FillChar({%H-}ExceptInfo, SizeOf(ExceptInfo), 0);
  if Name = '' then begin
    DispatchId:=0;
  end else begin // @dbg: pansichar(pointer(name)) ; pwidechar(pointer(name))
    {+}
    (* Notes: ------------------------------------------------------------------

       {$if btCharIsWide}
         Name == UnicodeString(always) == string == TbtString
         Bug: but when name declared as AnsiString method wrapper not converted parameter to declared type
       {$else}
         Name == AnsiString(always) == TbtString
         Bug: but when name declared as UnicodeString or 'string is unicode' method wrapper not converted parameter to declared type
       {$ifend}

       @debug code stack:
         function TPSExec.RunScript
           ...
           Cm_C:
           ...
             if u.ClassType = TPSExternalProcRec then begin
             ...
               TPSExternalProcRec(u).ProcPtr(Self, TPSExternalProcRec(u), FGlobalVars, FStack);
                 => function DelphiFunctionProc
                       Evaluate Windows: enter text:
                         Stack[2].FType,r
                           value is differ with base type
      Notes. ------------------------------------------------------------------
    *)
    sName := PTbtString(@Name)^;
    pName := StringToOleStr(sName);
    try
      if Self = nil then
        raise Exception.Create(RPS_NILInterfaceException);
      if Self.GetIDsOfNames(GUID_NULL, @pName, 1, LOCALE_SYSTEM_DEFAULT, @DispatchId) <> S_OK then
        raise Exception.Create(RPS_UnknownMethod);
    finally
      SysFreeString(pName);
    end;
  end;
  {+.}
  DispParam.cNamedArgs := 0;
  DispParam.rgdispidNamedArgs := nil;
  DispParam.cArgs := (High(Par) + 1);

  if PropertySet then
  begin
    Param := DISPATCH_PROPERTYPUT;
    DispParam.cNamedArgs := 1;
    DispParam.rgdispidNamedArgs := @DispPropertyPut;
  end else
    Param := DISPATCH_METHOD or DISPATCH_PROPERTYGET;

  WSFreeList := TPSList.Create;
  try
    GetMem(DispParam.rgvarg, SizeOf(TVariantArg) * (High(Par) + 1));
    FillCHar(DispParam.rgvarg^, SizeOf(TVariantArg) * (High(Par) + 1), 0);
    try
      for i := 0 to High(Par)  do
      begin
        {+}
        ArgType := PVarData(@Par[High(Par)-i]).VType;
        DispArg := @(DispParam.rgvarg{+}{$IFDEF FPC}^{$ENDIF}{+.}[i]);

        //if PVarData(@Par[High(Par)-i]).VType = varString then
        if ArgType = varString then
        {+.}
        begin
          {+}
          //DispParam.rgvarg{+}{$IFDEF FPC}^{$ENDIF}{+.}[i].vt := VT_BSTR;
          DispArg.vt := VT_BSTR;
          //DispParam.rgvarg{+}{$IFDEF FPC}^{$ENDIF}{+.}[i].bstrVal := StringToOleStr(AnsiString(Par[High(Par)-i]));
          DispArg.bstrVal := StringToOleStr(AnsiString(Par[High(Par)-i]));
          //WSFreeList.Add(DispParam.rgvarg[i].bstrVal);
          WSFreeList.Add(DispArg.bstrVal);
          {+.}
        {$IFDEF UNICODE}
        //end else if (PVarData(@Par[High(Par)-i]).VType = varOleStr) or (PVarData(@Par[High(Par)-i]).VType = varUString) then
        end else if (ArgType = varOleStr) or (ArgType = varUString) then
        begin
          {+}
          //DispParam.rgvarg[i].vt := VT_BSTR;
          DispArg.vt := VT_BSTR;
          //DispParam.rgvarg[i].bstrVal := StringToOleStr(UnicodeString(Par[High(Par)-i]));
          DispArg.bstrVal := StringToOleStr(UnicodeString(Par[High(Par)-i]));
          //WSFreeList.Add(DispParam.rgvarg[i].bstrVal);
          WSFreeList.Add(DispArg.bstrVal);
          {+.}
        {$ENDIF}
        end else
        begin
          {+}
          //DispParam.rgvarg[i].vt := VT_VARIANT or VT_BYREF;
          DispArg.vt := VT_VARIANT or VT_BYREF;

          // fix set IDispatch property value (sample: .Net interop over com object)
          if (ArgType = varDispatch) then
          begin
            DispArg.vt := VT_DISPATCH; {+}{@dbg@:hook.Dispatch.Invoke}{+.} // dbg.cond:
          end
          else if (ArgType and atByRef) = atByRef then
          begin
            if ((ArgType and atTypeMask) = varDispatch) then
            begin
              DispArg.vt := VT_DISPATCH;// or VT_BYREF; // quiet crush of applications for "VT_BYREF"
            end;
          end;

          if (DispArg.vt and VT_BYREF) = VT_BYREF then
          begin
            New(
            {$IFDEF DELPHI4UP}POleVariant{$ELSE}PVariant{$ENDIF}
            //(DispParam.rgvarg[i].pvarVal));
            (DispArg.pvarVal));

            (*
            {$IFDEF DELPHI4UP}
              POleVariant
            {$ELSE}
              PVariant
            {$ENDIF}
             (DispParam.rgvarg[i].pvarVal)^ := Par[High(Par)-i];
            *)

            //Move(Par[High(Par)-i],Pointer(DispParam.rgvarg[i].pvarVal)^,
            Move(Par[High(Par)-i],Pointer(DispArg.pvarVal)^,
             SizeOf({$IFDEF DELPHI4UP}OleVariant{$ELSE}Variant{$ENDIF}))
          end
          else // copy IDispatch value:
            DispArg.dispVal := PVarData(@Par[High(Par)-i]).VDispatch;
          // fix end.
          {+.}
        end;
      end;
      i := Self.Invoke(DispatchId, GUID_NULL, LOCALE_SYSTEM_DEFAULT, Param, DispParam, @Result, @ExceptInfo, @ArgErr);
      {$IFNDEF Delphi3UP}
      try
       if not Succeeded(i) then
       begin
         if i = DISP_E_EXCEPTION then
           raise Exception.Create(OleStrToString(ExceptInfo.bstrSource)+': '+OleStrToString(ExceptInfo.bstrDescription))
         else
           raise Exception.Create(SysErrorMessage(i));
       end;
      finally
        SysFreeString(ExceptInfo.bstrSource);
        SysFreeString(ExceptInfo.bstrDescription);
        SysFreeString(ExceptInfo.bstrHelpFile);
      end;
      {$ELSE Delphi3UP}
       if not Succeeded(i) then
       begin
         if i = DISP_E_EXCEPTION then
           {$IFDEF FPC}
           raise Exception.Create(string(ExceptInfo.Source+': '+ExceptInfo.Description))
           {$ELSE}
           raise Exception.Create(string(ExceptInfo.bstrSource+': '+ ExceptInfo.bstrDescription))
           {$ENDIF !FPC}
         else
           raise Exception.Create(SysErrorMessage(i));
       end;
      {$ENDIF Delphi3UP}
    finally
      for i := 0 to High(Par)  do
      begin
        {+}
        DispArg := @(DispParam.rgvarg{+}{$IFDEF FPC}^{$ENDIF}{+.}[i]);

        //if DispParam.rgvarg[i].vt = (VT_VARIANT or VT_BYREF) then
        if (DispArg.vt and VT_BYREF) = VT_BYREF then
        {+.}
        begin
          if{$IFDEF DELPHI4UP}POleVariant{$ELSE}PVariant{$ENDIF}
            //(DispParam.rgvarg[i].pvarVal) <> nil then
            (DispArg.pvarVal) <> nil then
            Dispose(
            {$IFDEF DELPHI4UP}
             POleVariant
            {$ELSE}
             PVariant
            {$ENDIF}
             //(DispParam.rgvarg[i].pvarVal));
             (DispArg.pvarVal));
        end;
      end;
      FreeMem(DispParam.rgvarg, SizeOf(TVariantArg) * (High(Par) + 1));
    end;
  finally
    for i := WSFreeList.Count-1 downto 0 do
      SysFreeString(WSFreeList[i]);
    WSFreeList.Free;
  end;
end; // function IDispatchInvoke
{$ENDIF !PS_NOIDISPATCH}

{ TPSTypeRec_ProcPtr }

procedure TPSTypeRec_ProcPtr.CalcSize;
begin
  FRealSize := 3 * SizeOf(Pointer);
end;

end.
