{ uPSR_comobj.pas } // version: 2020.1010.1010
{----------------------------------------------------------------------------}
{ RemObjects Pascal Script                                                   }
{----------------------------------------------------------------------------}
unit uPSR_comobj;

{$I PascalScript.inc}
interface
uses
  uPSRuntime, uPSUtils;

procedure RIRegister_ComObj(cl: TPSExec);

implementation
{+}
uses
  SysUtils
{$IFDEF FPC}
  {$IFDEF PS_FPC_HAS_COM}
  ,ComObj
  {$ENDIF}
{$ELSE}
  {$IFDEF DELPHI3UP}
  ,ComObj
  {$ELSE}
  ,Ole2
  {$ENDIF}
{$ENDIF}
;
{+.}
{$IFNDEF DELPHI3UP}

{$if (defined(DELPHI3UP) or defined(FPC))}
resourcestring
{$else}
const
{$ifend}
  RPS_OLEError = 'OLE error %.8x';

function OleErrorMessage(ErrorCode: HResult): String;
begin
  Result := SysErrorMessage(ErrorCode);
  if Result = '' then
    Result := Format(RPS_OLEError, [ErrorCode]);
end;

{+}
type
  EPSOleError = class(EPSError);
  Exception = EPSOleError;
{+.}

procedure OleError(ErrorCode: HResult);
begin
  raise Exception.Create(OleErrorMessage(ErrorCode));
end;

procedure OleCheck(Result: HResult);
begin
  if Result < 0 then OleError(Result);
end;

procedure CreateOleObject(const ClassName: string; var Disp: IDispatch);
var
  OldDisp: IDispatch;
  ClassID: TCLSID;
  WideCharBuf: array[0..127] of WideChar;
begin
  StringToWideChar(ClassName, WideCharBuf, SizeOf(WideCharBuf) div SizeOf(WideCharBuf[0]));
  OleCheck(CLSIDFromProgID(WideCharBuf, ClassID));
  if Disp <> nil then
  begin
    OldDisp := Disp;
    Disp := nil;
    OldDisp.Release;
  end;
  OleCheck(CoCreateInstance(ClassID, nil, CLSCTX_INPROC_SERVER or
    CLSCTX_LOCAL_SERVER, IID_IDispatch, Disp));
end;

procedure GetActiveOleObject(const ClassName: string; var Disp: IDispatch);
var
  Unknown: IUnknown;
  OldDisp: IDispatch;
  ClassID: TCLSID;
  WideCharBuf: array[0..127] of WideChar;
begin
  StringToWideChar(ClassName, WideCharBuf, SizeOf(WideCharBuf) div SizeOf(WideCharBuf[0]));
  OleCheck(CLSIDFromProgID(WideCharBuf, ClassID));
  OleCheck(GetActiveObject(ClassID, nil, Unknown));
  try
    if Disp <> nil then
    begin
      OldDisp := Disp;
      Disp := nil;
      OldDisp.Release;
    end;
    OleCheck(Unknown.QueryInterface(IID_IDispatch, Disp));
  finally
    Unknown.Release;
  end;
end;

{$ENDIF !DELPHI3UP}

{+}
{$IFNDEF PS_NOINTERFACES}
{$IFDEF DELPHI3UP}
function CreateGUID(var GUID: TGUID): HRESULT;
begin
  Result := SysUtils.CreateGUID(GUID);
end;

function StringToGUID(const S: string): TGUID;
begin
  Result := SysUtils.StringToGUID(S);
end;

function GUIDToString(const GUID: TGUID): string;
begin
  Result := SysUtils.GUIDToString(GUID);
end;

function IsEqualGUID(const guid1, guid2: TGUID): Boolean;
begin
  Result := SysUtils.IsEqualGUID(guid1, guid2);
end;
{$ENDIF}
{$ENDIF}
{+.}

procedure RIRegister_ComObj(cl: TPSExec);
begin
{+}
{$if (defined(DELPHI3UP) or defined(FPC))}
  cl.RegisterDelphiFunction(@CreateGUID, 'CreateGUID', cdRegister);
  cl.RegisterDelphiFunction(@StringToGUID, 'StringToGUID', cdRegister);
  cl.RegisterDelphiFunction(@GUIDToString, 'GUIDToString', cdRegister);
  cl.RegisterDelphiFunction(@IsEqualGUID, 'IsEqualGUID', cdRegister);
{$ifend}
{$IFDEF FPC}
    {$IFNDEF PS_NOINTERFACES}{$IFDEF PS_FPC_HAS_COM}
    cl.RegisterDelphiFunction(@OleCheck, 'OleCheck', cdRegister);
    cl.RegisterDelphiFunction(@CreateComObject, 'CreateComObject', cdRegister);
    cl.RegisterDelphiFunction(@CreateOleObject, 'CreateOleObject', cdRegister);
    cl.RegisterDelphiFunction(@GetActiveOleObject, 'GetActiveOleObject', cdRegister);
    {$ENDIF}{$ENDIF}
{$ELSE !FPC}
  cl.RegisterDelphiFunction(@OleCheck, 'OleCheck', cdRegister);
  {$IFNDEF PS_NOINTERFACES}{$IFDEF DELPHI3UP}
  cl.RegisterDelphiFunction(@CreateComObject, 'CreateComObject', cdRegister);
  {$ENDIF} {$ENDIF}
  cl.RegisterDelphiFunction(@CreateOleObject, 'CreateOleObject', cdRegister);
  cl.RegisterDelphiFunction(@GetActiveOleObject, 'GetActiveOleObject', cdRegister);
{$ENDIF !FPC}
{+.}
end;

end.
