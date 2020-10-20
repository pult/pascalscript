{ uPSC_dll.pas } // version: 2020.1010.1010
{----------------------------------------------------------------------------}
{ RemObjects Pascal Script                                                   }
{----------------------------------------------------------------------------}
{ Compiletime DLL importing support }
unit uPSC_dll;

{$I PascalScript.inc}
interface
{
  Function FindWindow(c1, c2: PChar): Cardinal; external 'FindWindow@user32.dll stdcall';
}
uses
  uPSCompiler, uPSUtils, SysUtils;

{$if (defined(DELPHI3UP) or defined(FPC))}
resourcestring
{$else}
const
{$ifend}
  RPS_Invalid_External = 'Invalid External';
  RPS_InvalidCallingConvention = 'Invalid Calling Convention';

function DllExternalProc(Sender: TPSPascalCompiler; Decl: TPSParametersDecl;
  const OriginalName, FExternal: TbtString): TPSRegProc;

type
  TDllCallingConvention = (
     clRegister
    ,clPascal
    ,ClCdecl
    ,ClStdCall
  );

var
  DefaultCC: TDllCallingConvention;

procedure RegisterDll_Compiletime(cs: TPSPascalCompiler);

implementation

function rpos(ch: TbtChar; const s: TbtString): Longint;
var i: Longint;
begin
  i := Length(S);
  for i := i downto 1 do begin
    if s[i] = ch then begin
      Result := i;
      Exit;
    end;
  end;
  Result := 0;
end;

function RemoveQuotes(s: TbtString): TbtString;
var L: Longint;
begin
  Result := s;
  L := Length(Result);
  if (L = 0) then
    Exit;
  if (Result[1] = '"') then begin
    Delete(Result,1,1);
    Dec(L);
  end;
  if (L > 0) and (Result[L] = '"') then
    Delete(Result, L, 1);
end;

function DllExternalProc(Sender: TPSPascalCompiler; Decl: TPSParametersDecl;
  const OriginalName, FExternal: TbtString): TPSRegProc;
var
  FuncName, Name, FuncCC, s, s2: TbtString;
  CC: TDllCallingConvention;
  DelayLoad, LoadWithAlteredSearchPath: Boolean;
begin
  Name := FastUpperCase(OriginalName);
  DelayLoad := False;
  LoadWithAlteredSearchPath := false;
  FuncCC := FExternal;
  if (Pos(TbtChar('@'), FuncCC) = 0) then begin
    Sender.MakeError('', ecCustomError, tbtString(RPS_Invalid_External));
    Result := nil;
    Exit;
  end;
  FuncName := Copy(FuncCC, 1, rpos('@', FuncCC)-1)+#0;
  Delete(FuncCc, 1, Length(FuncName));
  if (Pos(TbtChar(' '), Funccc) > 0) then begin
    if (FuncCC[1] = '"') then begin
      Delete(FuncCC, 1, 1);
      FuncName := RemoveQuotes(copy(FuncCC, 1, Pos(TbtChar('"'), FuncCC)-1))+#0+FuncName;
      Delete(FuncCC,1, Pos(TbtChar('"'), FuncCC));
      if (FuncCC <> '') and( FuncCC[1] = ' ') then Delete(FuncCC,1,1);
    end else begin
      FuncName := Copy(FuncCc, 1, Pos(TbtChar(' '),FuncCC)-1)+#0+FuncName;
      Delete(FuncCC, 1, Pos(TbtChar(' '), FuncCC));
    end;
    if (Pos(TbtChar(' '), FuncCC) > 0) then begin
      s := Copy(FuncCC, Pos(TbtChar(' '), Funccc)+1, MaxInt);
      FuncCC := FastUpperCase(Copy(FuncCC, 1, Pos(TbtChar(' '), FuncCC)-1));
      Delete(FuncCC, Pos(TbtChar(' '), Funccc), MaxInt);
      repeat
        if Pos(TbtChar(' '), s) > 0 then begin
          s2 := Copy(s, 1, Pos(TbtChar(' '), s)-1);
          Delete(s, 1, Pos(TbtChar(' '), s));
        end else begin
          s2 := s;
          s := '';
        end;
        if FastUpperCase(s2) = 'DELAYLOAD' then
          DelayLoad := True
        {$IFNDEF LINUX}
        else
        if FastUpperCase(s2) = 'LOADWITHALTEREDSEARCHPATH' then
          LoadWithAlteredSearchPath := True
        {$ENDIF}
        else begin
          Sender.MakeError('', ecCustomError, 'Invalid External');
          Result := nil;
          Exit;
        end;
      until (s = '');
    end else begin
      FuncCC := FastUpperCase(FuncCC);
    end;
    if      (FuncCC = 'STDCALL') then
      cc := ClStdCall
    else if (FuncCC = 'CDECL') then
      cc := ClCdecl
    else if (FuncCC = 'REGISTER') then
      cc := clRegister
    else if (FuncCC = 'PASCAL') then
      cc := clPascal
    else begin
      Sender.MakeError('', ecCustomError, TbtString(RPS_InvalidCallingConvention));
      Result := nil;
      Exit;
    end;
  end else begin
    FuncName := RemoveQuotes(FuncCC)+#0+FuncName;
    FuncCC := '';
    cc := DefaultCC;
  end;
  FuncName := 'dll:'+FuncName+TbtChar(cc)
    + TbtChar(bytebool(DelayLoad))
    + TbtChar(bytebool(LoadWithAlteredSearchPath))
    + DeclToBits(Decl);
  Result := TPSRegProc.Create;
  Result.ImportDecl := FuncName;
  Result.Decl.Assign(Decl);
  Result.Name := Name;
  Result.OrgName := OriginalName;
  Result.ExportName := False;
end;

procedure RegisterDll_Compiletime(cs: TPSPascalCompiler);
begin
  cs.OnExternalProc := DllExternalProc;
  cs.AddFunction('procedure UnloadDll(S: string)');
  cs.AddFunction('function DllGetLastError: LongInt');
end;

begin
  DefaultCC := clRegister;
end.
