{ Compiletime DLL importing support }
unit uPSR_dll;

{$I PascalScript.inc}
interface
uses
  uPSRuntime, uPSUtils;

procedure RegisterDLLRuntime(Caller: TPSExec);
procedure RegisterDLLRuntimeEx(Caller: TPSExec; AddDllProcImport, RegisterUnloadDLL: Boolean);

function ProcessDllImport(Caller: TPSExec; P: TPSExternalProcRec): Boolean;
function ProcessDllImportEx(Caller: TPSExec; P: TPSExternalProcRec; ForceDelayLoad: Boolean): Boolean;
function ProcessDllImportEx2(Caller: TPSExec; P: TPSExternalProcRec; ForceDelayLoad: Boolean; var DelayLoad: Boolean; var ErrorCode: LongInt): Boolean;
procedure UnloadDLL(Caller: TPSExec; const sname: tbtstring);
function UnloadProc(Caller: TPSExec; {%H-}p: TPSExternalProcRec; {%H-}Global, Stack: TPSStack): Boolean;

implementation
uses
  {$IFDEF UNIX}
  Unix, {%H-}baseunix, dynlibs, {%H-}termio, {%H-}sockets;
  {$ELSE}
  {$IFDEF KYLIX}SysUtils;{$ELSE}Windows;{$ENDIF}
  {$ENDIF}

{
p^.Ext1 contains the pointer to the Proc function
p^.ExportDecl:
  'dll:'+dllName+#0+FunctionName+#0+chr(Cc)+Chr(DelayLoad)+Chr(AlternateSearchPath)+VarParams
}

type
  TLoadedDll = record
    dllNameHash : Longint;
    dllName     : TbtString;
    dllHandle   : THandle;
  end;
  PLoadedDll = ^TLoadedDll;

  TMyExec = class(TPSExec);
  PInteger = ^Integer;

procedure LAstErrorFree(Sender: TPSExec; P: PInteger);
begin
  Dispose(P);
end;

procedure DLLSetLastError(Sender: TPSExec; P: Integer);
var pz: PInteger;
begin
  pz := Sender.FindProcResource(@LastErrorFree);
  if pz = nil then begin
    New(pz);
    Sender.AddResource(@LastErrorFree, pz);
  end;
  pz^ := P;
end;

function DLLGetLastError(Sender: TPSExec): Integer;
var pz: PInteger;
begin
  pz := Sender.FindProcResource(@LastErrorFree);
  if pz = nil then
    Result := 0
  else
    Result := pz^;
end;

procedure DllFree(Sender: TPSExec; P: PLoadedDll);
begin
  if Assigned(P) then begin
    if P^.dllHandle <> 0 then
      FreeLibrary(P^.dllHandle);
    P^.dllHandle := 0;
    Dispose(P);
  end;
end;

function LoadDll(Caller: TPSExec; P: TPSExternalProcRec; var ErrorCode: LongInt): Boolean;
var
  s, s2, s3: TbtString;
  h, i: Longint;
  ph: PLoadedDll;
  dllHandle: THandle;
  LoadWithAlteredSearchPath: Boolean;
  {$IFNDEF UNIX}
  Filename: string;
  {$ENDIF}
begin
  s := p.Decl;
  Delete(s, 1, 4);
  s2 := Copy(s, 1, Pos(TbtChar(#0), s)-1);
  Delete(s, 1, Length(s2)+1);
  h := MakeHash(s2);
  s3 := copy(s, 1, Pos(TbtChar(#0), s)-1);
  Delete(s, 1, Length(s3)+1);
  LoadWithAlteredSearchPath := ByteBool(s[3]);
  i := 2147483647; // maxint
  dllHandle := 0;
  repeat
    ph := Caller.FindProcResource2(@dllFree, i);
    if (ph = nil) then
    begin
      if s2 = '' then
      begin
        // don't pass an empty filename to LoadLibrary, just treat it as uncallable
        p.Ext2 := Pointer(1);
        ErrorCode := ERROR_MOD_NOT_FOUND;
        Result := False;
        Exit;
      end;

      {$IFDEF UNIX}
      {$DEFINE UNIX_OR_KYLIX}
      {$ENDIF}
      {$IFDEF KYLIX}
      {$DEFINE UNIX_OR_KYLIX}
      {$ENDIF}

      {$IFDEF UNIX_OR_KYLIX}
      dllhandle := LoadLibrary(PChar(s2));
      {$ELSE}
      {$IFDEF UNICODE}
      if Copy(s2, 1, 6) = '<utf8>' then
        Filename := UTF8ToUnicodeString(Copy(s2, 7, Maxint))
      else
        Filename := string(s2);
      {$ELSE}
      Filename := string(s2);
      {$ENDIF}
      if LoadWithAlteredSearchPath then
      {+}
      begin
        {$ifdef wince}
        dllHandle := LoadLibraryEx(PWideChar(WideString(Filename)), 0, LOAD_WITH_ALTERED_SEARCH_PATH);
        {$else}
        dllHandle := LoadLibraryEx(PChar(Filename), 0, LOAD_WITH_ALTERED_SEARCH_PATH);
        {$endif}
      end
      else
      begin
        {$ifdef wince}
        dllHandle := LoadLibrary(PWideChar(WideString(Filename)));
        {$else}
        dllHandle := LoadLibrary(PChar(Filename));
        {$endif}
      end;
      {+.}
      {$ENDIF}
      if dllhandle = 0 then
      begin
        p.Ext2 := Pointer(1);
        ErrorCode := GetLastError;
        Result := False;
        Exit;
      end;
      new(ph);
      ph^.dllNameHash := h;
      ph^.dllName := s2;
      ph^.dllHandle := dllHandle;
      Caller.AddResource(@DllFree, ph);
    end;
    if (ph^.dllNameHash = h) and (ph^.dllName = s2) then
    begin
      dllHandle := ph^.dllHandle;
    end;
  until dllHandle <> 0;
  {+}
  {$ifdef wince}
  p.Ext1 := GetProcAddress(dllHandle, pwidechar(widestring(s3)));
  {$else}
  p.Ext1 := GetProcAddress(dllHandle, pansichar(s3));
  {$endif}
  {+.}
  if p.Ext1 = nil then
  begin
    p.Ext2 := Pointer(1);
    ErrorCode := GetLastError;
    Result := false;
    Exit;
  end;
  Result := True;
end;

function DllProc(Caller: TPSExec; p: TPSExternalProcRec; {%H-}Global, Stack: TPSStack): Boolean;

var
  i: Longint;
  MyList: TIfList;
  n: PPSVariantIFC;
  CurrStack: Cardinal;
  cc: TPSCallingConvention;
  s: tbtstring;
  Dummy: LongInt;
begin
  if p.Ext2 <> nil then // error
  begin
    Result := false;
    Exit;
  end;
  if p.Ext1 = nil then
  begin
    if not LoadDll(Caller, P, {%H-}Dummy) then
    begin
      Result := false;
      Exit;
    end;
  end;
  s := p.Decl;
  delete(S, 1, pos(tbtchar(#0), s));
  delete(S, 1, pos(tbtchar(#0), s));
  if Length(S) < 2 then
  begin
    Result := False;
    Exit;
  end;
  cc := TPSCallingConvention(s[1]);
  delete(s, 1, 3); // cc + delayload + alternatesearchpath (delayload might also be forced!)
  CurrStack := Cardinal(Stack.Count) - Cardinal(length(s));
  if s[1] = #0 then inc(CurrStack);
  MyList := tIfList.Create;
  for i := 2 to Length(s) do
  begin
    MyList.Add(nil);
  end;
  for i := Length(s) downto 2 do
  begin
    MyList[i - 2] := NewPPSVariantIFC(Stack[CurrStack], s[i] <> #0);
    inc(CurrStack);
  end;
  if s[1] <> #0 then
  begin
    n := NewPPSVariantIFC(Stack[CurrStack], true);
  end else n := nil;
  try
    TMYExec(Caller).InnerfuseCall(nil, p.Ext1, cc, MyList, n);
    {$IFNDEF UNIX}
    DLLSetLastError(Caller, GetLastError);
    {$ENDIF}
  finally
    DisposePPSvariantIFC(n);
    DisposePPSVariantIFCList(MyList);
  end;
  Result := True;
end;

function ProcessDllImport(Caller: TPSExec; P: TPSExternalProcRec): Boolean;
begin
  Result := ProcessDllImportEx(Caller, P, False);
end;

function ProcessDllImportEx(Caller: TPSExec; P: TPSExternalProcRec; ForceDelayLoad: Boolean): Boolean;
var DelayLoad: Boolean; var ErrorCode: LongInt;
begin
  Result := ProcessDllImportEx2(Caller, P, ForceDelayLoad, {%H-}DelayLoad, {%H-}ErrorCode);
end;

function ProcessDllImportEx2(Caller: TPSExec; P: TPSExternalProcRec; ForceDelayLoad: Boolean;
  var DelayLoad: Boolean; var ErrorCode: LongInt): Boolean;
var
  s: tbtstring;
begin
  if not ForceDelayLoad then begin
    s := p.Decl;
    Delete(s,1,pos(tbtchar(#0), s));
    Delete(s,1,pos(tbtchar(#0), s));
    DelayLoad := bytebool(s[2]);
  end else
    DelayLoad := True;

  if DelayLoad then begin
    p.ProcPtr := DllProc;
    Result := True;
  end else begin
    p.ProcPtr := DllProc;
    Result := LoadDll(Caller, p, ErrorCode);
  end;
end;

function GetLastErrorProc(Caller: TPSExec; {%H-}p: TPSExternalProcRec; {%H-}Global, Stack: TPSStack): Boolean;
begin
  Stack.SetInt(-1, DLLGetLastError(Caller));
  Result := true;
end;

procedure UnloadDLL(Caller: TPSExec; const sname: tbtstring);
var
  h, i: Longint;
  pv: TPSProcRec;
  ph: PLoadedDll;
  s: tbtstring;
begin
  for i := Caller.GetProcCount -1 downto 0 do
  begin
    pv := Caller.GetProcNo(i);
    if not (pv is TPSExternalProcRec) then continue;
    if @TPSExternalProcRec(pv).ProcPtr <> @DllProc then continue;
    s := (TPSExternalProcRec(pv).Decl);
    delete(s,1,4);
    if copy(s,1,pos(tbtchar(#0),s)-1) = sname then
    begin
      TPSExternalProcRec(pv).Ext1 := nil;
    end;
  end;
  h := MakeHash(sname);
  i := 2147483647; // maxint
  repeat
    ph := Caller.FindProcResource2(@dllFree, i);
    if (ph = nil) then break;
    if (ph.dllNameHash = h) and (ph.dllName = sname) then
    begin
      FreeLibrary(ph^.dllHandle);
      Caller.DeleteResource(ph);
      dispose(ph);
    end;
  until false;
end;

function UnloadProc(Caller: TPSExec; p: TPSExternalProcRec; Global, Stack: TPSStack): Boolean;
begin
  UnloadDLL(Caller, Stack.GetAnsiString(-1));
  Result := True;
end;

procedure RegisterDLLRuntime(Caller: TPSExec);
begin
  RegisterDLLRuntimeEx(Caller, True, True);
end;

procedure RegisterDLLRuntimeEx(Caller: TPSExec; AddDllProcImport, RegisterUnloadDLL: Boolean);
begin
  if AddDllProcImport then
    Caller.AddSpecialProcImport('dll', @ProcessDllImport, nil);
  if RegisterUnloadDLL then
    Caller.RegisterFunctionName('UnloadDll', UnloadProc, nil, nil);
  Caller.RegisterFunctionName('DllGetLastError', GetLastErrorProc, nil, nil);
end;

end.
