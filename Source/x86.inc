{ x86.inc } // version: 2020.1010.1010
{----------------------------------------------------------------------------}
{ RemObjects Pascal Script                                                   }
{----------------------------------------------------------------------------}
{ implementation of x86 abi }
{$ifdef FPC}
{$define PS_ARRAY_ON_STACK}
{$endif}
function RealFloatCall_Register(p: Pointer;
  _EAX, _EDX, _ECX: Cardinal;
  StackData: Pointer;
  StackDataLen: Longint // stack length are in 4 bytes. (so 1 = 4 bytes)
  ): Extended; Stdcall; // make sure all things are on stack
var
  E: Extended;
begin
  asm
    mov ecx, stackdatalen
    jecxz @@2
    mov eax, stackdata
    @@1:
    mov edx, [eax]
    push edx
    sub eax, 4
    dec ecx
    or ecx, ecx
    jnz @@1
    @@2:
    mov eax,_EAX
    mov edx,_EDX
    mov ecx,_ECX
    call p
    fstp tbyte ptr [e]
  end;
  Result := E;
end;

function RealFloatCall_Other(p: Pointer;
  StackData: Pointer;
  StackDataLen: Longint // stack length are in 4 bytes. (so 1 = 4 bytes)
  ): Extended; Stdcall; // make sure all things are on stack
var
  E: Extended;
begin
  asm
    mov ecx, stackdatalen
    jecxz @@2
    mov eax, stackdata
    @@1:
    mov edx, [eax]
    push edx
    sub eax, 4
    dec ecx
    or ecx, ecx
    jnz @@1
    @@2:
    call p
    fstp tbyte ptr [e]
  end;
  Result := E;
end;

function RealFloatCall_CDecl(p: Pointer;
  StackData: Pointer;
  StackDataLen: Longint // stack length are in 4 bytes. (so 1 = 4 bytes)
  ): Extended; Stdcall; // make sure all things are on stack
var
  E: Extended;
begin
  asm
    mov ecx, stackdatalen
    jecxz @@2
    mov eax, stackdata
    @@1:
    mov edx, [eax]
    push edx
    sub eax, 4
    dec ecx
    or ecx, ecx
    jnz @@1
    @@2:
    call p
    fstp tbyte ptr [e]
    @@5:
    mov ecx, stackdatalen
    jecxz @@2
    @@6:
    pop edx
    dec ecx
    or ecx, ecx
    jnz @@6
  end;
  Result := E;
end;

function RealCall_Register(p: Pointer;
  _EAX, _EDX, _ECX: Cardinal;
  StackData: Pointer;
  StackDataLen: Longint; // stack length are in 4 bytes. (so 1 = 4 bytes)
  ResultLength: Longint; ResEDX: Pointer): Longint; Stdcall; // make sure all things are on stack
var
  r: Longint;
begin
  asm
    mov ecx, stackdatalen
    jecxz @@2
    mov eax, stackdata
    @@1:
    mov edx, [eax]
    push edx
    sub eax, 4
    dec ecx
    or ecx, ecx
    jnz @@1
    @@2:
    mov eax,_EAX
    mov edx,_EDX
    mov ecx,_ECX
    call p
    mov ecx, resultlength
    cmp ecx, 0
    je @@5
    cmp ecx, 1
    je @@3
    cmp ecx, 2
    je @@4
    mov r, eax
    jmp @@5
    @@3:
    xor ecx, ecx
    mov cl, al
    mov r, ecx
    jmp @@5
    @@4:
    xor ecx, ecx
    mov cx, ax
    mov r, ecx
    @@5:
    mov ecx, resedx
    jecxz @@6
    mov [ecx], edx
    @@6:
  end;
  Result := r;
end;

{+}
{$if not declared(OleCheck)}
type
  EInvocationError = class(Exception);
procedure OleCheck(Result: Longint{HResult});
begin
  if not (Result and $80000000 = 0) then
    raise EInvocationError.CreateFmt('Safecall error %.8x', [Result]);
end;
{$ifend}
{+.}

function RealCall_Other(p: Pointer;
  StackData: Pointer;
  StackDataLen: Longint; // stack length are in 4 bytes. (so 1 = 4 bytes)
  ResultLength: Longint; ResEDX: Pointer): Longint; Stdcall; // make sure all things are on stack
var
  r: Longint;
begin
  asm
    mov ecx, stackdatalen
    jecxz @@2
    mov eax, stackdata
    @@1:
    mov edx, [eax]
    push edx
    sub eax, 4
    dec ecx
    or ecx, ecx
    jnz @@1
    @@2:
    call p
    mov ecx, resultlength
    cmp ecx, 0
    je @@5
    cmp ecx, 1
    je @@3
    cmp ecx, 2
    je @@4
    mov r, eax
    jmp @@5
    @@3:
    xor ecx, ecx
    mov cl, al
    mov r, ecx
    jmp @@5
    @@4:
    xor ecx, ecx
    mov cx, ax
    mov r, ecx
    @@5:
    mov ecx, resedx
    jecxz @@6
    mov [ecx], edx
    @@6:
  end;
  Result := r;
end;

function RealCall_CDecl(p: Pointer;
  StackData: Pointer;
  StackDataLen: Longint; // stack length are in 4 bytes. (so 1 = 4 bytes)
  ResultLength: Longint; ResEDX: Pointer): Longint; Stdcall; // make sure all things are on stack
var
  r: Longint;
begin
  asm
    mov ecx, stackdatalen
    jecxz @@2
    mov eax, stackdata
    @@1:
    mov edx, [eax]
    push edx
    sub eax, 4
    dec ecx
    or ecx, ecx
    jnz @@1
    @@2:
    call p
    mov ecx, resultlength
    cmp ecx, 0
    je @@5
    cmp ecx, 1
    je @@3
    cmp ecx, 2
    je @@4
    mov r, eax
    jmp @@5
    @@3:
    xor ecx, ecx
    mov cl, al
    mov r, ecx
    jmp @@5
    @@4:
    xor ecx, ecx
    mov cx, ax
    mov r, ecx
    @@5:
    mov ecx, stackdatalen
    jecxz @@7
    @@6:
    pop eax
    dec ecx
    or ecx, ecx
    jnz @@6
    mov ecx, resedx
    jecxz @@7
    mov [ecx], edx
    @@7:
  end;
  Result := r;
end;

const
  {+}
  EmptyPChar: array[0..0] of tbtChar = tbtChar(#0);
  {+.}
function TPSExec.InnerfuseCall(_Self, Address: Pointer; CallingConv: TPSCallingConvention; Params: TPSList; res: PPSVariantIFC): Boolean;
var
  Stack: ansistring;
  I: Longint;
  RegUsage: Byte;
  CallData: TPSList;
  pp: ^Byte;
  {$IFDEF FPC}
  IsConstructor, {%H-}IsVirtualCons: Boolean;
  MethodData: TMethod;
  {$ENDIF}

  EAX, EDX, ECX: Longint;

  function rp(p: PPSVariantIFC): PPSVariantIFC;
  begin
    if {+}(p = nil) or (p.aType = nil){+.} then
    begin
      result := nil;
      exit;
    end;
    if p.aType.BaseType = btPointer then
    begin
      p^.aType := Pointer(Pointer(IPointer(p^.dta) + 4)^);
      p^.Dta := Pointer(p^.dta^);
    end;
    Result := p;
  end;

  function GetPtr(fVar: PPSVariantIFC): Boolean;
  var
    varPtr: Pointer;
    UseReg: Boolean;
    TempStr: tbtstring;
    p: Pointer;
  begin
    Result := False;
    if FVar = nil then
      Exit;
    TempStr := '';
    if fVar.VarParam then
    begin
      case fvar.aType.BaseType of
        btArray: begin
          if Copy(fvar.aType.ExportName, 1, 10) = '!OPENARRAY' then begin
            p := CreateOpenArray(True, Self, FVar);
            if p = nil then
              Exit;
            {+} // [#velter#]: https://github.com/remobjects/pascalscript/pull/107
            //                 https://github.com/velter/pascalscript/commit/406ae70bd5ce46aa36c1c3e00911e761ce1e5660
            //                 https://github.com/remobjects/pascalscript/pull/107/files#diff-1
            if CallData = nil then
              CallData := TPSList.Create();
            {+.}
            CallData.Add(p);
            case RegUsage of
              0: begin
                EAX := Longint(POpenArray(p)^.Data);
                Inc(RegUsage);
              end;
              1: begin
                EDX := Longint(POpenArray(p)^.Data);
                Inc(RegUsage);
              end;
              2: begin
                ECX := Longint(POpenArray(p)^.Data);
                Inc(RegUsage);
              end;
              else begin
                Stack := StringOfChar(AnsiChar(#0),4) + Stack;
                Pointer((@Stack[1])^) := POpenArray(p)^.Data;
              end;
            end;
            case RegUsage of
              0: begin
                EAX := Longint(POpenArray(p)^.ItemCount-1);
                Inc(RegUsage);
              end;
              1: begin
                EDX := Longint(POpenArray(p)^.ItemCount-1);
                Inc(RegUsage);
              end;
              2: begin
                ECX := Longint(POpenArray(p)^.ItemCount-1);
                Inc(RegUsage);
              end;
              else begin
                Stack := StringOfChar(AnsiChar(#0),4) + Stack;
                Longint((@Stack[1])^) := POpenArray(p)^.ItemCount-1;
              end;
            end;
            Result := True;
            Exit;
          end else begin
            {$IFDEF PS_DYNARRAY}
            VarPtr := fvar.Dta;
            {$ELSE}
            Exit;
            {$ENDIF}
          end;
        end; // btArray
        btVariant,
        btSet,
        btStaticArray,
        btRecord,
        btInterface,
        btClass,
        {$IFNDEF PS_NOWIDESTRING}
        btUnicodeString, btWideString, btWideChar,
        {+}{$if declared(btPWideChar)}btPWideChar,{$ifend}{+.}
        {$ENDIF !PS_NOWIDESTRING}
        {$IFNDEF PS_NOINT64}btS64,{$ENDIF}
        btU8, btS8, btU16, btS16, btU32, btS32, btSingle, btDouble, btExtended, btString, btPChar, btChar,
        btCurrency: begin
          VarPtr := fvar.Dta;
        end;
        else begin
          Exit; //invalid type
        end;
      end; // case fvar.aType.BaseType
      case RegUsage of
        0: begin
          EAX := Longint(VarPtr);
          Inc(RegUsage);
        end;
        1: begin
          EDX := Longint(VarPtr);
          Inc(RegUsage);
        end;
        2: begin
          ECX := Longint(VarPtr);
          Inc(RegUsage);
        end;
        else begin
          Stack := StringOfChar(AnsiChar(#0),4) + Stack;
          Pointer((@Stack[1])^) := VarPtr;
        end;
      end; // case RegUsage
    end else begin
      UseReg := True;
      case fVar^.aType.BaseType of
        btSet: begin
            tempStr := StringOfChar(AnsiChar(#0),4);
            case TPSTypeRec_Set(fvar.aType).aByteSize of
              1:
                Byte((@tempStr[1])^) := Byte(fvar.dta^);
              2:
                Word((@tempStr[1])^) := Word(fvar.dta^);
              3, 4:
                Cardinal((@tempStr[1])^) := Cardinal(fvar.dta^);
              else
                Pointer((@tempStr[1])^) := fvar.dta;
            end; // Ccase
        end;

        btArray: begin
          if Copy(fvar^.aType.ExportName, 1, 10) = '!OPENARRAY' then begin
            p := CreateOpenArray(False, Self, FVar);
            if p = nil then
              Exit;
            {+} // [#velter#]
            if CallData = nil then
              CallData := TPSList.Create();
            {+.}
            CallData.Add(p);
            case RegUsage of
              0: begin
                EAX := Longint(POpenArray(p)^.Data);
                Inc(RegUsage);
              end;
              1: begin
                EDX := Longint(POpenArray(p)^.Data);
                Inc(RegUsage);
              end;
              2: begin
                ECX := Longint(POpenArray(p)^.Data);
                Inc(RegUsage);
              end;
              else begin
                Stack := StringOfChar(AnsiChar(#0),4) + Stack;
                Pointer((@Stack[1])^) := POpenArray(p)^.Data;
              end;
            end; // case RegUsage
            case RegUsage of
              0: begin
                EAX := Longint(POpenArray(p)^.ItemCount-1);
                Inc(RegUsage);
              end;
              1: begin
                EDX := Longint(POpenArray(p)^.ItemCount-1);
                Inc(RegUsage);
              end;
              2: begin
                ECX := Longint(POpenArray(p)^.ItemCount-1);
                Inc(RegUsage);
              end;
              else begin
                Stack := StringOfChar(AnsiChar(#0),4) + Stack;
                Longint((@Stack[1])^) := POpenArray(p)^.ItemCount-1;
              end;
            end; // case RegUsage
            Result := True;
            Exit;
          end else begin
            {$IFDEF PS_DYNARRAY}
            TempStr := StringOfChar(AnsiChar(#0),4);
            Pointer((@TempStr[1])^) := Pointer(fvar.Dta^);
            {$IFDEF PS_ARRAY_ON_STACK}
            UseReg := false;
            {$ENDIF}
            {$ELSE !PS_DYNARRAY}
            Exit;
            {$ENDIF !PS_DYNARRAY}
          end;
        end; // btArray

        btVariant, btStaticArray, btRecord: begin
          TempStr := StringOfChar(AnsiChar(#0),4);
          Pointer((@TempStr[1])^) := Pointer(fvar.Dta);
        end;

        btDouble: {8 bytes} begin
          TempStr := StringOfChar(AnsiChar(#0),8);
          UseReg := False;
          Double((@TempStr[1])^) := Double(fvar.dta^);
        end;

        btCurrency{8 bytes}: begin
          TempStr := StringOfChar(AnsiChar(#0),8);
          UseReg := False;
          Currency((@TempStr[1])^) := Currency(fvar.dta^);
        end;

        btSingle{4 bytes}: begin
          TempStr := StringOfChar(AnsiChar(#0),4);;
          UseReg := False;
          Single((@TempStr[1])^) := single(fvar.dta^);
        end;

        btExtended{10 bytes}: begin
          UseReg := False;
          TempStr:= StringOfChar(AnsiChar(#0),12);
          Extended((@TempStr[1])^) := extended(fvar.dta^);
        end;

        btChar, btU8, btS8: begin
          TempStr := TbtChar(fVar^.dta^) + TbtString(StringOfChar(AnsiChar(#0),3));
        end;

        {$IFNDEF PS_NOWIDESTRING}btWideChar,{$ENDIF}
        btu16, btS16: begin
          TempStr := StringOfChar(AnsiChar(#0),4);
          Word((@TempStr[1])^) := word(fVar^.dta^);
        end;

        btu32, bts32: begin
          TempStr := StringOfChar(AnsiChar(#0),4);
          Longint((@TempStr[1])^) := Longint(fVar^.dta^);
        end;

        btPChar: begin
          TempStr := StringOfChar(AnsiChar(#0),4);
          if Pointer(fvar^.dta^) = nil then
            Pointer((@TempStr[1])^) := @EmptyPChar
          else
            Pointer((@TempStr[1])^) := Pointer(fvar^.dta^);
        end;

        {+}
        {$IFNDEF PS_NOWIDESTRING}
          {$if declared(btPWideChar)}
        btPWideChar: begin
          TempStr := StringOfChar(tbtChar(#0),4);
          if Pointer(fvar^.dta^) = nil then
            Pointer((@TempStr[1])^) := @EmptyPChar
          else
            Pointer((@TempStr[1])^) := Pointer(fvar^.dta^);
        end;
          {$ifend}
        {$ENDIF !PS_NOWIDESTRING}
        {+.}

        btclass, btinterface, btString: begin
          TempStr := StringOfChar(AnsiChar(#0),4);
          Pointer((@TempStr[1])^) := pointer(fvar^.dta^);
        end;

        {$IFNDEF PS_NOWIDESTRING}
        btWideString: begin
          TempStr := StringOfChar(AnsiChar(#0),4);
          Pointer((@TempStr[1])^) := pointer(fvar^.dta^);
        end;
        btUnicodeString: begin
          TempStr := StringOfChar(AnsiChar(#0),4);
          Pointer((@TempStr[1])^) := pointer(fvar^.dta^);
        end;
        {$ENDIF !PS_NOWIDESTRING}

        btProcPtr: begin
          {$IFDEF FPC}
          MethodData := MKMethod(Self, Longint(FVar.Dta^));
          TempStr := StringOfChar(AnsiChar(#0),4);
          Pointer((@TempStr[1])^) := @MethodData;
          {$ELSE !FPC}
          TempStr := StringOfChar(AnsiChar(#0),8);
          TMethod((@TempStr[1])^) := MKMethod(Self, Longint(FVar.Dta^));
          UseReg := false;
          {$ENDIF !FPC}
        end;

        {$IFNDEF PS_NOINT64}
        bts64: begin
          TempStr:= StringOfChar(AnsiChar(#0),8);
          Int64((@TempStr[1])^) := int64(fvar^.dta^);
          UseReg := False;
        end;
        {$ENDIF !PS_NOINT64}

      end; {case}
      if UseReg then
      begin
        case RegUsage of
          0: begin EAX := Longint((@TempStr[1])^); Inc(RegUsage); end;
          1: begin EDX := Longint((@TempStr[1])^); Inc(RegUsage); end;
          2: begin ECX := Longint((@TempStr[1])^); Inc(RegUsage); end;
          else begin
          {$IFDEF FPC_OLD_FIX}
            if CallingConv = cdRegister then
            Stack := Stack + TempStr
          else
          {$ENDIF}
            Stack := TempStr + Stack;
          end;
        end; // case
      end else begin
        {$IFDEF FPC_OLD_FIX}
            if CallingConv = cdRegister then
            Stack := Stack + TempStr
          else
        {$ENDIF}
          Stack := TempStr + Stack;
      end;
    end;
    Result := True;
  end;
begin
  {$IFDEF FPC}
  if (Integer(CallingConv) and 128) <> 0 then begin
    IsVirtualCons := true;
    CAllingConv := TPSCallingConvention(Integer(CallingConv) and not 128);
  end else
    IsVirtualCons:= false;
  if (Integer(CallingConv) and 64) <> 0 then begin
    IsConstructor := true;
    CAllingConv := TPSCallingConvention(Integer(CallingConv) and not 64);
  end else
    IsConstructor := false;
  {$ENDIF FPC}
  InnerfuseCall := False;
  if Address = nil then
    Exit; // need address
  Stack := '';
  {+} // [#velter#]
  {
  CallData := TPSList.Create;
  }
  CallData := nil;
  {+.}
  res := rp(res);
  if res <> nil then
    res.VarParam := True;
  try //try
    case CallingConv of
      cdRegister: begin
        EAX := 0;
        EDX := 0;
        ECX := 0;
        RegUsage := 0;

        {$IFDEF FPC} // FIX FOR FPC constructor calls
        if IsConstructor then begin
          if not GetPtr(rp(Params[0])) then
            exit; // this goes first
          RegUsage := 2;
          EDX := Longint(_Self);
          DisposePPSVariantIFC(Params[0]);
          Params.Delete(0);
        end else
        {$ENDIF FPC}

        if Assigned(_Self) then begin // @dbg: TClass(_Self).ClassName
          RegUsage := 1;
          EAX := Longint(_Self);
        end;

        for I := 0 to Params.Count - 1 do begin
          if not GetPtr(rp(Params[I])) then
            Exit;
        end;

        if Assigned(res) then begin
          case res^.aType.BaseType of
            {$IFNDEF PS_NOWIDESTRING}btWideString, btUnicodeString,{$ENDIF}
            {$IFNDEF PS_FPCSTRINGWORKAROUND}btString,{$ENDIF}
            {$IFNDEF FPC}btArray,{$ENDIF}
            btInterface, btRecord, btVariant, btStaticArray:
              GetPtr(res);
            btSet: begin
              if TPSTypeRec_Set(res.aType).aByteSize > 4 then
                GetPtr(res);
            end;
          end; // case
          {$IFDEF DARWIN}
          if (length(Stack) mod 16) <> 0 then begin
            Stack := Stack + StringOfChar(ansichar(#32), 16 - (Length(Stack) mod 16)) ;
          end;
          {$ENDIF}
          case res^.aType.BaseType of
            btSet: begin
              case TPSTypeRec_Set(res.aType).aByteSize  of
                1:
                  Byte(res.Dta^) := RealCall_Register(Address, EAX, EDX, ECX, @Stack[Length(Stack)-3], Length(Stack) div 4, 1, nil);
                2:
                  Word(res.Dta^) := RealCall_Register(Address, EAX, EDX, ECX, @Stack[Length(Stack)-3], Length(Stack) div 4, 2, nil);
                3, 4:
                  Longint(res.Dta^) := RealCall_Register(Address, EAX, EDX, ECX, @Stack[Length(Stack)-3], Length(Stack) div 4, 4, nil);
                else
                  RealCall_Register(Address, EAX, EDX, ECX, @Stack[Length(Stack)-3], Length(Stack) div 4, 0, nil)
              end;
            end;
            btSingle:
              TbtSingle(res.Dta^) := RealFloatCall_Register(Address, EAX, EDX, ECX, @Stack[Length(Stack)-3], Length(Stack) div 4);
            btDouble:
              TbtDouble(res.Dta^) := RealFloatCall_Register(Address, EAX, EDX, ECX, @Stack[Length(Stack)-3], Length(Stack) div 4);
            btExtended:
              TbtExtended(res.Dta^) := RealFloatCall_Register(Address, EAX, EDX, ECX, @Stack[Length(Stack)-3], Length(Stack) div 4);
            btchar,btU8, btS8:
              TbtU8(res.dta^) := RealCall_Register(Address, EAX, EDX, ECX, @Stack[Length(Stack)-3], Length(Stack) div 4, 1, nil);
            {$IFNDEF PS_NOWIDESTRING}btWideChar,{$ENDIF}
            btu16, bts16:
              TbtU16(res.dta^) := RealCall_Register(Address, EAX, EDX, ECX, @Stack[Length(Stack)-3], Length(Stack) div 4, 2, nil);
            btClass: begin
              {$IFDEF DELPHI}{$warnings off}{$ENDIF} // for delphi compiler reaction on fpc...
              {$IF DEFINED (fpc) and (fpc_version < 3)}
              {$IFDEF DELPHI}{$warnings on}{$ENDIF}
              if IsConstructor or IsVirtualCons then
                tbtu32(res.dta^) := RealCall_Register(Address, EDX, EAX, ECX,
                @Stack[Length(Stack) - 3], Length(Stack) div 4, 4, nil)
              else
              {$IFEND}
              TbtU32(res.dta^) := RealCall_Register(Address, EAX, EDX, ECX,
              @Stack[Length(Stack) - 3], Length(Stack) div 4, 4, nil);
            end;
            {$IFDEF FPC}btArray,{$ENDIF}
            btu32,bts32:
              TbtU32(res.dta^) := RealCall_Register(Address, EAX, EDX, ECX, @Stack[Length(Stack)-3], Length(Stack) div 4, 4, nil);
            btPChar: begin
              PAnsiChar(res.dta^) := PAnsiChar(RealCall_Register(Address, EAX, EDX, ECX, @Stack[Length(Stack)-3], Length(Stack) div 4, 4, nil));
            end;
            {+}
            {$IFNDEF PS_NOWIDESTRING}
              {$if declared(btPWideChar)}
            btPWideChar: begin
              PWideChar(res.dta^) := PWideChar(RealCall_Register(Address, EAX, EDX, ECX, @Stack[Length(Stack)-3], Length(Stack) div 4, 4, nil));
            end;
              {$ifend}
            {$ENDIF !PS_NOWIDESTRING}
            {+.}
            {$IFNDEF PS_NOINT64}bts64:
              begin
                EAX := RealCall_Register(Address, EAX, EDX, ECX, @Stack[Length(Stack)-3], Length(Stack) div 4, 4, @EDX);
                tbts64(res.dta^) := Int64(Cardinal(EDX)) shl 32 or Cardinal(EAX);
              end;
            {$ENDIF !PS_NOINT64}
            btCurrency:
              TbtCurrency(res.Dta^) := RealFloatCall_Register(Address, EAX, EDX, ECX, @Stack[Length(Stack)-3], Length(Stack) div 4) / 10000;
            btInterface,
            btVariant,
            {$IFNDEF PS_NOWIDESTRING}btWidestring, btUnicodestring,{$ENDIF}
            {$IFNDEF FPC}btArray,{$ENDIF}
            {$IFNDEF PS_FPCSTRINGWORKAROUND}btString,{$ENDIF}
            btStaticArray, btRecord:
              RealCall_Register(Address, EAX, EDX, ECX, @Stack[Length(Stack)-3], Length(Stack) div 4, 0, nil);
            {$IFDEF PS_FPCSTRINGWORKAROUND}
            btString: begin
               eax := RealCall_Register(Address, EAX, EDX, ECX, @Stack[Length(Stack)-3], Length(Stack) div 4, 4, nil);
               Longint(res.dta^) := eax;
            end;
            {$ENDIF PS_FPCSTRINGWORKAROUND}
            else
              Exit;
          end; // case res^.aType.BaseType
        end else begin
          {$IFDEF DARWIN}
          if (length(Stack) mod 16) <> 0 then begin
            Stack := Stack + StringOfChar(ansichar(#32), 16 - (Length(Stack) mod 16)) ;
          end;
          {$ENDIF}
          RealCall_Register(Address, EAX, EDX, ECX, @Stack[Length(Stack)-3], Length(Stack) div 4, 0, nil);
        end; //if
        Result := True;
      end; // cdRegister

      cdPascal: begin
        RegUsage := 3;
        for I :=  0 to Params.Count - 1 do begin
          if not GetPtr(Params[i]) then Exit;
        end;
        if assigned(res) then begin
          case res^.aType.BaseType of
            {$IFNDEF PS_NOWIDESTRING}btWideString, btUnicodeString, {$ENDIF}btInterface, btArray, btrecord, btstring, btVariant: GetPtr(res);
          end;
        end;
        if assigned(_Self) then begin
          Stack := StringOfChar(AnsiChar(#0),4) +Stack;
          Pointer((@Stack[1])^) := _Self;
        end;
        {$IFDEF DARWIN}
        if (length(Stack) mod 16) <> 0 then begin
          Stack := Stack + StringOfChar(ansichar(#32), 16 - (Length(Stack) mod 16)) ;
        end;
        {$ENDIF}
        if assigned(res) then begin
          case res^.aType.BaseType of
            btSingle:
              tbtsingle(res^.Dta^) := RealFloatCall_Other(Address, @Stack[Length(Stack)-3], Length(Stack) div 4);
            btDouble:
              tbtdouble(res^.Dta^) := RealFloatCall_Other(Address, @Stack[Length(Stack)-3], Length(Stack) div 4);
            btExtended:
              tbtextended(res^.Dta^) := RealFloatCall_Other(Address, @Stack[Length(Stack)-3], Length(Stack) div 4);
            btChar, btU8, btS8:
              tbtu8(res^.Dta^) := RealCall_Other(Address, @Stack[Length(Stack)-3], Length(Stack) div 4, 1, nil);
            {$IFNDEF PS_NOWIDESTRING}btWideChar, {$ENDIF}btu16, bts16:
              tbtu16(res^.Dta^) := RealCall_Other(Address, @Stack[Length(Stack)-3], Length(Stack) div 4, 2, nil);
            btClass, btu32, bts32:
              tbtu32(res^.Dta^):= RealCall_Other(Address, @Stack[Length(Stack)-3], Length(Stack) div 4, 4, nil);
            btPChar: begin
              PAnsiChar(res^.dta^) := PAnsiChar(RealCall_Other(Address, @Stack[Length(Stack)-3], Length(Stack) div 4, 4, nil));
            end;
            {+}
            {$IFNDEF PS_NOWIDESTRING}
              {$if declared(btPWideChar)}
            btPWideChar: begin
              PWideChar(res^.dta^) := PWideChar(RealCall_Other(Address, @Stack[Length(Stack)-3], Length(Stack) div 4, 4, nil));
            end;
              {$ifend}
            {$ENDIF !PS_NOWIDESTRING}
            {+.}
            {$IFNDEF PS_NOINT64}
            bts64: begin
              EAX := RealCall_Other(Address, @Stack[Length(Stack)-3], Length(Stack) div 4, 4, @EDX);
              TbtS64(res^.dta^) := Int64(EAX) shl 32 or EDX;
            end;
            {$ENDIF !PS_NOINT64}
            btVariant,
            btInterface, btRecord, btString:
              RealCall_Other(Address, @Stack[Length(Stack)-3], Length(Stack) div 4, 0, nil);
          else
            Exit;
          end;
        end else
          RealCall_Other(Address, @Stack[Length(Stack)-3], Length(Stack) div 4, 0, nil);
        Result := True;
      end; // cdPascal

      cdSafeCall: begin
        RegUsage := 3;
        if assigned(res) then begin
          GetPtr(res);
        end;
        for I :=  Params.Count - 1 downto 0 do begin
          if not GetPtr(Params[i]) then Exit;
        end;
        if assigned(_Self) then begin
          Stack := StringOfChar(AnsiChar(#0),4) +Stack;
          Pointer((@Stack[1])^) := _Self;
        end;
        {$IFDEF DARWIN}
        if (length(Stack) mod 16) <> 0 then begin
          Stack := Stack + StringOfChar(ansichar(#32), 16 - (Length(Stack) mod 16)) ;
        end;
        {$ENDIF}
        OleCheck(RealCall_Other(Address, @Stack[Length(Stack)-3], Length(Stack) div 4, 4, nil));
        Result := True;
      end; // cdSafeCall

      cdCDecl: begin
        RegUsage := 3;
        if assigned(_Self) then begin
          Stack := StringOfChar(AnsiChar(#0),4);
          Pointer((@Stack[1])^) := _Self;
        end;
        for I := Params.Count - 1 downto 0 do begin
          if not GetPtr(Params[I]) then Exit;
        end;
        {$IFDEF DARWIN}
        if (length(Stack) mod 16) <> 0 then begin
          Stack := Stack + StringOfChar(ansichar(#32), 16 - (Length(Stack) mod 16)) ;
        end;
        {$ENDIF}
        if assigned(res) then begin
          case res^.aType.BaseType of
            btSingle:      tbtsingle(res^.dta^) := RealFloatCall_Cdecl(Address, @Stack[Length(Stack)-3], Length(Stack) div 4);
            btDouble:      tbtdouble(res^.dta^) := RealFloatCall_Cdecl(Address, @Stack[Length(Stack)-3], Length(Stack) div 4);
            btExtended:    tbtextended(res^.dta^) := RealFloatCall_Cdecl(Address, @Stack[Length(Stack)-3], Length(Stack) div 4);
            btCHar, btU8, btS8:    tbtu8(res^.dta^) := RealCall_Cdecl(Address, @Stack[Length(Stack)-3], Length(Stack) div 4, 1, nil);
            {$IFNDEF PS_NOWIDESTRING}btWideChar, {$ENDIF}btu16, bts16:  tbtu16(res^.dta^) := RealCall_Cdecl(Address, @Stack[Length(Stack)-3], Length(Stack) div 4, 2, nil);
            btClass, btu32, bts32:  tbtu32(res^.dta^) := RealCall_Cdecl(Address, @Stack[Length(Stack)-3], Length(Stack) div 4, 4, nil);
            btPChar: begin
              PAnsiChar(res^.dta^) := PAnsiChar(RealCall_Cdecl(Address, @Stack[Length(Stack)-3], Length(Stack) div 4, 4, nil));
            end;
            {+}
            {$IFNDEF PS_NOWIDESTRING}
              {$if declared(btPWideChar)}
            btPWideChar: begin
              PWideChar(res^.dta^) := PWideChar(RealCall_Cdecl(Address, @Stack[Length(Stack)-3], Length(Stack) div 4, 4, nil));
            end;
              {$ifend}
            {$ENDIF !PS_NOWIDESTRING}
            {+.}
            {$IFNDEF PS_NOINT64}bts64:
              begin
                EAX := RealCall_CDecl(Address, @Stack[Length(Stack)-3], Length(Stack) div 4, 4, @EDX);
                tbts64(res^.Dta^) := Int64(EDX) shl 32 or EAX;
              end;
            {$ENDIF !PS_NOINT64}
            btVariant, {$IFNDEF PS_NOWIDESTRING}btUnicodeString, btWideString, {$ENDIF}
            btInterface,
            btArray, btrecord, btstring:      begin GetPtr(res); RealCall_Cdecl(Address, @Stack[Length(Stack)-3], Length(Stack) div 4, 0, nil); end;
          else
            exit;
          end;
        end else begin
          RealCall_CDecl(Address, @Stack[Length(Stack)-3], Length(Stack) div 4, 0, nil);
        end;
        Result := True;
      end; // cdCDecl

      cdStdCall: begin
        RegUsage := 3;
        for I := Params.Count - 1 downto 0 do begin
          if not GetPtr(Params[I]) then exit;
        end;
        if assigned(_Self) then begin
          Stack := StringOfChar(AnsiChar(#0),4) + Stack;
          Pointer((@Stack[1])^) := _Self;
        end;
        if Assigned(res) then begin
          case res^.aType.BaseType of
            btSingle:
              TbtSingle(res^.dta^) := RealFloatCall_Other(Address, @Stack[Length(Stack)-3], Length(Stack) div 4);
            btDouble:
              TbtDouble(res^.dta^) := RealFloatCall_Other(Address, @Stack[Length(Stack)-3], Length(Stack) div 4);
            btExtended:
              TbtExtended(res^.dta^):= RealFloatCall_Other(Address, @Stack[Length(Stack)-3], Length(Stack) div 4);
            btChar, btU8, btS8:
              TbtU8(res^.dta^) := RealCall_Other(Address, @Stack[Length(Stack)-3], Length(Stack) div 4, 1, nil);
            {$IFNDEF PS_NOWIDESTRING}btWideChar, {$ENDIF}
            btu16, bts16:
              TbtU16(res^.dta^) := RealCall_Other(Address, @Stack[Length(Stack)-3], Length(Stack) div 4, 2, nil);
            btClass, btu32, bts32:
              TbtU32(res^.dta^) := RealCall_Other(Address, @Stack[Length(Stack)-3], Length(Stack) div 4, 4, nil);
            btPChar: begin
              PAnsiChar(res^.dta^) := PAnsiChar(RealCall_Other(Address, @Stack[Length(Stack)-3], Length(Stack) div 4, 4, nil));
            end;
            {+}
            {$IFNDEF PS_NOWIDESTRING}
              {$if declared(btPWideChar)}
            btPWideChar: begin
              PWideChar(res^.dta^) := PWideChar(RealCall_Other(Address, @Stack[Length(Stack)-3], Length(Stack) div 4, 4, nil));
            end;
              {$ifend}
            {$ENDIF !PS_NOWIDESTRING}
            {+.}
            {$IFNDEF PS_NOINT64}
            bts64: begin
              EAX := RealCall_Other(Address, @Stack[Length(Stack)-3], Length(Stack) div 4, 4, @EDX);
              TbtS64(res^.dta^) := Int64(EDX) shl 32 or EAX;
            end;
            {$ENDIF !PS_NOINT64}
            btVariant, {$IFNDEF PS_NOWIDESTRING}btUnicodeString, btWideString,{$ENDIF}
            btInterface, btArray, btrecord, btstring: begin
              GetPtr(res); RealCall_Other(Address, @Stack[Length(Stack)-3], Length(Stack) div 4, 0, nil);
            end;
            else
              Exit;
          end; // case res^.aType.BaseType
        end else begin
          RealCall_Other(Address, @Stack[Length(Stack)-3], Length(Stack) div 4, 0, nil);
        end; // if
        Result := True;
      end; // cdStdCall

    end; // case CallingConv
  {except
    //--on e: Exception do
    begin
      raise;
    end;
  end;}
  finally
    {+} // [#velter#]
    if CallData<>nil then
    begin
      for i := CallData.Count-1 downto 0 do
      begin
        pp := CallData[i];
        case pp^ of
          0: DestroyOpenArray(Self, Pointer(pp));
        end;
      end;
      CallData.Free;
    end;
    {+.}
  end;
end; // function TPSExec.InnerfuseCall
