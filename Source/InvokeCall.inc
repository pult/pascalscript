{ InvokeCall.inc } // version: 2020.1117.2330
{----------------------------------------------------------------------------}
{ RemObjects Pascal Script                                                   }
{----------------------------------------------------------------------------}

// TODO: not supported parameter types: btProcPtr: "dcc:(-x86, +x64, ?), fpc(?)"
// TODO: only tested on DX 10.4

{$IFDEF _DCC_MSG_}
  {$MESSAGE 'Note: RPS use "InvokeCall.inc"'}
{$ENDIF}

{$if not declared(btCharIsWide)}
const
  {$if not declared(btCharSize)}
  btCharSize = SizeOf(TbtString(nil^)[1]);
  {$ifend}
  {$IFNDEF FPC}{$warn comparison_true off}{$ENDIF}
  btCharIsWide = {$if btCharSize=2}True{$else}False{$ifend};
  {$EXTERNALSYM btCharIsWide}
  {$IFDEF _DCC_MSG_}
    {$if btCharIsWide}
      {$MESSAGE 'Note: RPS TbtString is Unicode'}
    {$else}
      {$MESSAGE 'Note: RPS TbtString is Ansi'}
    {$ifend}
  {$ENDIF}
{$ifend}

{$IFDEF _PS_TRACE_INVOK_}{$IFDEF _DCC_MSG_}

{$if not declared(BoolToTxt)}
function BoolToTxt(B: Boolean): string;
begin
  if B
  then Result := 'True'
  else Result := 'False';
end;
{$ifend}

{$if not declared(PSBaseTypeToStr)}
function PSBaseTypeToStr(Self: PIFTypeRec): string;
var i: Longint;
begin
  case Self.BaseType of
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
                              i := TPSTypeRec_Record(Self).FieldTypes.Count;
                              if (i > 0) then begin
                                Result := Result+'('
                                  + PSBaseTypeToStr(PIFTypeRec(TPSTypeRec_Record(Self).FieldTypes[0]));
                                for i := 1 to i-1 do
                                  Result := Result+','
                                    + PSBaseTypeToStr(PIFTypeRec(TPSTypeRec_Record(Self).FieldTypes[i]));
                                Result := Result + ')';
                              end;
                            end;
    btArray               : Result := 'Array of '+PSBaseTypeToStr(TPSTypeRec_Array(Self).ArrayType);
    btPointer             : Result := 'Pointer';
    btPChar               : Result := {$if btCharIsWide}'PWideChar'{$else}'PAnsiChar'{$ifend};
    btResourcePointer     : Result := 'ResourcePointer';
    btVariant             : Result := 'Variant';
  {$IFNDEF PS_NOINT64}
    btS64                 : Result := 'S64';
    {+}
  //{$if declared(btU64)}
  //btU64                 : Result := 'U64';
  //{$ifend}
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
                              + '['+IntToStr(TPSTypeRec_StaticArray(Self).Size)
                              + '] of '+PSBaseTypeToStr(TPSTypeRec_Array(Self).ArrayType)
                              ;
    btSet                 : Result := 'Set';
    btCurrency            : Result := 'Currency';
    btClass               : Result := 'Class'
                              //+ ': '+string(TPSTypeRec_Class(Self).CN)
                              ;
    btInterface           : Result := 'Interface';
    btNotificationVariant : Result := 'NotificationVariant';
    btUnicodeString       : Result := 'UnicodeString';
    {$if declared(btPWideChar)}
    btPWideChar           : Result := 'PWideChar';
    {$ifend}
    btType                : Result := 'Type';
    btEnum                : Result := 'Enum';
    btExtClass            : Result := 'ExtClass';
    else                    Result := 'Unknown '+SysUtils.IntToStr(Self.BaseType);
  end; // case
end; // function PSBaseTypeToStr
{$ifend}

{$if not declared(PSParamTypeToStr)}
function PSParamTypeToStr(P: PPSVariantIFC): string;
begin
   Result := '';
   if P = nil then
     Exit;
  Result := 'type var: '+BoolToTxt(P.VarParam)+' "'
    + 'BT: '+SysUtils.IntToStr(Integer(P.aType.BaseType))
    + ', ' + PSBaseTypeToStr(P.aType)
  ;
  if P.aType.ExportName <> '' then
    Result := Result
    + '; EN: '+string(P.aType.ExportName);
  Result := Result
    + '; SZ: '+string(SysUtils.IntToStr(Integer(P.aType.RealSize)))
    + '"'
  ;
end;
{$ifend}

{$if not declared(dbg)}
procedure dbg(const S: string);
begin
  {$IFDEF MSWINDOWS}
  begin
    OutputDebugString(PChar('rps:> ' + S));
    begin
      //{$IFDEF _DCC_MSG_}
      if IsConsole then
        writeln('rps:> ', S);
      //{$ENDIF}
    end;
  end;
  {$ELSE !MSWINDOWS}
  begin
    //if IsConsole then
    //  writeln(stderr, 'rps:> ', S);
  end;
  {$ENDIF !MSWINDOWS}
end;
{$ifend}
{$ENDIF}{$ENDIF} // {$IFDEF _PS_TRACE_INVOK_}{$IFDEF _DCC_MSG_}

type
  EInvocationError = class(Exception);

const
  EmptyPChar: array[0..1] of Byte = (0, 0);

function TPSExec.InnerfuseCall(_Self, Address: Pointer; CallingConv: TPSCallingConvention;
  Params: TPSList; res: PPSVariantIFC): Boolean;

  function rp(p: PPSVariantIFC): PPSVariantIFC;
  begin
    if (p = nil) or (p.aType = nil) then begin
      Result := nil;
      Exit;
    end;
    if (p.aType.BaseType = btPointer) then begin
      p^.aType := Pointer(Pointer(IPointer(p^.Dta) + PointerSize)^);
      p^.Dta := Pointer(p^.Dta^);
    end;
    Result := p;
  end;

  type // system.pas
    PFieldTable = ^TFieldTable;
    {$IFNDEF FPC}
    TFieldTable = packed record
      X: Word;
      Size: Cardinal;
      Count: Cardinal;
      //Fields: array [0..0] of TFieldInfo;
    end;
    {$ELSE FPC}
    TFieldTable =
      {$ifndef FPC_REQUIRES_PROPER_ALIGNMENT}
      packed
      {$endif FPC_REQUIRES_PROPER_ALIGNMENT}
    record
      Count: Word;
      //ClassTable: PFieldClassTable;
      { Fields: array[Word] of TFieldInfo;  Elements have variant size! }
    end;
    {$ENDIF FPC}
  type
    TRecordOfByte01 = packed record d: array[1..01] of Byte; end;
    TRecordOfByte02 = packed record d: array[1..02] of Byte; end;
    TRecordOfByte03 = packed record d: array[1..03] of Byte; end;
    TRecordOfByte04 = packed record d: array[1..04] of Byte; end;
    TRecordOfByte05 = packed record d: array[1..05] of Byte; end;
    TRecordOfByte06 = packed record d: array[1..06] of Byte; end;
    TRecordOfByte07 = packed record d: array[1..07] of Byte; end;
    TRecordOfByte08 = packed record d: array[1..08] of Byte; end;
    TRecordOfByte09 = packed record d: array[1..09] of Byte; end;
    TRecordOfByte10 = packed record d: array[1..10] of Byte; end;
    TRecordOfByte11 = packed record d: array[1..11] of Byte; end;
    TRecordOfByte12 = packed record d: array[1..12] of Byte; end;
    TRecordOfByte13 = packed record d: array[1..13] of Byte; end;
    TRecordOfByte14 = packed record d: array[1..14] of Byte; end;
    TRecordOfByte15 = packed record d: array[1..15] of Byte; end;
    TRecordOfByte16 = packed record d: array[1..16] of Byte; end;

  function DoInvoke(CodeAddress: Pointer; const Args: TArray<TValue>;
    CallingConvention: TCallConv; AResultType: PTypeInfo; IsStatic: Boolean{ = False};
    {%H-}IsConstructor: Boolean{ = False}; ResultTypeSize: Longint = 0): TValue;
  type
    TRecordOfByteX = packed record
      data: Byte;
    end;
  var
    TD: PTypeData;
    TI2: PTypeInfo;
    TIBuf: array[0..SizeOf(TTypeInfo) + SizeOf(TOrdType) + (2*SizeOf(Integer)) + SizeOf(PPTypeInfo)] of Byte;
    FT: PFieldTable;
  begin
    FT := nil;
    if {(AResultType = nil) and} (ResultTypeSize > 0) then begin
      case ResultTypeSize of
        01: AResultType := TypeInfo(Byte);
        02: AResultType := TypeInfo(Word);
        03: AResultType := TypeInfo(TRecordOfByte03);
        04: AResultType := TypeInfo(Cardinal);
        //04: AResultType := TypeInfo(TRecordOfByte04);
        05: AResultType := TypeInfo(TRecordOfByte05);
        06: AResultType := TypeInfo(TRecordOfByte06);
        07: AResultType := TypeInfo(TRecordOfByte07);
        08: AResultType := TypeInfo(Int64);
        //08: AResultType := TypeInfo(TRecordOfByte08);
        09: AResultType := TypeInfo(TRecordOfByte09);
        10: AResultType := TypeInfo(TRecordOfByte10);
        //{$if SizeOf(Extended) > SizeOf(Double)}
        //SizeOf(Extended):
        //   AResultType := TypeInfo(Extended);
        //{$ifend}
        11: AResultType := TypeInfo(TRecordOfByte11);
        12: AResultType := TypeInfo(TRecordOfByte12);
        13: AResultType := TypeInfo(TRecordOfByte13);
        14: AResultType := TypeInfo(TRecordOfByte14);
        15: AResultType := TypeInfo(TRecordOfByte15);
        //--{$IFDEF CPU64}
        //--SizeOf(TMethod):
        //--   AResultType := TypeInfo(TMethod); ==> AV
        //--{$ENDIF}
        //SizeOf(TVarData):
        //  AResultType := TypeInfo(TVarData);
        16: AResultType := TypeInfo(TRecordOfByte16);
        else begin
          // hack:
          AResultType := TypeInfo(TRecordOfByteX);
          //--TD := GetTypeData(AResultType); // Note: failed memory access for modification typeinfo

          // make temporary typeinfo clone
          TI2 := @TIBuf[0]; //--GetMem(TI2, SizeOf(TIBuf)); //@dbg
          with TI2^ do
          begin
            Kind := AResultType.Kind;
            Name := AResultType.Name;
          end;
          AResultType := TI2;

          TD := GetTypeData(AResultType);
          TD.RecSize := ResultTypeSize; // modify typeinfo

          FT := PFieldTable(Integer(AResultType) + Byte(PTypeInfo(AResultType).Name[0]));
          FT.Count := 1; // TODO: PC Check
        end;
      end; // case
    end;
    // XE3: IsConstructor parameter not defined
    // FPC: IsConstructor generate exception
    Result :=  {$IFNDEF FPC}System.{$ENDIF}Rtti.Invoke(CodeAddress,Args,CallingConvention,AResultType,IsStatic
      {$if defined(FPC) or defined(DELPHI23UP)}
      ,{IsConstructor:}False
      {$ifend}
    );

    if Assigned(FT) then
      FT.Count := 0; // !!! Disable AV in _FinnalizeRecord // TODO: PC Check
  end;

  function TValue_From_Raw(ValuePtr: Pointer; ValueSize: Longint; ValueCopy: Boolean = False): TValue;
  type
    TRecordOfByteX = packed record
      data: Byte;
    end;
  var
    TI, TI2: PTypeInfo;
    TIBuf: array[0..SizeOf(TTypeInfo) + SizeOf(TOrdType) + (2*SizeOf(Integer)) + SizeOf(PPTypeInfo)] of Byte;
    TD: PTypeData;
    FT: PFieldTable;
  begin
    case ValueSize of
      00: Result := TValue.Empty;
      01: Result := TValue.From<Byte>    (PByte    (ValuePtr)^);
      02: Result := TValue.From<Word>    (PWord    (ValuePtr)^);
      03: Result := TValue.From<TRecordOfByte03>   (TRecordOfByte03(ValuePtr^));
      04: Result := TValue.From<Cardinal>(PCardinal(ValuePtr)^);
      //04: Result := TValue.From<TRecordOfByte04>   (TRecordOfByte04(ValuePtr^));
      05: Result := TValue.From<TRecordOfByte05>   (TRecordOfByte05(ValuePtr^));
      06: Result := TValue.From<TRecordOfByte06>   (TRecordOfByte06(ValuePtr^));
      07: Result := TValue.From<TRecordOfByte07>   (TRecordOfByte07(ValuePtr^));
      08: Result := TValue.From<Int64>   (PInt64   (ValuePtr)^);
      //08: Result := TValue.From<TRecordOfByte08>   (TRecordOfByte08(ValuePtr^));
      09: Result := TValue.From<TRecordOfByte09>   (TRecordOfByte09(ValuePtr^));
      10: Result := TValue.From<TRecordOfByte10>   (TRecordOfByte10(ValuePtr^));
      //{$if SizeOf(Extended) > SizeOf(Double)}
      //SizeOf(Extended):
      //   Result := TValue.From<Extended>   (PExtended (ValuePtr)^);
      //{$ifend}
      11: Result := TValue.From<TRecordOfByte11>   (TRecordOfByte11(ValuePtr^));
      12: Result := TValue.From<TRecordOfByte12>   (TRecordOfByte12(ValuePtr^));
      13: Result := TValue.From<TRecordOfByte13>   (TRecordOfByte13(ValuePtr^));
      14: Result := TValue.From<TRecordOfByte14>   (TRecordOfByte14(ValuePtr^));
      15: Result := TValue.From<TRecordOfByte15>   (TRecordOfByte15(ValuePtr^));
      //--{$IFDEF CPU64}
      //--SizeOf(TMethod):
      //--   Result := TValue.From<TMethod>   (PMethod (ValuePtr)^); ==> AV
      //--{$ENDIF}
      //--SizeOf(TVarData):
      //--   Result := TValue.From<TVarData>   (PVarData (ValuePtr)^);
      16: Result := TValue.From<TRecordOfByte16>   (TRecordOfByte16(ValuePtr^));
      else begin
        TI := PTypeInfo(System.TypeInfo(TRecordOfByteX));
        //--TD := GetTypeData(TI); // Note: failed memory access for modification typeinfo
        //-if not VirtualProtect(@TD.RecSize, SizeOf(TD.RecSize), PAGE_READWRITE, nil) then
        //-  RaiseLastOsError();

        // make temporary typeinfo clone
        TI2 := @TIBuf[0]; //-GetMem(TI2, SizeOf(TIBuf)); //@dbg
        with TI2^ do
        begin
          Kind := TI.Kind;
          Name := TI.Name;
        end;
        TI := TI2;
        TD := GetTypeData(TI);

        TD.RecSize := ValueSize; // modify typeinfo
        FT := PFieldTable(Integer(TI) + Byte(PTypeInfo(TI).Name[0]));
        FT.Count := 1; // TODO: PC Check

        if ValueCopy then begin
          TValue.Make(ValuePtr, TI, Result);
        end else
        begin
          {$IFDEF FPC}
          TValue.Make(ValuePtr, TI, Result);
          {$ELSE}
          TValue.MakeWithoutCopy(ValuePtr, TI, Result);
          {$ENDIF}
        end;

        FT.Count := 0; // !!! Disable AV in _FinnalizeRecord // TODO: PC Check

      end;
    end; // case
  end;

  var
    Args: TArray<TValue>;
    ArgIdx : Integer;
  procedure AddArg(Arg : TValue);
  begin
    Inc(ArgIdx);
    Args[ArgIdx] := Arg;
  end;

var
  {$IFDEF FPC}
  IsConstructor, {%H-}IsVirtualCons: Boolean;
  {$ENDIF}
  SysCalConv : TCallConv;
  Arg : TValue;
  i : Integer;
  fvar: PPSVariantIFC;
  IsConstr : Boolean;
  ResValue : TValue;
  S: string;
  {%H-}p: Pointer;
  {%H-}pp: ^Byte;
  {%H-}CallData: TPSList;
  {%H-}ctx: TRTTIContext;
  {%H-}RttiType: TRttiType;
  {$if defined(FPC) or not defined(cpu32)} {$else}
  M: TMethod;
  {$ifend}
begin
  // ######### DEBUG: TRACE:
  {$IFDEF _PS_TRACE_INVOK_}{$IFDEF _DCC_MSG_}
  dbg('#CALL: "TPSExec.InnerfuseCall": *( Params['+SysUtils.IntToStr(Params.Count)+'] ): res{'+BoolToTxt(Assigned(res))+'}');
  {$ENDIF}{$ENDIF}
  // ######### DEBUG: TRACE.
  //
  Result := False;
  {$IFDEF FPC}
  if (Integer(CallingConv) and 128) <> 0 then begin
    IsVirtualCons := True;
    CallingConv := TPSCallingConvention(Integer(CallingConv) and not 128);
  end else begin
    IsVirtualCons := False;
  end;
  if IsVirtualCons then ;
  if (Integer(CallingConv) and 64) <> 0 then begin
    IsConstructor := True;
    CallingConv := TPSCallingConvention(Integer(CallingConv) and not 64);
  end else begin
    IsConstructor := False;
  end;
  {$ENDIF FPC}

  case CallingConv of
    cdRegister : begin SysCalConv := ccReg;       end;
    cdPascal   : begin SysCalConv := ccPascal;    end;
    cdCdecl    : begin SysCalConv := ccCdecl;     end;
    cdStdCall  : begin SysCalConv := ccStdCall;   end;
    cdSafeCall : begin SysCalConv := ccSafeCall;  end;
    {$IFNDEF FPC}
    else         begin SysCalConv := ccReg;       end; // to prevent warning "W1036 Variable might not have been initialized"
    {$ENDIF}
  end;

  {$IFNDEF FPC}{$warnings off}{$ENDIF} // dcc: Warning: W1023 Comparing signed and unsigned types - widened both operands
  {$IFDEF FPC} // FPC: FIX FOR FPC constructor calls
    //x64.inc: {$if defined(FPC) and (fpc_version >= 3)}
    //x86.inc: {$ifdef FPC} + case CallingConv of cdRegister
  {$if (defined(CPU32) or (declared(fpc_version) and (fpc_version >= 3)))} // TODO: FPC: test and check other platforms
  {$IFDEF CPU32}
  if (CallingConv = cdRegister) then
  {$ENDIF }
  begin
    if IsConstructor then begin
      fvar := Params[0];
      p := rp(fvar);
      if p = nil then
        Exit; // this goes first  // internal error
      case fVar.aType.BaseType of
        btArray,
        btVariant,
        btSet,
        btStaticArray,
        btRecord,
        btInterface,
        btClass,
        {$IFNDEF PS_NOWIDESTRING}
        btUnicodeString, btWideString, btWideChar,
        {$if declared(btPWideChar)}
        btPWideChar,
        {$ifend}
        {$ENDIF !PS_NOWIDESTRING}
        btU8, btS8, btU16,
        btS16, btU32, btS32, btSingle, btDouble, btExtended, btString, btPChar, btChar, btCurrency
        {$IFNDEF PS_NOINT64}
        ,bts64
        {$ENDIF}
        ,btProcPtr
        : { empty } ;
      else
        Exit; // this goes first  // internal error
      end; // case
      DisposePPSVariantIFC(p);
      Params.Delete(0);
    end;
  end;
  {$ifend}
  {$ENDIF FPC}
  {$IFNDEF FPC}{$warnings on}{$ENDIF}

  ArgIdx := -1;
  SetLength({%H-}Args, 2*Params.Count+1); // maximum params

  if Assigned(_Self) then
    AddArg( TValue.From<Pointer>( _Self ) );

  CallData := nil;
  try
    for I := 0 to Params.Count-1 do begin
      fvar := Params[i];
      if (fvar = nil) then begin
        S := 'Unassigned parameter #"'
          + SysUtils.IntToStr(i)
          + '"!';

        // ######### DEBUG: TRACE:
        {$IFDEF _PS_TRACE_INVOK_}{$IFDEF _DCC_MSG_}
        dbg(S);
        {$ENDIF}{$ENDIF}
        // ######### DEBUG: TRACE.

        raise EInvocationError.Create('Internal: ' + S); // optional
        Exit;
      end;

      // ######### DEBUG: TRACE:
      {$IFDEF _PS_TRACE_INVOK_}{$IFDEF _DCC_MSG_}
      {$if declared(PSParamTypeToStr)}
      S := 'INFO:    Param['+SysUtils.IntToStr(i)+']: '+PSParamTypeToStr(fvar);
      dbg(S);
      {$ifend}
      {$ENDIF}{$ENDIF}
      // ######### DEBUG: TRACE.

      p := fvar;
      fvar := rp(PPSVariantIFC(p));

      if (fvar = nil) then begin
        S := 'Unassigned parameter ref #"'
          + SysUtils.IntToStr(i)
          + '"!';

        // ######### DEBUG: TRACE:
        {$IFDEF _PS_TRACE_INVOK_}{$IFDEF _DCC_MSG_}
        dbg(S);
        {$ENDIF}{$ENDIF}
        // ######### DEBUG: TRACE.

        raise EInvocationError.Create('Internal: ' + S); // optional
        Exit;
      end;

      // ######### DEBUG: TRACE:
      if Assigned(fvar) then begin
        {$IFDEF _PS_TRACE_INVOK_}{$IFDEF _DCC_MSG_}
        {$if declared(PSParamTypeToStr)}
        S := 'INFO: rp:Param['+SysUtils.IntToStr(i)+']: '+PSParamTypeToStr(fvar)+'; Same: '+BoolToTxt(fvar=p);
        dbg(S);
        {$ifend}
        {$ENDIF}{$ENDIF}
      end;
      // ######### DEBUG: TRACE.

      Arg := nil;
      if fvar.VarParam then begin { var param }
        case fvar.aType.BaseType of
          btArray: begin
              if Copy(fVar.aType.ExportName, 1, 10) = '!OPENARRAY' then begin
                p := CreateOpenArray(True, Self, fVar);
                if p = nil then
                  raise EInvocationError.Create('Internal: ' + 'CreateOpenArray call for var param failed!');
                  //Exit; // internal error
                if CallData = nil then
                  CallData := TPSList.Create;
                CallData.Add(p);
                Arg := TValue.From<Pointer>( Pointer(POpenArray(p)^.Data) );
                AddArg(Arg);
                Arg := TValue.From<Longint>( Longint(POpenArray(p)^.ItemCount-1) ); //?? type IPointer
              end else begin
                Arg := TValue.From<Pointer>( Pointer(fvar.Dta) );
              end;
            end;
          btVariant,
          btSet,
          btStaticArray,
          btRecord,
          btInterface,
          btClass,
          {$IFNDEF PS_NOWIDESTRING}
          btUnicodeString, btWideString, btWideChar,
          {$if declared(btPWideChar)}
          btPWideChar,
          {$ifend}
          {$ENDIF !PS_NOWIDESTRING}
          btU8, btS8, btU16,
          btS16, btU32, btS32, btSingle, btDouble, btExtended,
          btString, btPChar, btChar, btCurrency
          {$IFNDEF PS_NOINT64}
          ,bts64
          {$ENDIF}: begin
              Arg := TValue.From<Pointer>( Pointer(fvar.Dta) );
          end;
          else
            begin
              //Arg := TValue.From<Pointer>( Pointer(fvar.Dta) );
              //(*
              // invalid type
              {$if declared(PSParamTypeToStr)}
              S := '@Var Param: '+PSParamTypeToStr(fvar)
                + '" not implemented!';

              // ######### DEBUG: TRACE:
              {$IFDEF _PS_TRACE_INVOK_}{$IFDEF _DCC_MSG_}
              dbg(S);
              {$ENDIF}{$ENDIF}
              // ######### DEBUG: TRACE.

              raise EInvocationError.Create('Internal: ' + S); // optional
              {$ifend} // "$if declared(PSParamTypeToStr)"
              Exit; // internal error
              //*)
            end;
        end;
      end else begin  { not a var param }
        case fvar.aType.BaseType of { add normal params here }
          {$IFNDEF PS_NOWIDESTRING}
          btWidestring:                      Arg := TValue.From<WideString>( PWideString(fvar.Dta)^ );
          btUnicodeString:                   Arg := TValue.From<UnicodeString>( PUnicodeString(fvar.Dta)^ );
          {$ENDIF}
          {$if btCharIsWide}                 // unicode string
          btString:                          Arg := TValue.From<UnicodeString>( PUnicodeString(fvar.Dta)^ );
          {$else}                            // ansi string
          btString:                          Arg := TValue.From<AnsiString>( PAnsiString(fvar.Dta)^ );
          {$ifend}
          btU8, btS8:                        Arg := TValue.From<Byte>( PByte(fvar.Dta)^ );
          btU16, BtS16:                      Arg := TValue.From<Word>( PWord(fvar.Dta)^ );
          btU32, btS32:                      Arg := TValue.From<Cardinal>( PCardinal(fvar.Dta)^ );
          {$IFNDEF PS_NOINT64}
          btS64:                             Arg := TValue.From<Int64>( PInt64(fvar.Dta)^ );
          {$ENDIF}
          btSingle:                          Arg := TValue.From<Single>( PSingle(fvar.Dta)^ );
          btDouble:                          Arg := TValue.From<Double>( PDouble(fvar.Dta)^ );
          btExtended:                        Arg := TValue.From<Extended>( PExtended(fvar.Dta)^ );
          btCurrency:                        Arg := TValue.From<Currency>( PCurrency(fvar.Dta)^ );
          {$if btCharIsWide}                 // unicode
          btPChar:                         if Pointer(fVar.Dta^) = nil then
                                             Arg := TValue.From<PWideChar>( PPWideChar(@EmptyPChar)^ )
                                           else
                                             Arg := TValue.From<PWideChar>( PPWideChar(fvar.Dta)^ );
          {$else}                            // ansi
          btPChar:                         if Pointer(fVar.Dta^) = nil then
                                             Arg := TValue.From<PAnsiChar>( PPAnsiChar(@EmptyPChar)^ )
                                           else
                                             Arg := TValue.From<PAnsiChar>( PPAnsiChar(fvar.Dta)^ );
          {$ifend}
          {$IFNDEF PS_NOWIDESTRING}
          btWideChar:                        Arg := TValue.From<WideChar>( PWideChar(fvar.Dta)^ );
            {$if declared(btPWideChar)}
          btPWideChar:                    //-Arg := TValue.From<PWideChar>( PPWideChar(fvar.Dta)^ );
                                           if Pointer(fVar.Dta^) = nil then
                                             Arg := TValue.From<PWideChar>( PPWideChar(@EmptyPChar)^ )
                                           else
                                             Arg := TValue.From<PWideChar>( PPWideChar(fvar.Dta)^ );
            {$ifend}
          {$ENDIF !PS_NOWIDESTRING}
          {$if btCharIsWide}                 // unicode char
          btChar:                            Arg := TValue.From<WideChar>( PWideChar(fvar.Dta)^ );
          {$else}                            // ansi char
          btChar:                            Arg := TValue.From<AnsiChar>( PAnsiChar(fvar.Dta)^ );
          {$ifend}
          btClass:                           Arg := TValue.From<TObject>( TObject(fvar.Dta^) );
          btPointer:                         Arg := TValue.From<Pointer>( PPointer(fvar.Dta)^ );
       //?btResourcePointer:                 Arg := TValue.From<Pointer>( PPointer(fvar.Dta)^ );
          btProcPtr:  // x64 : reference
            begin     // x86: fpc: referense; delphi: value
              {$if defined(FPC) or not defined(cpu32)} // reference:
              GetMem(p, SizeOf(TParamData)*4);
              PParamData(p).pType := 254;
              if CallData = nil then
                CallData := TPSList.Create;
              CallData.Add(p);
              PParamData(p).pMethod := MKMethod(Self, Longint(fVar.Dta^));
              p := @PParamData(p).pMethod;
              Arg := TValue.From<Pointer>( p );
              {$else} // value:
              //--Arg := TValue.From<TMethod>( MKMethod(Self, Longint(fVar.Dta^)) ); ==> AV
              M := MKMethod(Self, Longint(fVar.Dta^));
              Arg := TValue_From_Raw(@M, SizeOf(M), {copy data:}True);
              {$ifend}
            end;
          btInterface:                       Arg := TValue.From<Pointer>( PPointer(fvar.Dta)^ );
          btRecord:                          Arg := TValue_From_Raw(fvar.Dta, fVar.aType.RealSize);
          btStaticArray:                     Arg := TValue_From_Raw(fvar.Dta, fVar.aType.RealSize);
          btArray:
            begin
               if Copy(fvar.aType.ExportName, 1, 10) = '!OPENARRAY' then begin
                 p := CreateOpenArray(False, Self, fVar);
                 if p = nil then
                   raise EInvocationError.Create('Internal: ' + 'CreateOpenArray call for param failed!');
                   //Exit; // internal error
                 if CallData = nil then
                   CallData := TPSList.Create;
                 CallData.Add(p);
                 //in case of openarray we should provide TWO params: first is pointer to array,
                 Arg := TValue.From<Pointer>( Pointer(POpenArray(p)^.Data) );
                 AddArg(Arg);
                 //2nd - integer with arraylength - 1 (high)
                 Arg := TValue.From<Longint>( Longint(POpenArray(p)^.ItemCount-1) ); //?? type IPointer
               end else begin //dynarray = just push pointer:
                 Arg := TValue.From<Pointer>( PPointer(fvar.Dta){$IFNDEF FPC}^{$ENDIF} );
               end;
            end;
          btVariant:                         Arg := TValue.From<Pointer>( Pointer(fvar.Dta) );
          btSet:
            case TPSTypeRec_Set(fvar.aType).aByteSize  of
              1:       Arg := TValue.From<Byte>    (PByte    (fvar.Dta)^);
              2:       Arg := TValue.From<Word>    (PWord    (fvar.Dta)^);
              3,4:     Arg := TValue.From<Cardinal>(PCardinal(fvar.Dta)^);
              5,6,7,8: Arg := TValue.From<Int64>   (PInt64   (fvar.Dta)^);
              else     Arg := TValue.From<Pointer> (Pointer  (fvar.Dta));
            end; // case
          else begin
            {$if declared(PSParamTypeToStr)}
            S := '@Param: '+PSParamTypeToStr(fvar)
              + '" not implemented!';

            // ######### DEBUG: TRACE:
            {$IFDEF _PS_TRACE_INVOK_}{$IFDEF _DCC_MSG_}
            dbg(S);
            {$ENDIF}{$ENDIF}
            // ######### DEBUG: TRACE.

            raise EInvocationError.Create('Internal: ' + S); // optional
            {$ifend} // "$if declared(PSParamTypeToStr)"
            Exit; // internal error
          end;
        end;  { case }
      end; { if "not a var param" }
      AddArg(Arg);
    end; // for I
    SetLength(Args, ArgIdx+1); // truncate args

    IsConstr := False; //checked on Delhi, constructors works fine.

    if Assigned(res) then begin
      // ######### DEBUG: TRACE:
      {$IFDEF _PS_TRACE_INVOK_}{$IFDEF _DCC_MSG_}
      {$if declared(PSParamTypeToStr)}
      S := 'INFO:    Result: '+PSParamTypeToStr(res);
      dbg(S);
      {$ifend}
      {$ENDIF}{$ENDIF}
      // ######### DEBUG: TRACE.

      p := res;
      res := rp(PPSVariantIFC(p));
      if Assigned(res) then begin
        res.VarParam := True;

        // ######### DEBUG: TRACE:
        {$IFDEF _PS_TRACE_INVOK_}{$IFDEF _DCC_MSG_}
        {$if declared(PSParamTypeToStr)}
        S := 'INFO: rp:Result: '+PSParamTypeToStr(res);
        dbg(S);
        {$ifend}
        {$ENDIF}{$ENDIF}
        // ######### DEBUG: TRACE.
      end;

    end; // if Assigned(res)

    if not Assigned(res) then begin
      DoInvoke(Address,Args,SysCalConv,nil,False,IsConstr);  { ignore return }
    end else begin
      case res.aType.BaseType of { add result types here }
        {$if btCharIsWide}
        btString:              begin
                                 ResValue := DoInvoke(Address,Args,SysCalConv,
                                   TypeInfo(UnicodeString),False,IsConstr);
                                 {$IFDEF FPC}
                                 TbtString(res.Dta^) := TbtString( ResValue.AsUnicodeString );
                                 {$ELSE}
                                 TbtString(res.Dta^) := TbtString( ResValue.AsType<UnicodeString> );
                                 {$ENDIF}
                               end;
        {$else}
        btString:              begin
                                 ResValue := DoInvoke(Address,Args,SysCalConv,
                                   TypeInfo(AnsiString), False,IsConstr);
                                 {$IFDEF FPC}
                                 TbtString(res.Dta^) := TbtString( ResValue.AsAnsiString );
                                 {$ELSE}
                                 TbtString(res.Dta^) := TbtString( ResValue.AsType<AnsiString> );
                                 {$ENDIF}
                               end;
        {$ifend}
        {$IFNDEF PS_NOWIDESTRING}
        btUnicodeString:       begin
                                 ResValue := DoInvoke(Address,Args,SysCalConv,
                                   TypeInfo(UnicodeString),False,IsConstr);
                                 {$IFDEF FPC}
                                 {TbtUnicodeString}UnicodeString(res.Dta^) := ResValue.AsUnicodeString;
                                 {$ELSE}
                                 {TbtUnicodeString}UnicodeString(res.Dta^) := ResValue.AsType<UnicodeString>;
                                 {$ENDIF}
                               end;
        btWideString:          begin
                                 ResValue := DoInvoke(Address,Args,SysCalConv,
                                   TypeInfo(WideString),False,IsConstr);
                                 {$IFDEF FPC}
                                 {TbtWideString}WideString(res.Dta^) := ResValue.AsUnicodeString;
                                 {$ELSE}
                                 {TbtWideString}WideString(res.Dta^) := ResValue.AsType<WideString>;
                                 {$ENDIF}
                               end;
        {$ENDIF}
        btU8, btS8:            begin
                                 ResValue := DoInvoke(Address,Args,SysCalConv,
                                   TypeInfo(Byte),False,IsConstr);
                                 {$IFDEF FPC}
                                 PByte(res.Dta)^ := Byte(ResValue.AsOrdinal);
                                 {$ELSE}
                                 PByte(res.Dta)^ := ResValue.AsType<Byte>
                                 {$ENDIF}
                               end;
        btU16, btS16:          begin
                                 ResValue := DoInvoke(Address,Args,SysCalConv,
                                   TypeInfo(Word),False,IsConstr);
                                 {$IFDEF FPC}
                                 PWord(res.Dta)^ := Word(ResValue.AsOrdinal);
                                 {$ELSE}
                                 PWord(res.Dta)^ := ResValue.AsType<Word>;
                                 {$ENDIF}
                               end;
        btU32, btS32:          begin
                                 ResValue := DoInvoke(Address,Args,SysCalConv,
                                   TypeInfo(Cardinal),False,IsConstr);
                                 {$IFDEF FPC}
                                 PCardinal(res.Dta)^ := Cardinal(ResValue.AsOrdinal);
                                 {$ELSE}
                                 PCardinal(res.Dta)^ := ResValue.AsType<Cardinal>;
                                 {$ENDIF}
                               end;
        {$IFNDEF PS_NOINT64}
        btS64:                 begin
                                 PInt64(res.Dta)^ := DoInvoke(Address,Args,SysCalConv,
                                   TypeInfo(Int64),False,IsConstr).AsInt64;
                               end;
        {$ENDIF}
        btCurrency:            begin
                                 ResValue := DoInvoke(Address,Args,SysCalConv,
                                   TypeInfo(Currency),False,IsConstr);
                                 {$IFDEF FPC}
                                 PCurrency(res.Dta)^ := ResValue.AsCurrency;
                                 {$ELSE}
                                 PCurrency(res.Dta)^ := ResValue.AsType<Currency>;
                                 {$ENDIF}
                               end;
        btSingle:              begin
                                 ResValue := DoInvoke(Address,Args,SysCalConv,
                                   TypeInfo(Single),False,IsConstr);
                                 {$IFDEF FPC}
                                 PSingle(res.Dta)^ := ResValue.AsExtended;
                                 {$ELSE}
                                 PSingle(res.Dta)^ := ResValue.AsType<Single>;
                                 {$ENDIF}
                               end;
        btDouble:              begin
                                 ResValue := DoInvoke(Address,Args,SysCalConv,
                                   TypeInfo(Double),False,IsConstr);
                                 {$IFDEF FPC}
                                 PDouble(res.Dta)^ := ResValue.AsExtended;
                                 {$ELSE}
                                 PDouble(res.Dta)^ := ResValue.AsType<Double>;
                                 {$ENDIF}
                               end;
        btExtended:            begin
                                 PExtended(res.Dta)^ := DoInvoke(Address,Args,SysCalConv,
                                   TypeInfo(Extended),False,IsConstr).AsExtended;
                               end;
        {$if btCharIsWide}     // unicode
        btPChar:               begin
                                 ResValue := DoInvoke(Address,Args,SysCalConv,
                                   TypeInfo(PWideChar),False,IsConstr);
                                 {$IFDEF FPC}
                                 {PTbtChar}PWideChar(res.Dta^) := PWideChar(ResValue.AsOrdinal);
                                 {$ELSE}
                                 {PTbtChar}PWideChar(res.Dta^) := ResValue.AsType<PWideChar>;
                                 {$ENDIF}
                               end;
        {$else}                // ansi
        btPChar:               begin
                                 ResValue := DoInvoke(Address,Args,SysCalConv,
                                   TypeInfo(PAnsiChar),False,IsConstr);
                                 {$IFDEF FPC}
                                 {PTbtChar}PAnsiChar(res.Dta^) := PAnsiChar(ResValue.AsOrdinal);
                                 {$ELSE}
                                 {PTbtChar}PAnsiChar(res.Dta^) := ResValue.AsType<PAnsiChar>;
                                 {$ENDIF}
                               end;
        {$ifend}
        {$if btCharIsWide}     // unicode
        btChar:                begin
                                 ResValue := DoInvoke(Address,Args,SysCalConv,
                                   TypeInfo(WideChar),False,IsConstr);
                                 {$IFDEF FPC}
                                 {TbtChar}WideChar(res.Dta^) := ResValue.AsWideChar;
                                 {$ELSE}
                                 {TbtChar}WideChar(res.Dta^) := ResValue.AsType<WideChar>;
                                 {$ENDIF}
                               end;
        {$else}                // ansi
        btChar:                begin
                                 ResValue := DoInvoke(Address,Args,SysCalConv,
                                   TypeInfo(AnsiChar),False,IsConstr);
                                 {$IFDEF FPC}
                                 {TbtChar}AnsiChar(res.Dta^) := ResValue.AsAnsiChar;
                                 {$ELSE}
                                 {TbtChar}AnsiChar(res.Dta^) := ResValue.AsType<AnsiChar>;
                                 {$ENDIF}
                               end;
        {$ifend}
        {$IFNDEF PS_NOWIDESTRING}
        btWideChar:            begin
                                 ResValue := DoInvoke(Address,Args,SysCalConv,
                                   TypeInfo(WideChar),False,IsConstr);
                                 {$IFDEF FPC}
                                 {TbtWideChar}PWideChar(res.Dta)^ := ResValue.AsWideChar;
                                 {$ELSE}
                                 {TbtWideChar}PWideChar(res.Dta)^ := ResValue.AsType<WideChar>;
                                 {$ENDIF}
                               end;
          {$if declared(btPWideChar)}
        btPWideChar:           begin
                                 ResValue := DoInvoke(Address,Args,SysCalConv,
                                   TypeInfo(PWideChar),False,IsConstr);
                                 {$IFDEF FPC}
                                 {PTbtWideChar}PWideChar(res.Dta^) := PWideChar(ResValue.AsOrdinal);
                                 {$ELSE}
                                 {PTbtWideChar}PWideChar(res.Dta^) := ResValue.AsType<PWideChar>;
                                 {$ENDIF}
                               end;
          {$ifend}
        {$ENDIF}
        btInterface:           begin
                                 ResValue := DoInvoke(Address,Args,SysCalConv,
                                   TypeInfo(IInterface),True,IsConstr);
                                 {$IFDEF FPC}
                                 IInterface(res.Dta^) := ResValue.AsInterface;
                                 {$ELSE}
                                 IInterface(res.Dta^) := ResValue.AsType<IInterface>;
                                 {$ENDIF}
                             end;
      btSet:
        begin
          case TPSTypeRec_Set(res.aType).aByteSize of
            1: Byte(res.Dta^) := Byte(DoInvoke(Address,Args,SysCalConv,TypeInfo(Byte),False,IsConstr).AsInteger);
            2: Word(res.Dta^) := Word(DoInvoke(Address,Args,SysCalConv,TypeInfo(Word),False,IsConstr).AsInteger);
            3,
            4: Longint(res.Dta^) := Cardinal(DoInvoke(Address,Args,SysCalConv,TypeInfo(Cardinal),False,IsConstr).AsInteger);
            5,6,7,8:
              Int64(res.Dta^) := Cardinal(DoInvoke(Address,Args,SysCalConv,TypeInfo(Int64),False,IsConstr).AsInt64);
            else begin
              ResValue := DoInvoke(Address,Args,SysCalConv,TypeInfo(Pointer),False,IsConstr);
              {$IFDEF FPC}
              Pointer(res.Dta^) := Pointer(ResValue.AsOrdinal);
              {$ELSE}
              Pointer(res.Dta^) := ResValue.AsType<Pointer>;
              {$ENDIF}
            end;
          end; // case
        end;
      btClass:
        begin
          ResValue := DoInvoke(Address,Args,SysCalConv,TypeInfo(TObject),False,IsConstr);
          TObject(res.Dta^) := ResValue.AsObject;
        end;
        btVariant:
          begin
            ResValue := DoInvoke(Address,Args,SysCalConv,TypeInfo(TVarData),False,IsConstr);
            {$IFDEF FPC}
            ResValue.ExtractRawData(res.Dta);
            {$ELSE}
            TVarData(res.Dta^) := ResValue.AsType<TVarData>;
            {$ENDIF}
          end;
        btProcPtr:
        begin //??
          ResValue := DoInvoke(Address,Args,SysCalConv,TypeInfo(TMethod),False,IsConstr);
          {$IFDEF FPC}
          ResValue.ExtractRawData(res.Dta);
          {$ELSE}
          TMethod(res.Dta^) := ResValue.AsType<TMethod>;
          {$ENDIF}
        end;
        btStaticArray:
        begin
          ResValue := DoInvoke(Address,Args,SysCalConv,TypeInfo(Pointer),False,IsConstr,
          //{size:}TPSTypeRec_StaticArray(res.aType).Size);
            {size:}Cardinal(TPSTypeRec_StaticArray(res.aType).Size) * TPSTypeRec_StaticArray(res.aType).ArrayType.RealSize);
          if not CopyArrayContents(res.Dta, ResValue.GetReferenceToRawData,
            TPSTypeRec_StaticArray(res.aType).Size, TPSTypeRec_StaticArray(res.aType).ArrayType) then
          begin
		    raise EInvocationError.Create('Internal: ' + 'CopyArrayContents call for static array result failed!');
		  end;
        end;
        btRecord:
        begin
          ResValue := DoInvoke(Address,Args,SysCalConv,{TypeInfo:}nil,False,IsConstr,
            {size:}TPSTypeRec_Record(res.aType).RealSize);
          if not CopyArrayContents(res.Dta, ResValue.GetReferenceToRawData, 1, res.aType) then begin
		    raise EInvocationError.Create('Internal: ' + 'CopyArrayContents call for record result failed!');
		  end;
        end;
        btArray: //need to check with open arrays
        begin
          ResValue := DoInvoke(Address,Args,SysCalConv,TypeInfo(Pointer),False,IsConstr
            {size:},PSDynArrayGetLength(Pointer(res.Dta^), res.aType) );
          if Pointer(ResValue.AsOrdinal) = nil then
            res.Dta := nil
          else begin
            if not CopyArrayContents(res.Dta, Pointer(ResValue.AsOrdinal), 1, res.aType) then begin
      		    raise EInvocationError.Create('Internal: ' + 'CopyArrayContents call for dyn array result failed!');
			      end;
          end;
        end;
        else begin
          {$if declared(PSParamTypeToStr)}
          S := '$Param '+PSParamTypeToStr(res)
            + '" not implemented!';

          // ######### DEBUG: TRACE:
          {$IFDEF _PS_TRACE_INVOK_}{$IFDEF _DCC_MSG_}
          dbg(S);
          {$ENDIF}{$ENDIF}
          // ######### DEBUG: TRACE.

          raise EInvocationError.Create('Internal: ' + S); // optional
          {$ifend} // "$if declared(PSParamTypeToStr)"
          Exit;  // internal error
        end;
      end;  { case }
    end; // Assigned(res)
    Result := True;
  finally
    if Assigned(CallData) then begin
      for i := CallData.Count-1 downto 0 do begin
        pp := CallData[i];
        case pp^ of
          0: begin
            DestroyOpenArray(Self, Pointer(pp));
          end;
          254: begin
            FreeMem(pp);
          end;
        end; // case
      end; // for
      CallData.Free;
    end; // if
  end; // finally
end; // function TPSExec.InnerfuseCall

// optional:
(*
procedure PutOnFPUStackExtended(ft: extended);
begin
  ?
end; // procedure PutOnFPUStackExtended

function MyAllMethodsHandler2({RCX:}Self: PScriptMethodInfo; const {RDX:}Stack: PPointer;
  {R8:}_EDX, {R9:}_ECX: Pointer): Integer;
begin
  ?
end; // function MyAllMethodsHandler2

procedure MyAllMethodsHandler;
begin
  ?
end; // procedure MyAllMethodsHandler
//*)
