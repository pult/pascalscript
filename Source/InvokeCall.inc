{ InvokeCall.inc } // version: 2020.1031.1530
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
                                  + PSBaseTypeToStr(PIFTypeRecBase(TPSTypeRec_Record(Self).FieldTypes[0]));
                                for i := 1 to i-1 do
                                  Result := Result+','
                                    + PSBaseTypeToStr(PIFTypeRecBase(TPSTypeRec_Record(Self).FieldTypes[i]));
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
    else                    Result := 'Unknown '+SysUtils.IntToStr(FBaseType);
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

  //XE3
  //function Invoke(CodeAddress: Pointer; const Args: TArray<TValue>;
  //  CallingConvention: TCallConv; AResultType: PTypeInfo; IsStatic: Boolean = False): TValue;

  //DX10,3
  //function Invoke(CodeAddress: Pointer; const Args: TArray<TValue>;
  //  CallingConvention: TCallConv; AResultType: PTypeInfo; IsStatic: Boolean = False;
  //  IsConstructor: Boolean = False ): TValue;

  {$if defined(FPC) or defined(DELPHI23UP)}
  {$else}
  function Invoke(CodeAddress: Pointer; const Args: TArray<TValue>;
    CallingConvention: TCallConv; AResultType: PTypeInfo; IsStatic: Boolean{ = False};
    IsConstructor: Boolean{ = False} ): TValue; overload;
  begin
    //?? IsConstructor
    Result := System.Rtti.Invoke(CodeAddress,Args,CallingConvention,AResultType,IsStatic);
  end;
  {$ifend}

type
  TMethodCallData = record
    MType: Byte;
    MCallback: TMethod;
  end;
  PMethodCallData=^TMethodCallData;
var
  SysCalConv : TCallConv;
  Args: TArray<TValue>;
  Arg : TValue;
  i : Integer;
  fvar: PPSVariantIFC;
  IsConstr : Boolean;
  {%H-}ctx: TRTTIContext;
  {%H-}RttiType : TRttiType;
  ResValue : TValue;
  S: string;
//{%H-}poa: POpenArray; //??
  {%H-}ATypeInfo: Pointer;
  {%H-}p: Pointer;
  {%H-}CallData: TPSList;
  {%H-}pp: ^Byte;
begin
  // ######### DEBUG: TRACE:
  {$IFDEF _PS_TRACE_INVOK_}{$IFDEF _DCC_MSG_}
  dbg('#CALL: "TPSExec.InnerfuseCall": *( Params['+SysUtils.IntToStr(Params.Count)+'] ): res{'+BoolToTxt(Assigned(res))+'}');
  {$ENDIF}{$ENDIF}
  // ######### DEBUG: TRACE.
  //
  Result := False; //if Result then ;
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

  if Assigned(_Self) then
  begin
    Arg := TValue.From<Pointer>( _Self );
    {$if defined(FPC) or defined(DELPHI23UP)}
    Args := [Arg]; // XE3: Error: E2001 Ordinal type required
    {$else}
    SetLength({%H-}Args, 1);
    Args[0] := Arg;
    {$ifend}
  end;

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
                  Exit;
                if CallData = nil then
                  CallData := TPSList.Create;
                CallData.Add(p);

                Arg := TValue.From<Pointer>( Pointer(POpenArray(p)^.Data) );
                {$if defined(FPC) or defined(DELPHI23UP)}
                Args := Args + [Arg]; // XE3: Error: E2001 Ordinal type required
                {$else}
                SetLength(Args, Length(Args)+1);
                Args[High(Args)] := Arg;
                {$ifend}
                Arg := TValue.From<Longint>( Longint(POpenArray(p)^.ItemCount-1) ); //?? type IPointer

              end else begin
                Arg := TValue.From<Pointer>( Pointer(fvar.Dta) );
              end;
            end;
          (*
          btSet: begin
              Arg := TValue.From<Pointer>( Pointer(fvar.Dta) );
              //case TPSTypeRec_Set(fvar.aType).aByteSize  of
              //  1:       Arg := TValue.From<Byte>    (PByte    (fvar.Dta)^);
              //  2:       Arg := TValue.From<Word>    (PWord    (fvar.Dta)^);
              //  3,4:     Arg := TValue.From<Cardinal>(PCardinal(fvar.Dta)^);
              //  //{$if defined(CPU64)} // CPU64: // or: "{$if SizeOf(Pointer) = 8}"
              //  //5,6,7,8: Arg := TValue.From<Pointer>(PPointer  (fvar.Dta)^);
              //  //{$ifend}
              //  5,6,7,8: Arg := TValue.From<Int64>   (PInt64   (fvar.Dta)^);
              //  else     Arg := TValue.From<Pointer> (Pointer  (fvar.Dta));
              //end; // case
            end;
          btVariant: begin
           //??Arg := TValue.From<Variant> (PVariant  (fvar.Dta));
              Arg := TValue.From<Pointer>( Pointer(fvar.Dta) );
            end;
          btInterface: begin
           //??Arg := TValue.From<IInterface> (IInterface  (fvar.Dta));
              Arg := TValue.From<Pointer>( Pointer(fvar.Dta) );
            end;
          btStaticArray, btRecord, btClass,
          {$IFNDEF PS_NOWIDESTRING}
          btWideString, btWideChar,
          {$if declared(btPWideChar)}
          btPWideChar,
          {$ifend}
          {$ENDIF !PS_NOWIDESTRING}
          btU8, btS8, btU16, btS16, btU32, btS32, btSingle, btDouble, btExtended,
          btString, btPChar, btChar, btCurrency, btUnicodeString, btPointer
          //?, btResourcePointer
          {$IFNDEF PS_NOINT64}
          ,bts64
          {$ENDIF}:
            Arg := TValue.From<Pointer>( Pointer(fvar.Dta) );
          //*)

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
              S := '@Param: '+PSParamTypeToStr(fvar)
                + '" not implemented!';

              // ######### DEBUG: TRACE:
              {$IFDEF _PS_TRACE_INVOK_}{$IFDEF _DCC_MSG_}
              dbg(S);
              {$ENDIF}{$ENDIF}
              // ######### DEBUG: TRACE.

              raise EInvocationError.Create('Internal: ' + S); // optional
              {$ifend} // "$if declared(PSParamTypeToStr)"
              Exit;
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
          btPChar:                        //-Arg := TValue.From<PWideChar>( PPWideChar(fvar.Dta)^ );
                                           if Pointer(fVar.Dta^) = nil then
                                             Arg := TValue.From<PWideChar>( PPWideChar(@EmptyPChar)^ )
                                           else
                                             Arg := TValue.From<PWideChar>( PPWideChar(fvar.Dta)^ );
          {$else}                            // ansi
          btPChar:                        //-Arg := TValue.From<PAnsiChar>( PPAnsiChar(fvar.Dta)^ );
                                           if Pointer(fVar.Dta^) = nil then
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
          btProcPtr:
            begin //--Arg := TValue.From<Pointer>(fvar.Dta);

{$IFDEF _PS_TRACE_INVOK_}{$IFDEF _DCC_MSG_}           //# temporary:
dbg('# param['+SysUtils.IntToStr(i)+']: btProcPtr');  //# temporary:
{$ENDIF}{$ENDIF}                                      //# temporary:

//#TODO: Failed work on x86, but fine on x64

              Arg := TValue.From<TMethod>( MKMethod(Self, Longint(fVar.Dta^)) ); // +x64, -x86

(*
              //
              // like "x86.inc":
              //
              p := New(PMethodCallData);
              TMethodCallData(p^).MType := 255;
              if CallData = nil then
                CallData := TPSList.Create;
              CallData.Add(p);
              //{$ifndef EMPTY_METHODS_HANDLER}
              TMethodCallData(p^).MCallback := MKMethod(Self, Longint(fVar.Dta^));
              //{$else}
              //TMethodCallData(p^).MCallback.Code := nil;
              //TMethodCallData(p^).MCallback.Data := nil;
              //{$endif}
              Arg := TValue.From<Pointer>( Pointer(@(TMethodCallData(p^).MCallback)) ); // +x64, -x86
              //
              //.
//*)

              // ######### DEBUG: TRACE:
(*
              {$IFDEF _PS_TRACE_INVOK_}{$IFDEF _DCC_MSG_}
              S := ' # Ptr: Self:' + SysUtils.IntToStr(IPointer(Self))
                 + '; ProcNo:' + SysUtils.IntToStr(Longint(fVar.Dta^))
                 ;
              dbg(S);

              S := ' # CB: Code:' + SysUtils.IntToStr(IPointer(TMethodCallData(p^).MCallback.Code)) // == @MyAllMethodsHandler
                 + '; @MyAllMethodsHandler:' + SysUtils.IntToStr(IPointer(MyAllMethodsHandlerPtr))
                 + '; Data:' + SysUtils.IntToStr(IPointer(TMethodCallData(p^).MCallback.Data))
                 + '; SE:' + SysUtils.IntToStr(IPointer( PScriptMethodInfo(TMethodCallData(p^).MCallback.Data).SE )) // == Self
                 + '; ProcNo:' + SysUtils.IntToStr( PScriptMethodInfo(TMethodCallData(p^).MCallback.Data).ProcNo )
                 ;
              dbg(S);
              {$ENDIF}{$ENDIF}
//*)
              // ######### DEBUG: TRACE.

            end;
          btInterface:
            begin
              Arg := TValue.From<Pointer>( PPointer(fvar.Dta)^ );
            //??Arg := TValue.From<Pointer>( PPointer(fvar.Dta)^ );
            //??Arg := TValue.From<IInterface>( PIInterface(fvar.Dta)^ );
            //??Arg := TValue.From<IInterface>( PIInterface(fvar.Dta) );
            //??Arg := TValue.From(Pointer(fvar.Dta^));
            //??Arg := TValue.From(IInterface(fvar.Dta^));
            end;
          btRecord:
            begin
              //Arg := TValue.From<Pointer>(fvar.Dta);
              if fVar.aType.RealSize <= SizeOf(Pointer) then
                Arg := TValue.From<IPointer>( IPointer(fvar.Dta^) )
              else
                Arg := TValue.From<Pointer>( Pointer(fvar.Dta) );
            end;
          btStaticArray:                     Arg := TValue.From<Pointer>( Pointer(fvar.Dta) );
                                           //Arg := TValue.From<Pointer>(fvar.Dta);
                                           //Arg := TValue.From(Pointer(fvar.Dta));
          btArray:
            begin
               if Copy(fvar.aType.ExportName, 1, 10) = '!OPENARRAY' then begin
                 (*
                 //in case of openarray we should provide TWO params: first is pointer to array,
                 Arg := TValue.From<Pointer>( Pointer(fvar.Dta^) );

             //--Arg := TValue.From<Pointer>( Pointer(fvar.Dta) );

             //?? poa := CreateOpenArray(False, Self, fVar);
             //   Arg := TValue.From<Pointer>( Pointer( poa ) ); //??

                 {$if defined(FPC) or defined(DELPHI23UP)}
                 Args := Args + [Arg];
                 {$else}
                 SetLength(Args, Length(Args)+1);
                 Args[High(Args)] := Arg;
                 {$ifend}

                 //2nd - integer with arraylength - 1 (high)
                 Arg := TValue.From<Integer>(PSDynArrayGetLength(Pointer(fvar.Dta^), fvar.aType)-1);// = High of OpenArray
                 //*)
                 p := CreateOpenArray(False, Self, fVar);
                 if p = nil then
                   Exit;
                 if CallData = nil then
                   CallData := TPSList.Create;
                 CallData.Add(p);

                 //in case of openarray we should provide TWO params: first is pointer to array,
                 Arg := TValue.From<Pointer>( Pointer(POpenArray(p)^.Data) );
                 {$if defined(FPC) or defined(DELPHI23UP)}
                 Args := Args + [Arg];
                 {$else}
                 SetLength(Args, Length(Args)+1);
                 Args[High(Args)] := Arg;
                 {$ifend}

                 //2nd - integer with arraylength - 1 (high)
                 Arg := TValue.From<Longint>( Longint(POpenArray(p)^.ItemCount-1) ); //?? type IPointer
               end else begin //dynarray = just push pointer:
                 Arg := TValue.From<Pointer>( PPointer(fvar.Dta){$IFNDEF FPC}^{$ENDIF} );
               end;
            end;
          btVariant:
            begin
              Arg := TValue.From<Pointer>( Pointer(fvar.Dta) );
            //?Arg := TValue.From<Pointer>( PPointer(fvar.Dta)^ );
            //?Arg := TValue.From<Variant>( Variant(fvar.Dta^) );
            //?Arg := TValue.From<Variant>( PVariant(fvar.Dta)^ );
            end;
          btSet:
            case TPSTypeRec_Set(fvar.aType).aByteSize  of
              1:       Arg := TValue.From<Byte>    (PByte    (fvar.Dta)^);
              2:       Arg := TValue.From<Word>    (PWord    (fvar.Dta)^);
              3,4:     Arg := TValue.From<Cardinal>(PCardinal(fvar.Dta)^);
              //{$if defined(CPU64)} // CPU64: // or: "{$if SizeOf(Pointer) = 8}"
              //5,6,7,8: Arg := TValue.From<Pointer>(PPointer  (fvar.Dta)^);
              //{$ifend}
              5,6,7,8: Arg := TValue.From<Int64>   (PInt64   (fvar.Dta)^);
              else     Arg := TValue.From<Pointer> (Pointer  (fvar.Dta));
            end; // case
          else begin
            {$if declared(PSParamTypeToStr)}
            S := '#Param '+PSParamTypeToStr(fvar)
              + '" not implemented!';

            // ######### DEBUG: TRACE:
            {$IFDEF _PS_TRACE_INVOK_}{$IFDEF _DCC_MSG_}
            dbg(S);
            {$ENDIF}{$ENDIF}
            // ######### DEBUG: TRACE.

            raise EInvocationError.Create('Internal: ' + S); // optional
            {$ifend} // "$if declared(PSParamTypeToStr)"
            Exit;
          end;
        end;  { case }
      end; { if "not a var param" }
      if Arg.IsEmpty then begin
        {$if declared(PSParamTypeToStr)}
        S := '#Param '+PSParamTypeToStr(fvar)
          + '" not detected!';

        // ######### DEBUG: TRACE:
        {$IFDEF _PS_TRACE_INVOK_}{$IFDEF _DCC_MSG_}
        dbg(S);
        {$ENDIF}{$ENDIF}
        // ######### DEBUG: TRACE.

        //raise EInvocationError.Create('Internal: ' + S); // optional
        {$ifend} // "$if declared(PSParamTypeToStr)"
        //Exit;
      end;
      {$if defined(FPC) or defined(DELPHI23UP)}
      Args := Args + [Arg];
      {$else}
      SetLength(Args, Length(Args)+1);
      Args[High(Args)] := Arg;
      {$ifend}
    end; // for I

    IsConstr := (Integer(CallingConv) and 64) <> 0; // TODO: FPC rtti currently not supported IsConstr

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
      Invoke(Address,Args,SysCalConv,nil,False,IsConstr);  { ignore return }
    end else begin
      case res.aType.BaseType of { add result types here }
        {$if btCharIsWide}
        btString:              begin
                                 ResValue := Invoke(Address,Args,SysCalConv,
                                   TypeInfo(UnicodeString),False,IsConstr);
                                 {$IFDEF FPC}
                                 TbtString(res.Dta^) := TbtString( ResValue.AsUnicodeString );
                                 {$ELSE}
                                 TbtString(res.Dta^) := TbtString( ResValue.AsType<UnicodeString> );
                                 {$ENDIF}
                               end;
        {$else}
        btString:              begin
                                 ResValue := Invoke(Address,Args,SysCalConv,
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
                                 ResValue := Invoke(Address,Args,SysCalConv,
                                   TypeInfo(UnicodeString),False,IsConstr);
                                 {$IFDEF FPC}
                                 {TbtUnicodeString}UnicodeString(res.Dta^) := ResValue.AsUnicodeString;
                                 {$ELSE}
                                 {TbtUnicodeString}UnicodeString(res.Dta^) := ResValue.AsType<UnicodeString>;
                                 {$ENDIF}
                               end;
        btWideString:          begin
                                 ResValue := Invoke(Address,Args,SysCalConv,
                                   TypeInfo(WideString),False,IsConstr);
                                 {$IFDEF FPC}
                                 {TbtWideString}WideString(res.Dta^) := ResValue.AsUnicodeString;
                                 {$ELSE}
                                 {TbtWideString}WideString(res.Dta^) := ResValue.AsType<WideString>;
                                 {$ENDIF}
                               end;
        {$ENDIF}
        btU8, btS8:            begin
                                 ResValue := Invoke(Address,Args,SysCalConv,
                                   TypeInfo(Byte),False,IsConstr);
                                 {$IFDEF FPC}
                                 PByte(res.Dta)^ := Byte(ResValue.AsOrdinal);
                                 {$ELSE}
                                 PByte(res.Dta)^ := ResValue.AsType<Byte>
                                 {$ENDIF}
                               end;
        btU16, btS16:          begin
                                 ResValue := Invoke(Address,Args,SysCalConv,
                                   TypeInfo(Word),False,IsConstr);
                                 {$IFDEF FPC}
                                 PWord(res.Dta)^ := Word(ResValue.AsOrdinal);
                                 {$ELSE}
                                 PWord(res.Dta)^ := ResValue.AsType<Word>;
                                 {$ENDIF}
                               end;
        btU32, btS32:          begin
                                 ResValue := Invoke(Address,Args,SysCalConv,
                                   TypeInfo(Cardinal),False,IsConstr);
                                 {$IFDEF FPC}
                                 PCardinal(res.Dta)^ := Cardinal(ResValue.AsOrdinal);
                                 {$ELSE}
                                 PCardinal(res.Dta)^ := ResValue.AsType<Cardinal>;
                                 {$ENDIF}
                               end;
        {$IFNDEF PS_NOINT64}
        btS64:                 begin
                                 PInt64(res.Dta)^ := Invoke(Address,Args,SysCalConv,
                                   TypeInfo(Int64),False,IsConstr).AsInt64;
                               end;
        {$ENDIF}
        btCurrency:            begin
                                 ResValue := Invoke(Address,Args,SysCalConv,
                                   TypeInfo(Currency),False,IsConstr);
                                 {$IFDEF FPC}
                                 PCurrency(res.Dta)^ := ResValue.AsCurrency;
                                 {$ELSE}
                                 PCurrency(res.Dta)^ := ResValue.AsType<Currency>;
                                 {$ENDIF}
                               end;
        //TODO: XE3: Error: E2089 Invalid typecast
        btSingle:              begin
                                 ResValue := Invoke(Address,Args,SysCalConv,
                                   TypeInfo(Single),False,IsConstr);
                                 {$IFDEF FPC}
                                 PSingle(res.Dta)^ := ResValue.AsExtended;
                                 {$ELSE}
                                 PSingle(res.Dta)^ := ResValue.AsType<Single>;
                                 {$ENDIF}
                               end;
        //TODO: XE3: Error: E2089 Invalid typecast
        btDouble:              begin
                                 ResValue := Invoke(Address,Args,SysCalConv,
                                   TypeInfo(Double),False,IsConstr);
                                 {$IFDEF FPC}
                                 PDouble(res.Dta)^ := ResValue.AsExtended;
                                 {$ELSE}
                                 PDouble(res.Dta)^ := ResValue.AsType<Double>;
                                 {$ENDIF}
                               end;
        btExtended:            begin
                                 PExtended(res.Dta)^ := Invoke(Address,Args,SysCalConv,
                                   TypeInfo(Extended),False,IsConstr).AsExtended;
                               end;
        {$if btCharIsWide}     // unicode
        btPChar:               begin
                                 ResValue := Invoke(Address,Args,SysCalConv,
                                   TypeInfo(PWideChar),False,IsConstr);
                                 {$IFDEF FPC}
                                 {PTbtChar}PWideChar(res.Dta^) := PWideChar(ResValue.AsOrdinal);
                                 {$ELSE}
                                 {PTbtChar}PWideChar(res.Dta^) := ResValue.AsType<PWideChar>;
                                 {$ENDIF}
                               end;
        {$else}                // ansi
        btPChar:               begin
                                 ResValue := Invoke(Address,Args,SysCalConv,
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
                                 ResValue := Invoke(Address,Args,SysCalConv,
                                   TypeInfo(WideChar),False,IsConstr);
                                 {$IFDEF FPC}
                                 {TbtChar}WideChar(res.Dta^) := ResValue.AsWideChar;
                                 {$ELSE}
                                 {TbtChar}WideChar(res.Dta^) := ResValue.AsType<WideChar>;
                                 {$ENDIF}
                               end;
        {$else}                // ansi
        btChar:                begin
                                 ResValue := Invoke(Address,Args,SysCalConv,
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
                                 ResValue := Invoke(Address,Args,SysCalConv,
                                   TypeInfo(WideChar),False,IsConstr);
                                 {$IFDEF FPC}
                                 {TbtWideChar}PWideChar(res.Dta)^ := ResValue.AsWideChar;
                                 {$ELSE}
                                 {TbtWideChar}PWideChar(res.Dta)^ := ResValue.AsType<WideChar>;
                                 {$ENDIF}
                               end;
          {$if declared(btPWideChar)}
        btPWideChar:           begin
                                 ResValue := Invoke(Address,Args,SysCalConv,
                                   TypeInfo(PWideChar),False,IsConstr);
                                 {$IFDEF FPC}
                                 {PTbtWideChar}PWideChar(res.Dta^) := PWideChar(ResValue.AsOrdinal);
                                 {$ELSE}
                                 {PTbtWideChar}PWideChar(res.Dta^) := ResValue.AsType<PWideChar>;
                                 {$ENDIF}
                               end;
          {$ifend}
        {$ENDIF}
        btInterface:           begin  // TODO: ... ?empty ; check RefCount
                                 //(* #0:
                                 ResValue := Invoke(Address,Args,SysCalConv,
                                   TypeInfo(IInterface),True,IsConstr);
                                 {$IFDEF FPC}
                                 IInterface(res.Dta^) := ResValue.AsInterface;
                                 {$ELSE}
                                 IInterface(res.Dta^) := ResValue.AsType<IInterface>;
                                 {$ENDIF}
                                 // #0. *)
                                 (* #1:
                                 ResValue := Invoke(Address,Args,SysCalConv,
                                   TypeInfo(Pointer),True,IsConstr);
                                 {$IFDEF FPC}
                                 Pointer(res.Dta^) := Pointer(ResValue.AsOrdinal);
                                 {$ELSE}
                                 Pointer(res.Dta^) := Pointer(ResValue.AsType<Pointer>);
                                 {$ENDIF}
                                 // #1. *)
                             end;
      btSet:
        begin
          case TPSTypeRec_Set(res.aType).aByteSize of
            1: Byte(res.Dta^) := Byte(Invoke(Address,Args,SysCalConv,TypeInfo(Byte),False,IsConstr).AsInteger);
            2: Word(res.Dta^) := Word(Invoke(Address,Args,SysCalConv,TypeInfo(Word),False,IsConstr).AsInteger);
            3,
            4: Longint(res.Dta^) := Cardinal(Invoke(Address,Args,SysCalConv,TypeInfo(Cardinal),False,IsConstr).AsInteger);
            //{$if defined(CPU64)} // CPU64: // or: "{$if SizeOf(Pointer) = 8}"
            //5,6,7,8: Arg := TValue.From<Pointer>(PPointer  (fvar.Dta)^);
            //  Pointer(res.Dta^) := Pointer(Invoke(Address,Args,SysCalConv,TypeInfo(Pointer),False,IsConstr).GetReferenceToRawData^);
            //{$ifend}
            5,6,7,8:
              Int64(res.Dta^) := Cardinal(Invoke(Address,Args,SysCalConv,TypeInfo(Int64),False,IsConstr).AsInt64);
            {$IFNDEF FPC}
            else begin
              for RttiType in ctx.GetTypes do begin // FPC: Failed method TRttiType.GetTypes
                if (RttiType.Name.ToUpper.EndsWith(String(res.aType.FExportName))) and (RttiType.TypeKind = tkSet)
                  and (RttiType.TypeSize = TPSTypeRec_Set(res.aType).aByteSize) then
                begin
                  ResValue := Invoke(Address,Args,SysCalConv,{TypeInfo:}RttiType.Handle,False,IsConstr);
                  ResValue.ExtractRawData(res.Dta);
                  Break;
                end; // if
              end; // for
            end;
            {$ENDIF !FPC}
          end; // case
        end;
      btClass:
        begin
          {$IFNDEF FPC}
          for RttiType in ctx.GetTypes do
            if (RttiType.Name.ToUpper.EndsWith(string(res.aType.FExportName))) and (RttiType.TypeKind = tkClass) then
          {$ENDIF !FPC}
            begin
              ATypeInfo := {$IFDEF FPC}TypeInfo(TObject){$ELSE}RttiType.Handle{$ENDIF};
              ResValue := Invoke(Address,Args,SysCalConv,ATypeInfo,False,IsConstr);
              TObject(res.Dta^) := ResValue.AsObject;
              {$IFNDEF FPC}Break;{$ENDIF}
            end;
        end;
        btVariant: // TODO: ... ? empty ; check RefCount
          begin
            //OK:
            ResValue := Invoke(Address,Args,SysCalConv,TypeInfo(TVarData),False,IsConstr);
            TVarData(res.Dta^) := ResValue.AsType<TVarData>;

            //OK:
            //ResValue := Invoke(Address,Args,SysCalConv,TypeInfo(Variant),False,IsConstr);
            //Variant(res.Dta^) := ResValue.AsVariant;
          end;
        btStaticArray:
        begin
          {$IFDEF FPC}
          ResValue := Invoke(Address,Args,SysCalConv,TypeInfo(Pointer),False,IsConstr);
          PPointer(res.Dta)^ := ResValue.AsOrdinal;
          {$ELSE !FPC}
            {$IFNDEF FPC}
            for RttiType in ctx.GetTypes do
              if (RttiType.Name.ToUpper.EndsWith(string(res.aType.FExportName))) and (RttiType.TypeKind = tkArray) then
            {$ENDIF !FPC}
              begin
                ATypeInfo := {$IFDEF FPC}TypeInfo(??){$ELSE}RttiType.Handle{$ENDIF};
                ResValue := Invoke(Address,Args,SysCalConv,ATypeInfo,False,IsConstr);
                // function CopyArrayContents(Dest, Src: Pointer; Len: Longint; aType: TPSTypeRec): Boolean;
                CopyArrayContents(res.Dta, ResValue.GetReferenceToRawData,
                  TPSTypeRec_StaticArray(res.aType).Size, TPSTypeRec_StaticArray(res.aType).ArrayType);
                {$IFNDEF FPC}Break;{$ENDIF}
              end;
          {$ENDIF !FPC}
        end;
        btRecord:
        begin
          {$IFDEF FPC}
          ResValue := Invoke(Address,Args,SysCalConv,TypeInfo(Pointer),False,IsConstr);
          PPointer(res.Dta)^ := ResValue.AsOrdinal;
          {$ELSE !FPC}
            {$IFNDEF FPC}
            for RttiType in ctx.GetTypes do
              if (RttiType.Name.ToUpper.EndsWith(string(res.aType.FExportName))) and (RttiType.TypeKind = tkRecord) then
            {$ENDIF !FPC}
              begin
                ATypeInfo := {$IFDEF FPC}TypeInfo(??){$ELSE}RttiType.Handle{$ENDIF};
                ResValue := Invoke(Address,Args,SysCalConv,ATypeInfo,False,IsConstr);
                CopyArrayContents(res.Dta, (ResValue.GetReferenceToRawData), 1, res.aType);
                {$IFNDEF FPC}Break;{$ENDIF}
              end;
          {$ENDIF !FPC}
        end;
        btArray: //need to check with open arrays
        begin
          {$IFDEF FPC}
          ResValue := Invoke(Address,Args,SysCalConv,TypeInfo(Pointer),False,IsConstr);
          PPointer(res.Dta)^ := ResValue.AsOrdinal;
          {$ELSE !FPC}
            //NB. When you have types-synonyms like TCardinalDynArray = array of Cardinal;
            //it will NOT be added to RTTI list by compiler. So, it will be never
            //finded by this way. The only way is to declares types like:
            //TCardinalDynArray_ = array of Cardinal;
            //TCardinalDynArray = type TCardinalDynArray_
            {$IFNDEF FPC}
            for RttiType in ctx.GetTypes do
              if (RttiType.Name.ToUpper.EndsWith(string(res.aType.FExportName))) and (RttiType.TypeKind = tkDynArray) then
            {$ENDIF !FPC}
              begin
                ATypeInfo := {$IFDEF FPC}TypeInfo(??){$ELSE}RttiType.Handle{$ENDIF};
                ResValue := Invoke(Address,Args,SysCalConv,ATypeInfo,False,IsConstr);
                if ResValue.GetArrayLength > 0 then
                  CopyArrayContents(res.Dta, ResValue.GetReferenceToRawData, 1, res.aType)
                else
                  res.Dta := nil;
                {$IFNDEF FPC}Break;{$ENDIF}
              end;
          {$ENDIF !FPC}
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
          Exit;
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
          255: begin
            Dispose(PMethodCallData(pp));
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
