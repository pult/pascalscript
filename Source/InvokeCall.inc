{ InvokeCall.inc }

{+}
type
  EInvocationError = class(Exception);

{$IFNDEF DELPHI23UP}

//function ArrayOfConstToTValueArray(const Params: array of const): TArray<TValue>;
//begin
//  //TODO:...
//end;
//
//function TValueArrayToArrayOfConst(const Params: array of TValue): TArray<TVarRec>;
//begin
//  //TODO:...
//end;
//
//type
//  TArrayHelper = class
//  public
//    class function Concat<T>(const Args: array of TArray<T>): TArray<T>; static;
//  end;
//class function TArrayHelper.Concat<T>(
//  const Args: array of TArray<T>): TArray<T>;
//var
//  i, j, out, len: Integer;
//begin
//  len := 0;
//  for i := 0 to High(Args) do
//    len := len + Length(Args[i]);
//  SetLength(Result, len);
//  out := 0;
//  for i := 0 to High(Args) do
//    for j := 0 to High(Args[i]) do
//    begin
//      Result[out] := Args[i][j];
//      Inc(out);
//    end;
//end;

function Invoke(CodeAddress: Pointer; const Args: TArray<TValue>;
  CallingConvention: TCallConv; AResultType: PTypeInfo; IsStatic: Boolean = False;
  IsConstructor: Boolean = False ): TValue;
begin
  // TODO: XE3: implement IsConstructor
//XE3
//function Invoke(CodeAddress: Pointer; const Args: TArray<TValue>;
//  CallingConvention: TCallConv; AResultType: PTypeInfo; IsStatic: Boolean = False): TValue;
//#1
//  if UseResultPointer(AResultType) then
//  begin
//    TValue.Make(nil, AResultType, Result);
//    PutRefArg(Result.GetReferenceToRawData);
//  end;

  if IsConstructor then
  begin
    //if (CallingConvention = ccSafeCall) then
    //  IsConstructor := False;
    AResultType := nil;
  end;

//DX10,3
//function Invoke(CodeAddress: Pointer; const Args: TArray<TValue>;
//  CallingConvention: TCallConv; AResultType: PTypeInfo; IsStatic: Boolean = False;
//  IsConstructor: Boolean = False ): TValue;
//#1
//  if (CallingConvention <> ccSafeCall) and UseResultPointer(AResultType, IsConstructor, CallingConvention) then
//  begin
//    TValue.Make(nil, AResultType, Result);
//    PutRefArg(Result.GetReferenceToRawData);
//  end;

  Result := System.Rtti.Invoke(CodeAddress, Args, CallingConvention, AResultType, IsStatic);
end; // function Invoke
{$ENDIF}

{+.}
function TPSExec.InnerfuseCall(_Self, Address: Pointer; CallingConv: TPSCallingConvention; Params: TPSList; res: PPSVariantIFC): Boolean;
var SysCalConv : TCallConv;
    Args: TArray<TValue>;
    Arg : TValue;
    i : Integer;
    fvar: PPSVariantIFC;
    IsConstr : Boolean;
    ctx: TRTTIContext;
    RttiType : TRttiType;
    ResValue : TValue;
    {+}
    S: string;
    {+.}
begin
  Result := False;
  case CallingConv of
    cdRegister : SysCalConv := ccReg;
    cdPascal : SysCalConv := ccPascal;
    cdCdecl : SysCalConv := ccCdecl;
    cdStdCall : SysCalConv := ccStdCall;
    cdSafeCall : SysCalConv := ccSafeCall;
  else
    SysCalConv := ccReg;//to prevent warning "W1036 Variable might not have been initialized"
  end;

  if Assigned(_Self) then
  begin
    {+}
  //Args := Args + [TValue.From<Pointer>( _Self )];
    SetLength(Args, 1);
    Args[0] := TValue.From<Pointer>( _Self );
    {+.}
  end;

  for I := 0 to Params.Count - 1 do
  begin
    if Params[i] = nil
      then Exit;
    fvar := Params[i];

    if fvar.varparam then
    begin { var param }
      case fvar.aType.BaseType of
        btArray, btVariant, btSet, btStaticArray, btRecord, btInterface, btClass, {$IFNDEF PS_NOWIDESTRING} btWideString, btWideChar, {+}{$if declared(btPWideChar)}btPWideChar,{$ifend}{+.}{$ENDIF}
        btU8, btS8, btU16, btS16, btU32, btS32, btSingle, btDouble, btExtended, btString, btPChar, btChar, btCurrency,
        btUnicodeString
        {$IFNDEF PS_NOINT64}, bts64{$ENDIF}:
          Arg := TValue.From<Pointer>( Pointer(fvar.dta) );
        else
          begin
            Exit;
          end;
      end;
    end
    else
    begin  { not a var param }
      case fvar.aType.BaseType of
        { add normal params here }
        {$IFNDEF PS_NOWIDESTRING}
        {+}
        btWidestring:                      Arg := TValue.From(pwidestring(fvar.dta)^);
        btUnicodestring:                   Arg := TValue.From(punicodestring(fvar.dta)^);
        {+.}
        {$ENDIF}
        {+}
      //btString:                          Arg := TValue.From(pstring(fvar.dta)^);
        btString:                          Arg := TValue.From(pansistring(fvar.dta)^);
        {+.}
        btU8, btS8:                        Arg := TValue.From(pbyte(fvar.dta)^);
        btU16, BtS16:                      Arg := TValue.From(pword(fvar.dta)^);
        btU32, btS32:                      Arg := TValue.From(pCardinal(fvar.dta)^);
        {+}
        {$IFNDEF PS_NOINT64}
        bts64:                             Arg := TValue.From(pint64(fvar.dta)^);
        {$ENDIF}
        {+.}
        btSingle:                          Arg := TValue.From(PSingle(fvar.dta)^);
        btDouble:                          Arg := TValue.From(PDouble(fvar.dta)^);
        btExtended:                        Arg := TValue.From(PExtended(fvar.dta)^);
        {+}
      //btPChar:                           Arg := TValue.From(ppchar(fvar.dta)^);
        btPChar:                           Arg := TValue.From(ppansichar(fvar.dta)^);
        {+.}
        {+}
        {$IFNDEF PS_NOWIDESTRING}
        btWideChar:                        Arg := TValue.From(pwidechar(fvar.dta)^);
          {$if declared(btPWideChar)}
        btPWideChar:                       Arg := TValue.From(ppwidechar(fvar.dta)^);
          {$ifend}
        {$ENDIF !PS_NOWIDESTRING}
        {+.}
        {+}
      //btChar:                            Arg := TValue.From(pchar(fvar.dta)^);
        btChar:                            Arg := TValue.From(pansichar(fvar.dta)^);
        {+.}
        btClass:                           Arg := TValue.From(TObject(fvar.dta^));
        btRecord:                          Arg := TValue.From<Pointer>(fvar.dta);
        btStaticArray:                     Arg := TValue.From<Pointer>(fvar.dta);
        btArray:
          begin
             if Copy(fvar.aType.ExportName, 1, 10) = '!OPENARRAY' then
             begin  //openarray
               //in case of openarray we should provide TWO params: first is pointer to array,
               Args := Args + [TValue.From<Pointer>(Pointer(fvar.Dta^))]; // TODO: XE3: Error: E2001 Ordinal type required
               //2nd - integer with arraylength - 1 (high)
               Arg := TValue.From<Integer>(PSDynArrayGetLength(Pointer(fvar.Dta^), fvar.aType)-1);// = High of OpenArray
             end
             else //dynarray = just push pointer
               Arg := TValue.From<Pointer>(fvar.dta);
          end;
        btSet:
          begin
            case TPSTypeRec_Set(fvar.aType).aByteSize  of
              1: Arg := TValue.From(pbyte(fvar.dta)^);
              2: Arg := TValue.From(pWord(fvar.dta)^);
              3,
              4: Arg := TValue.From(pCardinal(fvar.dta)^);
              else
                Arg := TValue.From<Pointer>(fvar.dta);
            end;
          end;
        else
        {+}
        begin
          S := 'Parameter type "'
            + SysUtils.IntToStr(Integer(fvar.aType.BaseType))
            + '" not implemented!';
          {$IFDEF MSWINDOWS}
          OutputDebugString(PChar('rps:> ' + S));
          {$ELSE}
          //writeln(stderr, S);
          {$ENDIF}
          raise EInvocationError.Create('Internal: ' + S);
          //Exit;
        end;
        {+.}
      end;  { case }
    end;
    Args := Args + [Arg]; // TODO: XE3: Error: E2001 Ordinal type required
  end;

  IsConstr := (Integer(CallingConv) and 64) <> 0;
  if not assigned(res) then
  begin

//XE3
//function Invoke(CodeAddress: Pointer; const Args: TArray<TValue>;
//  CallingConvention: TCallConv; AResultType: PTypeInfo; IsStatic: Boolean = False): TValue;

//DX10,3
//function Invoke(CodeAddress: Pointer; const Args: TArray<TValue>;
//  CallingConvention: TCallConv; AResultType: PTypeInfo; IsStatic: Boolean = False;
//  IsConstructor: Boolean = False ): TValue;

    Invoke(Address,Args,SysCalConv,nil,False,IsConstr);  { ignore return }
  end
  else begin
    case res.atype.basetype of
      { add result types here }
      {+}
      btString:                tbtstring(res.dta^) := tbtstring(Invoke(Address,Args,SysCalConv,TypeInfo(String),False,IsConstr).AsString);
    //btString:                tbtstring(res.dta^) := tbtstring(Invoke(Address,Args,SysCalConv,TypeInfo(AnsiString),False,IsConstr).AsType<AnsiString>());
      {+.}
      {$IFNDEF PS_NOWIDESTRING}
      {+}
      btUnicodeString:         tbtunicodestring(res.dta^) := Invoke(Address,Args,SysCalConv,TypeInfo(String),False,IsConstr).AsString;
    //btUnicodeString:         tbtunicodestring(res.dta^) := Invoke(Address,Args,SysCalConv,TypeInfo(UnicodeString),False,IsConstr).AsType<UnicodeString>();
      btWideString:            tbtWideString(res.dta^) := Invoke(Address,Args,SysCalConv,TypeInfo(String),False,IsConstr).AsString;
    //btWideString:            tbtWideString(res.dta^) := Invoke(Address,Args,SysCalConv,TypeInfo(WideString),False,IsConstr).AsType<WideString>();
      {+.}
      {$ENDIF}
      btU8, btS8:              pbyte(res.dta)^ := Byte(Invoke(Address,Args,SysCalConv,TypeInfo(Byte),False,IsConstr).AsInteger);
      btU16, btS16:            pword(res.dta)^ := word(Invoke(Address,Args,SysCalConv,TypeInfo(Word),False,IsConstr).AsInteger);
      btU32, btS32:            pCardinal(res.dta)^ := Cardinal(Invoke(Address,Args,SysCalConv,TypeInfo(Cardinal),False,IsConstr).AsInteger);
      {+}
      {$IFNDEF PS_NOINT64}
      bts64:                   pInt64(res.dta)^ := Int64(Invoke(Address,Args,SysCalConv,TypeInfo(Int64),False,IsConstr).AsInt64);
      {$ENDIF}
      {+.}
      //TODO: XE3: Error: E2089 Invalid typecast
      btSingle:                psingle(res.dta)^ := Double(Invoke(Address,Args,SysCalConv,TypeInfo(Single),False,IsConstr).AsExtended);
      //TODO: XE3: Error: E2089 Invalid typecast
      btDouble:                pdouble(res.dta)^ := Double(Invoke(Address,Args,SysCalConv,TypeInfo(Double),False,IsConstr).AsExtended);
      btExtended:              pextended(res.dta)^ := Extended(Invoke(Address,Args,SysCalConv,TypeInfo(Extended),False,IsConstr).AsExtended);

      {+}
    //btPChar:                 ppchar(res.dta)^ := pchar(Invoke(Address,Args,SysCalConv,TypeInfo(PChar),False,IsConstr).AsType<PChar>());
      btPChar:                 ppansichar(res.dta)^ := pansichar(Invoke(Address,Args,SysCalConv,TypeInfo(PAnsiChar),False,IsConstr).AsType<PAnsiChar>());
      //btChar:                  pchar(res.dta)^ := Char(Invoke(Address,Args,SysCalConv,TypeInfo(Char),False,IsConstr).AsType<Char>());
      btChar:                  pansichar(res.dta)^ := AnsiChar(Invoke(Address,Args,SysCalConv,TypeInfo(AnsiChar),False,IsConstr).AsType<AnsiChar>());
      {+.}
      {+}
      {$IFNDEF PS_NOWIDESTRING}
      btWideChar:              pwidechar(res.dta)^ := WideChar(Invoke(Address,Args,SysCalConv,TypeInfo(WideChar),False,IsConstr).AsType<WideChar>());
        {$if declared(btPWideChar)}
      btPWideChar:             ppwidechar(res.dta)^ := pwidechar(Invoke(Address,Args,SysCalConv,TypeInfo(PWideChar),False,IsConstr).AsType<PWideChar>());
        {$ifend}
      {$ENDIF}
      {+.}
      btSet:
        begin
          case TPSTypeRec_Set(res.aType).aByteSize  of
            1: byte(res.Dta^) := Byte(Invoke(Address,Args,SysCalConv,TypeInfo(Byte),False,IsConstr).AsInteger);
            2: word(res.Dta^) := word(Invoke(Address,Args,SysCalConv,TypeInfo(Word),False,IsConstr).AsInteger);
            3,
            4: Longint(res.Dta^) := Cardinal(Invoke(Address,Args,SysCalConv,TypeInfo(Cardinal),False,IsConstr).AsInteger);
            else
            begin
              for RttiType in ctx.GetTypes do
                if (RttiType.Name.ToUpper.EndsWith(String(res.aType.FExportName))) and (RttiType.TypeKind = tkSet)
                  and (RttiType.TypeSize = TPSTypeRec_Set(res.aType).aByteSize) then
                begin
                  Invoke(Address,Args,SysCalConv,RttiType.Handle,False,IsConstr).ExtractRawData(res.dta);
                  Break;
                end;
            end;
          end;
        end;
      btClass:
      begin
        for RttiType in ctx.GetTypes do
          if (RttiType.Name.ToUpper.EndsWith(String(res.aType.FExportName))) and (RttiType.TypeKind = tkClass) then
          begin
            TObject(res.dta^) := Invoke(Address,Args,SysCalConv,RttiType.Handle,False,IsConstr).AsObject;
            Break;
          end;
      end;
      btStaticArray:
      begin
        for RttiType in ctx.GetTypes do
          if (RttiType.Name.ToUpper.EndsWith(String(res.aType.FExportName))) and (RttiType.TypeKind = tkArray) then
          begin
            CopyArrayContents(res.dta, Invoke(Address,Args,SysCalConv,RttiType.Handle,False,IsConstr).GetReferenceToRawData, TPSTypeRec_StaticArray(res.aType).Size, TPSTypeRec_StaticArray(res.aType).ArrayType);
            Break;
          end;
      end;
      btRecord:
      begin
        for RttiType in ctx.GetTypes do
          if (RttiType.Name.ToUpper.EndsWith(String(res.aType.FExportName))) and (RttiType.TypeKind = tkRecord) then
          begin
            CopyArrayContents(res.dta, (Invoke(Address,Args,SysCalConv,RttiType.Handle,False,IsConstr).GetReferenceToRawData), 1, res.aType);
            Break;
          end;
      end;
      btArray: //need to check with open arrays
      begin
        for RttiType in ctx.GetTypes do
          if (RttiType.Name.ToUpper.EndsWith(String(res.aType.FExportName))) and (RttiType.TypeKind = tkDynArray) then
          begin
            ResValue := Invoke(Address,Args,SysCalConv,RttiType.Handle,False,IsConstr);
            if ResValue.GetArrayLength > 0 then
              CopyArrayContents(res.dta, ResValue.GetReferenceToRawData, 1, res.aType)
            else
              res.dta := nil;
            Break;
          end;
      end;
      else
      {+}
      begin
        S := 'Result type "'
          + SysUtils.IntToStr(Integer(res.atype.basetype))
          + '" not implemented!';
        {$IFDEF MSWINDOWS}
        OutputDebugString(PChar('rps:> ' + S));
        {$ELSE}
        //writeln(stderr, S);
        {$ENDIF}
        raise EInvocationError.Create('Internal: ' + S);
        //Exit;
      end;
      {+.}
    end;  { case }
  end; //assigned(res)

  Result := True;
end; // function TPSExec.InnerfuseCall
