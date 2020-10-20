{ InvokeCall.inc } // version: 2020.1020.2105
{----------------------------------------------------------------------------}
{ RemObjects Pascal Script                                                   }
{----------------------------------------------------------------------------}

// TODO: not supported parameter types: interface, disp, varriant, ?

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

type
  EInvocationError = class(Exception);

function TPSExec.InnerfuseCall(_Self, Address: Pointer; CallingConv: TPSCallingConvention;
  Params: TPSList; res: PPSVariantIFC): Boolean;

// TODO: currentry not all parameter types supported: interface, disp, variant

type
  TMethodCallData = record
    AType: Byte;
    Data: TMethod;
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
  Result := False; //if Result then ;
  case CallingConv of
    cdRegister : SysCalConv := ccReg;
    cdPascal   : SysCalConv := ccPascal;
    cdCdecl    : SysCalConv := ccCdecl;
    cdStdCall  : SysCalConv := ccStdCall;
    cdSafeCall : SysCalConv := ccSafeCall;
    {$IFNDEF FPC}
    else         SysCalConv := ccReg; // to prevent warning "W1036 Variable might not have been initialized"
    {$ENDIF}
  end;

  if Assigned(_Self) then
  begin
    Arg := TValue.From<Pointer>( _Self );
    {$if defined(FPC) or (not defined(DELPHI23UP))}
    Args := {Args +} [Arg];
    {$else}
    SetLength({%H-}Args, 1);
    Args[0] := Arg;
    {$ifend}
  end;

  CallData := nil;
  try
    for I := 0 to Params.Count - 1 do begin
      fvar := Params[i];
      if fvar = nil then begin
        S := 'Unassigned parameter #"'
          + SysUtils.IntToStr(i)
          + '"!';
        {$IFDEF MSWINDOWS}
        OutputDebugString(PChar('rps:> ' + S));
        {$ELSE}
        //writeln(stderr, S);
        {$ENDIF}
        raise EInvocationError.Create('Internal: ' + S); // optional
        Exit;
      end;

      Arg := nil;
      if fvar.VarParam then
      begin { var param }
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
                Args := Args + [Arg];
                Arg := TValue.From<Longint>( Longint(POpenArray(p)^.ItemCount-1) ); //?? type IPointer

              end else begin
                Arg := TValue.From<Pointer>( Pointer(fvar.Dta) );
              end;
            end;
          btSet: begin // -- Arg := TValue.From<Pointer>( Pointer(fvar.Dta) );
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
            end;
          btVariant: begin
           //??Arg := TValue.From<Variant> (PVariant  (fvar.Dta));
              Arg := TValue.From<Pointer>( Pointer(fvar.Dta) );
            end;
          btInterface: begin
           //??Arg := TValue.From<IInterface> (IInterface  (fvar.Dta));
              Arg := TValue.From<Pointer>( Pointer(fvar.Dta) );
            end;
          {btArray,} {btVariant,} {btSet,} btStaticArray, btRecord, {btInterface,} btClass,
          {$IFNDEF PS_NOWIDESTRING}
          btWideString, btWideChar,

          {$if declared(btPWideChar)}
          btPWideChar,
          {$ifend}

          {$ENDIF !PS_NOWIDESTRING}
          btU8, btS8, btU16, btS16, btU32, btS32, btSingle, btDouble, btExtended, btString, btPChar, btChar, btCurrency,
          btUnicodeString
          {$IFNDEF PS_NOINT64}
          ,bts64
          {$ENDIF}:
            Arg := TValue.From<Pointer>( Pointer(fvar.Dta) );
          else
            begin
              S := 'Parameter type (var) "'
                + SysUtils.IntToStr(Integer(fvar.aType.BaseType))
                + ' EN: '+string(fvar.aType.ExportName)
                + ' SZ: '+string(UIntToStr(fvar.aType.RealSize))
                + '" not implemented!';
              {$IFDEF MSWINDOWS}
              OutputDebugString(PChar('rps:> ' + S));
              {$ELSE}
              //writeln(stderr, S);
              {$ENDIF}
              raise EInvocationError.Create('Internal: ' + S); // optional
              Exit;
            end;
        end;
      end else begin  { not a var param }
        case fvar.aType.BaseType of { add normal params here }
          {$IFNDEF PS_NOWIDESTRING}
          btWidestring:                      Arg := TValue.From<WideString>(PWideString(fvar.Dta)^);
          btUnicodeString:                   Arg := TValue.From<UnicodeString>(PUnicodeString(fvar.Dta)^);
          {$ENDIF}
          {$if btCharIsWide}                 // unicode string
          btString:                          Arg := TValue.From<string>(PUnicodeString(fvar.Dta)^);
          {$else}                            // ansi string
          btString:                          Arg := TValue.From<AnsiString>(PAnsiString(fvar.Dta)^);
          {$ifend}
          btU8, btS8:                        Arg := TValue.From<Byte>(PByte(fvar.Dta)^);
          btU16, BtS16:                      Arg := TValue.From<Word>(PWord(fvar.Dta)^);
          btU32, btS32:                      Arg := TValue.From<Cardinal>(PCardinal(fvar.Dta)^);
          {$IFNDEF PS_NOINT64}
          btS64:                             Arg := TValue.From<Int64>(PInt64(fvar.Dta)^);
          {$ENDIF}
          btSingle:                          Arg := TValue.From<Single>(PSingle(fvar.Dta)^);
          btDouble:                          Arg := TValue.From<Double>(PDouble(fvar.Dta)^);
          btExtended:                        Arg := TValue.From<Extended>(PExtended(fvar.Dta)^);
          btCurrency:                        Arg := TValue.From<Currency>(PCurrency(fvar.Dta)^);
          {$if btCharIsWide}                 // unicode
          btPChar:                           Arg := TValue.From<PWideChar>(PPWideChar(fvar.Dta)^);
          {$else}                            // ansi
          btPChar:                           Arg := TValue.From<PAnsiChar>(PPAnsiChar(fvar.Dta)^);
          {$ifend}
          {$IFNDEF PS_NOWIDESTRING}
          btWideChar:                        Arg := TValue.From<WideChar>(PWideChar(fvar.Dta)^);
            {$if declared(btPWideChar)}
          btPWideChar:                       Arg := TValue.From<PWideChar>(PPWideChar(fvar.Dta)^);
            {$ifend}
          {$ENDIF !PS_NOWIDESTRING}
          {$if btCharIsWide}                 // unicode char
          btChar:                            Arg := TValue.From<WideChar>(PWideChar(fvar.Dta)^);
          {$else}                            // ansi char
          btChar:                            Arg := TValue.From<AnsiChar>(PAnsiChar(fvar.Dta)^);
          {$ifend}
          btClass:                           Arg := TValue.From<TObject>(TObject(fvar.Dta^));
          btPointer:                         Arg := TValue.From<Pointer>(fvar.Dta);
       //?btResourcePointer:                 Arg := TValue.From<Pointer>(fvar.Dta);
          btProcPtr:
            begin //--Arg := TValue.From<Pointer>(fvar.Dta);
              p := New(PMethodCallData);
              TMethodCallData(p^).AType := 255;
              if CallData = nil then
                CallData := TPSList.Create;
              CallData.Add(p);
              // Uncomment when Handler will be implemented
              //TMethodCallData(p^).Data.Code := nil;
              //TMethodCallData(p^).Data.Data := nil;
              TMethodCallData(p^).Data := MKMethod(Self, Longint(fVar.Dta^)); // It requires the implementation of "Handled"
              Arg := TValue.From<Pointer>( PPointer(@TMethodCallData(p^).Data) );
            end;
          btInterface: // TODO: ...
            begin
              Arg := TValue.From<Pointer>(fvar.Dta);
            //??Arg := TValue.From<IInterface>( PIInterface(fvar.Dta) );
            //??Arg := TValue.From(Pointer(fvar.Dta^));
            //??Arg := TValue.From(IInterface(fvar.Dta^));
            end;
          btRecord:
            begin
              //Arg := TValue.From<Pointer>(fvar.Dta);
              if fVar.aType.RealSize <= SizeOf(Pointer) then
                Arg := TValue.From<IPointer>(IPointer(fvar.Dta^))
              else
                Arg := TValue.From<Pointer>( Pointer(fvar.Dta) );
            end;
          btStaticArray:                     Arg := TValue.From<Pointer>(fvar.Dta);
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

                 Args := Args + [Arg]; // TODO: XE3: Error: E2001 Ordinal type required
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
                 Args := Args + [Arg];
                 //2nd - integer with arraylength - 1 (high)
                 Arg := TValue.From<Longint>( Longint(POpenArray(p)^.ItemCount-1) ); //?? type IPointer
               end else begin //dynarray = just push pointer:
                 Arg := TValue.From<Pointer>( Pointer(fvar.Dta) );
               end;
            end;
          btVariant: // TODO: ...
            begin
              Arg := TValue.From<Pointer>(fvar.Dta);
            //?Arg := TValue.From<Variant>(Variant(fvar.Dta^));
            //?Arg := TValue.From<Variant>(PVariant(fvar.Dta)^);
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
            S := 'Parameter type (!var) "'
              + SysUtils.IntToStr(Integer(fvar.aType.BaseType))
              + ' EN: '+string(fvar.aType.ExportName)
              + ' SZ: '+string(UIntToStr(fvar.aType.RealSize))
              + '" not implemented!';
            {$IFDEF MSWINDOWS}
            OutputDebugString(PChar('rps:> ' + S));
            {$ELSE}
            //writeln(stderr, S);
            {$ENDIF}
            raise EInvocationError.Create('Internal: ' + S); // optional
            Exit;
          end;
        end;  { case }
      end; { if "not a var param" }
      Args := Args + [Arg]; // TODO: XE3: Error: E2001 Ordinal type required
    end; // for I

    IsConstr := (Integer(CallingConv) and 64) <> 0;
    if not Assigned(res) then begin
      Invoke(Address,Args,SysCalConv,nil,False,IsConstr); { ignore return }
    end else begin
      case res.aType.BaseType of { add result types here }
        {$if btCharIsWide}
        btString:              begin
                                 ResValue := Invoke(Address,Args,SysCalConv,
                                   TypeInfo(UnicodeString),False,IsConstr);
                                 {$IFDEF FPC}
                                 TbtString(res.Dta^) := TbtString( ResValue.AsUnicodeString );
                                 {$ELSE}
                                 TbtString(res.Dta^) := TbtString( ResValue.AsString );
                                 {$ENDIF}
                               end;
        {$else}
        btString:              begin
                                 ResValue := Invoke(Address,Args,SysCalConv,TypeInfo(AnsiString),
                                   False,IsConstr);
                                 {$IFDEF FPC}
                                 TbtString(res.Dta^) := TbtString( ResValue.AsAnsiString );
                                 {$ELSE}
                                 TbtString(res.Dta^) := TbtString( ResValue.AsType<AnsiString> );
                                 {$ENDIF}
                               end;
        {$ifend}
        {$IFNDEF PS_NOWIDESTRING}
        btUnicodeString:       begin
                                 ResValue := Invoke(Address,Args,SysCalConv,TypeInfo(String),False,IsConstr);
                                 {$IFDEF FPC}
                                 TbtUnicodeString(res.Dta^) := ResValue.AsUnicodeString;
                                 {$ELSE}
                                 TbtUnicodeString(res.Dta^) := ResValue.AsString;
                                 {$ENDIF}
                               end;
        btWideString:          begin
                                 ResValue := Invoke(Address,Args,SysCalConv,TypeInfo(String),False,IsConstr);
                                 {$IFDEF FPC}
                                 TbtWideString(res.Dta^) := ResValue.AsUnicodeString;
                                 {$ELSE}
                                 TbtWideString(res.Dta^) := ResValue.AsString;
                                 //TbtWideString(res.Dta^) := ResValue.AsType<WideString>();
                                 {$ENDIF}
                               end;
        {$ENDIF}
        btU8, btS8:            begin
                                 ResValue := Invoke(Address,Args,SysCalConv,TypeInfo(Byte),False,IsConstr);
                                 {$IFDEF FPC}
                                 PByte(res.Dta)^ := Byte(ResValue.AsOrdinal);
                                 {$ELSE}
                                 PByte(res.Dta)^ := ResValue.AsType<Byte>
                                 {$ENDIF}
                               end;
        btU16, btS16:          begin
                                 ResValue := Invoke(Address,Args,SysCalConv,TypeInfo(Word),False,IsConstr);
                                 {$IFDEF FPC}
                                 PWord(res.Dta)^ := Word(ResValue.AsOrdinal);
                                 {$ELSE}
                                 PWord(res.Dta)^ := ResValue.AsType<Word>;
                                 {$ENDIF}
                               end;
        btU32, btS32:          begin
                                 ResValue := Invoke(Address,Args,SysCalConv,TypeInfo(Cardinal),False,IsConstr);
                                 {$IFDEF FPC}
                                 PCardinal(res.Dta)^ := Cardinal(ResValue.AsOrdinal);
                                 {$ELSE}
                                 PCardinal(res.Dta)^ := ResValue.AsType<Cardinal>;
                                 {$ENDIF}
                               end;
        {$IFNDEF PS_NOINT64}
        btS64:                   PInt64(res.Dta)^ := Invoke(Address,Args,SysCalConv,TypeInfo(Int64),False,IsConstr).AsInt64;
        {$ENDIF}
        btCurrency:            begin
                                 ResValue := Invoke(Address,Args,SysCalConv,TypeInfo(Currency),False,IsConstr);
                                 {$IFDEF FPC}
                                 PCurrency(res.Dta)^ := ResValue.AsCurrency;
                                 {$ELSE}
                                 PCurrency(res.Dta)^ := ResValue.AsType<Currency>;
                                 {$ENDIF}
                               end;
        //TODO: XE3: Error: E2089 Invalid typecast
        btSingle:              begin
                                 ResValue := Invoke(Address,Args,SysCalConv,TypeInfo(Single),False,IsConstr);
                                 {$IFDEF FPC}
                                 PSingle(res.Dta)^ := ResValue.AsExtended;
                                 {$ELSE}
                                 PSingle(res.Dta)^ := ResValue.AsType<Single>;
                                 {$ENDIF}
                               end;
        //TODO: XE3: Error: E2089 Invalid typecast
        btDouble:              begin
                                 ResValue := Invoke(Address,Args,SysCalConv,TypeInfo(Double),False,IsConstr);
                                 {$IFDEF FPC}
                                 PDouble(res.Dta)^ := ResValue.AsExtended;
                                 {$ELSE}
                                 PDouble(res.Dta)^ := ResValue.AsType<Double>;
                                 {$ENDIF}
                               end;
        btExtended:              PExtended(res.Dta)^ := Invoke(Address,Args,SysCalConv,TypeInfo(Extended),False,IsConstr).AsExtended;
        {$if btCharIsWide}     // unicode
        btPChar:               begin
                                 {$IFDEF FPC}
                                 ResValue := Invoke(Address,Args,SysCalConv,TypeInfo(PWideChar),False,IsConstr);
                                 PPWideChar(res.Dta)^ := PWideChar(ResValue.AsOrdinal);
                                 {$ELSE}
                                 ResValue := Invoke(Address,Args,SysCalConv,TypeInfo(PChar),False,IsConstr);
                                 PPChar(res.Dta)^ := ResValue.AsType<PChar>;
                                 {$ENDIF}
                               end;
        {$else}                // ansi
        btPChar:               begin
                                 ResValue := Invoke(Address,Args,SysCalConv,TypeInfo(PAnsiChar),False,IsConstr);
                                 {$IFDEF FPC}
                                 PPAnsiChar(res.Dta)^ := PAnsiChar(ResValue.AsOrdinal);
                                 {$ELSE}
                                 PPAnsiChar(res.Dta)^ := ResValue.AsType<PAnsiChar>;
                                 {$ENDIF}
                               end;
        {$ifend}
        {$if btCharIsWide}     // unicode
        btChar:                begin
                                 {$IFDEF FPC}
                                 ResValue := Invoke(Address,Args,SysCalConv,TypeInfo(WideChar),False,IsConstr);
                                 PWideChar(res.Dta)^ := ResValue.AsWideChar;
                                 {$ELSE}
                                 ResValue := Invoke(Address,Args,SysCalConv,TypeInfo(Char),False,IsConstr);
                                 PChar(res.Dta)^ := ResValue.AsType<Char>;
                                 {$ENDIF}
                               end;
        {$else}                // ansi
        btChar:                begin
                                 ResValue := Invoke(Address,Args,SysCalConv,TypeInfo(AnsiChar),False,IsConstr);
                                 {$IFDEF FPC}
                                 PAnsiChar(res.Dta)^ := ResValue.AsAnsiChar;
                                 {$ELSE}
                                 PAnsiChar(res.Dta)^ := ResValue.AsType<AnsiChar>;
                                 {$ENDIF}
                               end;
        {$ifend}
        {$IFNDEF PS_NOWIDESTRING}
        btWideChar:            begin
                                 ResValue := Invoke(Address,Args,SysCalConv,TypeInfo(WideChar),False,IsConstr);
                                 {$IFDEF FPC}
                                 PWideChar(res.Dta)^ := ResValue.AsWideChar;
                                 {$ELSE}
                                 PWideChar(res.Dta)^ := ResValue.AsType<WideChar>;
                                 {$ENDIF}
                               end;
          {$if declared(btPWideChar)}
        btPWideChar:           begin
                                 ResValue := Invoke(Address,Args,SysCalConv,TypeInfo(PWideChar),False,IsConstr);
                                 {$IFDEF FPC}
                                 PPWideChar(res.Dta)^ := PWideChar(ResValue.AsOrdinal);
                                 {$ELSE}
                                 PPWideChar(res.Dta)^ := ResValue.AsType<PWideChar>;
                                 {$ENDIF}
                               end;
          {$ifend}
        {$ENDIF}
        btInterface:           begin  // TODO: ... ?empty ; check RefCount
                                 ResValue := Invoke(Address,Args,SysCalConv,TypeInfo(IInterface),True,IsConstr);
                                 {$IFDEF FPC}
                                 IInterface(res.Dta^) := ResValue.AsInterface;
                                 {$ELSE}
                                 IInterface(res.Dta^) := ResValue.AsType<IInterface>;
                                 {$ENDIF}
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
            ResValue := Invoke(Address,Args,SysCalConv,TypeInfo(Pointer),False,IsConstr);
            Variant(res.Dta^) := PVariant(ResValue.AsOrdinal)^;
          end;
        btStaticArray:
        begin
          {$IFNDEF FPC}
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
          {$IFNDEF FPC}
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
          {$IFNDEF FPC}
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
          S := 'Result type "'
            + SysUtils.IntToStr(Integer(res.aType.BaseType))
            + ' EN: '+string(res.aType.ExportName)
            + ' SZ: '+string(UIntToStr(res.aType.RealSize))
            + '" not implemented!';
          {$IFDEF MSWINDOWS}
          OutputDebugString(PChar('rps:> ' + S));
          {$ELSE}
          //writeln(stderr, S);
          {$ENDIF}
          raise EInvocationError.Create('Internal: ' + S); // optional
          Exit;
        end;
      end;  { case }
    end; // Assigned(res)
    Result := True;
  finally
    if Assigned(CallData) then begin
      for i := CallData.Count-1 downto 0 do
      begin
        pp := CallData[i];
        case pp^ of
          0: begin
            DestroyOpenArray(Self, Pointer(pp));
          end;
          255: begin // [#velter#]: https://github.com/velter/pascalscript/commit/1c598a1406390e2e762368b07a185ecb187182a7
            //FreeMem(Pointer(pp), sizeof(TMethodCallData)); // release TMethodCallData
            Dispose(PMethodCallData(pp));
          end;
        end;
      end;
      CallData.Free;
    end;
  end;
end; // function TPSExec.InnerfuseCall
