{ uPSDisassembly.pas } // version: 2020.1030.1825
{----------------------------------------------------------------------------}
{ RemObjects Pascal Script                                                   }
{----------------------------------------------------------------------------}
unit uPSDisassembly;
{$I PascalScript.inc}

interface

uses
  {$IFDEF DELPHI7UP}Types,{$ENDIF}
  //{$IFDEF DELPHI17UP}System.UITypes,{$ENDIF}
  uPSRuntime, uPSUtils, SysUtils;

function IFPS3DataToText(const Input: TbtString; var Output: string): Boolean;

implementation

{+}
{$IFDEF DELPHI12UP}
uses
  AnsiStrings;
{$ENDIF}
{+.}

type
  TMyPSExec = class(TPSExec)
    function ImportProc(const {%H-}Name: ShortString; proc: TIFExternalProcRec): Boolean; override;
  end;

function Debug2Str(const s: string): string;
var i: Integer;
begin
  Result := '';
  for i := 1 to length(s) do begin
    if (s[i] < #32) or (s[i] > #128) then
      Result := Result + '\'+inttohex(ord(s[i]), 2)
    else if s[i] = '\' then
      Result := Result + '\\'
    else
      Result := Result + s[i];
  end;

end;

function SpecImportProc(Sender: TObject; p: TIFExternalProcRec): Boolean; forward;

function FloatToStr(Value: Extended): string;
begin
  try
    Result := SysUtils.FloatToStr(Value);
  except
    Result := 'NaNa';
  end;
end;

function IFPS3DataToText(const Input: TbtString; var Output: string): Boolean;
var I: TMyPSExec;

  procedure Writeln(const S: string);
  begin
    Output := Output + S + #13#10;
  end;

  {$if declared(PSBaseTypeToStr)}
  function BaseTypeToStr(P: PIFTypeRec): string; {$ifdef _inline_}inline;{$endif}
  begin
    Result := uPSUtils.PSBaseTypeToStr( PIFTypeRecBase(P) );
  end;
  {$else}
  function BaseTypeToStr(P: PIFTypeRec): string;
  var i: Longint;
  begin
    case p.BaseType of
      btU8: Result := 'U8';
      btS8: Result := 'S8';
      btU16: Result := 'U16';
      btS16: Result := 'S16';
      btU32: Result := 'U32';
      btS32: Result := 'S32';
      {$IFNDEF PS_NOINT64}
      bts64: Result := 'S64';
      {$ENDIF}
      btChar: Result := {$IFDEF PS_PANSICHAR}'AnsiChar'{$ELSE}'Char'{$ENDIF};
      {$IFNDEF PS_NOWIDESTRING}
      btWideChar: Result := 'WideChar';
      btWideString: Result := 'WideString';
      {$ENDIF}
      btSet: Result := 'Set';
      btSingle: Result := 'Single';
      btDouble: Result := 'Double';
      btExtended: Result := 'Extended';
      btString: Result := 'String';
      btRecord: begin
          Result := 'Record(';
          for i := 0 to TPSTypeRec_Record(p).FieldTypes.Count-1 do begin
            if i <> 0 then Result := Result+',';
            Result := Result + BaseTypeToStr(PIFTypeRec(TPSTypeRec_Record(p).FieldTypes[i]));
          end;
          Result := Result + ')';
        end;
      btArray: Result := 'Array of '+BaseTypeToStr(TPSTypeRec_Array(p).ArrayType);
      btResourcePointer: Result := 'ResourcePointer';
      btPointer: Result := 'Pointer';
      btVariant: Result := 'Variant';
      btClass: Result := 'Class';
      btProcPtr: Result := 'ProcPtr';
      btStaticArray: Result := 'StaticArray['+IntToStr(TPSTypeRec_StaticArray(p).Size)
        + '] of '+BaseTypeToStr(TPSTypeRec_Array(p).ArrayType);
      btPChar: Result := 'PChar';
      {+}
      {$IFNDEF PS_NOWIDESTRING}
        {$if declared(btPWideChar)}
      btPWideChar: Result := 'PWideChar';
        {$ifend}
      {$ENDIF}
      {+.}
      btCurrency: Result := 'Currency';
      btUnicodeString: Result := 'UnicodeString';
      btInterface: Result := 'Interface';
      btType: Result := 'Type';
      btEnum: Result := 'Enum';
      btExtClass: Result := 'ExtClass';
    else
      Result := 'Unknown '+IntToStr(p.BaseType);
    end;
  end; // function BaseTypeToStr
  {$ifend} // !declared(PSBaseTypeToStr)

  procedure WriteTypes;
  var T: Longint;
  begin
    Writeln('[TYPES]');
    for T := 0 to i.FTypes.Count-1 do begin
      if PIFTypeRec(i.FTypes[t]).ExportName <> '' then
        Writeln('Type ['+IntToStr(t)+']: '
          + BaseTypeToStr(PIFTypeRec(i.FTypes[t]))
          + ' Export: '
          + {+}string(PIFTypeRec(i.FTypes[t]).ExportName){+.})
      else
        Writeln('Type ['+IntToStr(t)+']: '
          + BaseTypeToStr(PIFTypeRec(i.FTypes[t])));
    end; // for
  end;

  procedure WriteVars();

    function FindType(p: Pointer): Cardinal;
    var T: Longint;
    begin
      Result := Cardinal(-1);
      for T := 0 to i.FTypes.Count -1 do begin
        if p = i.FTypes[t] then begin
          Result := t;
          Exit;
        end;
      end;
    end;

  var T: Longint;
  begin
    Writeln('[VARS]');
    for t := 0 to i.FGlobalVars.Count-1 do begin
      Writeln('Var ['+IntToStr(t)+']: '
        + IntToStr(FindType(PIFVariant(i.FGlobalVars[t])^.FType))
        + ' ' + BaseTypeToStr(PIFVariant(i.FGlobalVars[t])^.Ftype)
        + ' ' + {+}string(PIFVariant(i.FGlobalVars[t])^.Ftype.ExportName){+.});
    end;
  end; // procedure WriteVars

  procedure WriteProcs();
  var t: Longint;

    procedure WriteProc(proc: TPSProcRec);
    var
      sc, CP: Cardinal;

      function ReadData(var Data; Len: Cardinal): Boolean;
      begin
        if CP + Len <= TPSInternalProcRec(PROC).Length then begin
          Move(TPSInternalProcRec(Proc).Data[CP], Data, Len);
          CP := CP + Len;
          Result := True;
        end else Result := False;
      end;

      function ReadByte(var B: Byte): Boolean;
      begin
        if CP < TPSInternalProcRec(Proc).Length then begin
          b := TPSInternalProcRec(Proc).Data^[cp];
          Inc(CP);
          Result := True;
        end else Result := False;
      end;

      function ReadLong(var B: Cardinal): Boolean;
      begin
        if CP + 3 < TPSInternalProcRec(Proc).Length then begin
          b := Cardinal((@TPSInternalProcRec(Proc).Data[CP])^);
          Inc(CP, 4);
          Result := True;
        end else Result := False;
      end;

      function ReadWriteVariable: string;
      var
        VarType: byte;
        L1, L2: Cardinal;

        function ReadVar(FType: Cardinal): string;
        var
          F: PIFTypeRec;
          b: Byte;
          w: Word;
          l: Cardinal;
          {$IFNDEF PS_NOINT64}ff: Int64;{$ENDIF}
          e: Extended;
          ss: Single;
          d: Double;
          s: TbtString;
          c: Char;
          {$IFNDEF PS_NOWIDESTRING}
          wc: WideChar;
          ws: WideString;
          {$ENDIF}
        begin
          Result := '';
          F:= i.FTypes[Ftype];
          if f = nil then
            Exit;
          {$IFDEF FPC}{$push} // FPC: https://wiki.freepascal.org/Turn_warnings_and_hints_on_or_off
            {$warn 5057 off}  // FPC: Hint: Local variable "$1" does not seem to be initialized
            {$warn 5091 off}  // FPC: Local variable "$1" of a managed type does not seem to be initialized
          {$ENDIF}
          case f.BaseType of
            btProcPtr: begin if not ReadData({%H-}l, 4) then exit;
              Result := 'PROC: '+IntToStr(l); end;
            btU8: begin if not ReadData({%H-}b, 1) then exit;
              Result := IntToStr(tbtu8(b)); end;
            btS8: begin if not ReadData(b, 1) then exit;
              Result := IntToStr(tbts8(b)); end;
            btU16: begin if not ReadData({%H-}w, 2) then exit;
              Result := IntToStr(tbtu16(w)); end;
            btS16: begin if not ReadData(w, 2) then exit;
              Result := IntToStr(tbts16(w)); end;
            btU32: begin if not ReadData(l, 4) then exit;
              Result := IntToStr(tbtu32(l)); end;
            btS32: begin if not ReadData(l, 4) then exit;
              Result := IntToStr(tbts32(l)); end;
            {$IFNDEF PS_NOINT64}
            bts64: begin if not ReadData({%H-}ff, 8) then exit;
              Result := IntToStr(ff); end;
            {$ENDIF}
            btSingle: begin if not ReadData({%H-}ss, Sizeof(tbtsingle)) then exit;
              Result := FloatToStr(ss); end;
            btDouble: begin if not ReadData({%H-}d, Sizeof(tbtdouble)) then exit;
              Result := FloatToStr(d); end;
            btExtended: begin if not ReadData({%H-}e, Sizeof(tbtextended)) then exit;
              Result := FloatToStr(e); end;
            btPChar, btString: begin if not ReadData(l, 4) then exit;
              SetLength(s, l); if not ReadData(s[1], l) then exit;
              Result := {+}string(MakeString(s)){+.}; end;
            btSet: begin
                SetLength(s, TPSTypeRec_Set(f).aByteSize);
                if not ReadData(s[1], length(s)) then exit;
                Result := {+}string(MakeString(s)){+.};
              end;
            btChar: begin if not ReadData({%H-}c, 1) then exit;
              Result := '#'+IntToStr(ord(c)); end;
            {$IFNDEF PS_NOWIDESTRING}
            btWideChar: begin if not ReadData({%H-}wc, 2) then exit;
              Result := '#'+IntToStr(ord(wc)); end;
            {+}{$if declared(btPWideChar)}btPWideChar,{$ifend}{+.}
            btWideString: begin if not ReadData(l, 4) then exit; SetLength(ws, l);
              if not ReadData(ws[1], l*2) then exit;
              Result := {+}string(MakeWString(ws)){+.}; end;
            {$ENDIF}
          end;
          {$IFDEF FPC}{$pop}{$ENDIF}
        end; // function ReadVar

        function AddressToStr(a: Cardinal): String;
        begin
          if a < PSAddrNegativeStackStart then
            Result := 'GlobalVar['+IntToStr(a)+']'
          else
            Result := 'Base['+IntToStr(Longint(A-PSAddrStackStart))+']';
        end;

      begin // function ReadWriteVariable
        Result := '';
        if not ReadByte({%H-}VarType) then
          Exit;
        case VarType of
          0: begin
            if not ReadLong({%H-}L1) then
              Exit;
            Result := AddressToStr(L1);
          end;
          1: begin
            if not ReadLong(L1) then
              Exit;
            Result := '['+ReadVar(l1)+']';
          end;
          2: begin
            if not ReadLong(L1) then
              Exit;
            if not ReadLong({%H-}L2) then
              Exit;
            Result := AddressToStr(L1)+'.['+IntToStr(l2)+']';
          end;
          3: begin
            if not ReadLong(l1) then
              Exit;
            if not ReadLong(l2) then
              Exit;
            Result := AddressToStr(L1)+'.'+AddressToStr(l2);
          end;
        end;
      end; // function ReadWriteVariable

    var
      b: Byte;
      s: string;
      DP, D1, D2, D3, D4: Cardinal;
    begin // procedure WriteProc
      CP := 0;
      sc := 0;
      while True do begin
        DP := cp;
        if not ReadByte({%H-}b) then
          Exit;
        case b of
          CM_A: begin
            {$IFDEF FPC}
            Output := Output + ' ['+IntToStr(dp)+'] ASSIGN '+ ReadWriteVariable;
            Output := Output + ', ' + ReadWriteVariable + #13#10;
            {$ELSE}
            Writeln(' ['+IntToStr(dp)+'] ASSIGN '+ReadWriteVariable+ ', ' + ReadWriteVariable);
            {$ENDIF}
          end;
          CM_CA: begin
            if not ReadByte(b) then
              Exit;
            case b of
              0: s:= '+';
              1: s := '-';
              2: s := '*';
              3: s:= '/';
              4: s:= 'MOD';
              5: s:= 'SHL';
              6: s:= 'SHR';
              7: s:= 'AND';
              8: s:= 'OR';
              9: s:= 'XOR';
              else
                Exit;
            end;
            Writeln(' ['+IntToStr(dp)+'] CALC '+ReadWriteVariable+ ' '+s+' ' + ReadWriteVariable);
          end;
          CM_P: begin
            Inc(sc);
            Writeln(' ['+IntToStr(dp)+'] PUSH '+ReadWriteVariable + ' // '+IntToStr(sc));
          end;
          CM_PV: begin
            Inc(sc);
            Writeln(' ['+IntToStr(dp)+'] PUSHVAR '+ReadWriteVariable + ' // '+IntToStr(sc));
          end;
          CM_PO: begin
            Dec(Sc);
            Writeln(' ['+IntToStr(dp)+'] POP // '+IntToStr(sc));
          end;
          Cm_C: begin
            if not ReadLong({%H-}D1) then
              Exit;
            Writeln(' ['+IntToStr(dp)+'] CALL '+IntToStr(d1));
          end;
          Cm_PG: begin
            if not ReadLong(D1) then
              Exit;
            Writeln(' ['+IntToStr(dp)+'] POP/GOTO currpos + '
              + UIntToStr(D1)
              + ' ['
              + UIntToStr(CP+D1)
              + ']');
          end;
          Cm_P2G: begin
            if not ReadLong(D1) then
              Exit;
            Writeln(' ['+IntToStr(dp)+'] POP2/GOTO currpos + '
              + UIntToStr(D1)
              + ' ['
              + UIntToStr(CP+D1)
              + ']');
          end;
          Cm_G: begin
            if not ReadLong(D1) then
              Exit;
            Writeln(' ['
              + UIntToStr(DP)
              + '] GOTO currpos + '
              + UIntToStr(D1)
              + ' ['
              + UIntToStr(CP+D1)
              + ']');
          end;
          Cm_CG: begin
            if not ReadLong(D1) then
              Exit;
            Writeln(' ['
              + UIntToStr(DP)
              + '] COND_GOTO currpos + '
              + UIntToStr(D1)
              + ' ' + ReadWriteVariable + ' ['
              + UIntToStr(CP+D1)+']');
          end;
          Cm_CNG: begin
            if not ReadLong(D1) then
              Exit;
            Writeln(' ['
              + UIntToStr(DP)
              + '] COND_NOT_GOTO currpos + '
              + UIntToStr(D1)
              + ' ' + ReadWriteVariable + ' ['
              + UIntToStr(CP+D1)
              + ']');
          end;
          Cm_R: begin
            Writeln( ' [' +UIntToStr(DP) + '] RET' );
          end;
          Cm_ST: begin
            if not ReadLong(d1) or not readLong({%H-}d2) then
              Exit;
            Writeln(' ['+IntToStr(dp)+'] SETSTACKTYPE Base['+IntToStr(d1)+'] '+IntToStr(d2));
          end;
          Cm_Pt: begin
            Inc(sc);
            if not ReadLong(D1) then
              Exit;
            Writeln(' ['+IntToStr(dp)+'] PUSHTYPE '+IntToStr(d1)
              + '('+BaseTypeToStr(TPSTypeRec(I.FTypes[d1]))+') // '+IntToStr(sc));
          end;
          CM_CO: begin
            if not ReadByte(b) then
              Exit;
            case b of
              0: s := '>=';
              1: s := '<=';
              2: s := '>';
              3: s := '<';
              4: s := '<>';
              5: s := '=';
              else
                Exit;
            end;
            Writeln(' ['+IntToStr(dp)+'] COMPARE into '+ReadWriteVariable
              + ': '+ReadWriteVariable+' '+s+' '+ReadWriteVariable);
          end;
          Cm_cv: begin
            Writeln(' ['+IntToStr(dp)+'] CALLVAR '+ReadWriteVariable);
          end;
          Cm_inc: begin
            Writeln(' ['+IntToStr(dp)+'] INC '+ReadWriteVariable);
          end;
          Cm_dec: begin
            Writeln(' ['+IntToStr(dp)+'] DEC '+ReadWriteVariable);
          end;
          cm_sp: begin
            Writeln(' ['+IntToStr(dp)+'] SETPOINTER '+ReadWriteVariable
              + ': '+ReadWriteVariable);
          end;
          cm_spc: begin
            Writeln(' ['+IntToStr(dp)+'] SETCOPYPOINTER '+ReadWriteVariable
              + ': '+ReadWriteVariable);
          end;
          cm_in: begin
            Writeln(' ['+IntToStr(dp)+'] INOT '+ReadWriteVariable);
          end;
          cm_bn: begin
            Writeln(' ['+IntToStr(dp)+'] BNOT '+ReadWriteVariable);
          end;
          cm_vm: begin
            Writeln(' ['+IntToStr(dp)+'] MINUS '+ReadWriteVariable);
          end;
          cm_sf: begin
             s := ReadWriteVariable;
             if not ReadByte(b) then
               Exit;
             if b = 0 then
               Writeln(' ['+IntToStr(dp)+'] SETFLAG '+s)
             else
               Writeln(' ['+IntToStr(dp)+'] SETFLAG NOT '+s);
           end;
           cm_fg: begin
             if not ReadLong(D1) then
               Exit;
             Writeln(' ['
               + IntToStr(DP)
               + '] FLAGGOTO currpos + '
               + UIntToStr(D1)
               + ' ['
               + UIntToStr(CP+D1)
               + ']');
           end;
           cm_puexh: begin
             if not ReadLong(D1) then
               Exit;
             if not ReadLong(D2) then
               Exit;
             if not ReadLong({%H-}D3) then
               Exit;
             if not ReadLong({%H-}D4) then
               Exit;
             Writeln(' ['+IntToStr(dp)+'] PUSHEXCEPTION '+IntToStr(d1)+' '+IntToStr(d2)
               + ' '+IntToStr(d3)+' '+IntToStr(d4));
           end;
           cm_poexh: begin
             if not ReadByte(b) then
               Exit;
             Writeln(' ['+IntToStr(dp)+'] POPEXCEPTION '+IntToStr(b));
           end;
        else
          begin
            Writeln(' Disasm Error');
            Break;
          end;
        end;
      end; // while True
    end; // procedure WriteProc

  begin // procedure WriteProcs
    Writeln('[PROCS]');
    for t := 0 to i.FProcs.Count-1 do begin
      if TPSProcRec(i.FProcs[t]).ClassType = TIFExternalProcRec then begin
        if TPSExternalProcRec(i.FProcs[t]). Decl = '' then
          Writeln('Proc ['+IntToStr(t)+']: External: '
            + {+}string(TPSExternalProcRec(i.FProcs[t]).Name){+.})
        else
          Writeln('Proc ['+IntToStr(t)+']: External Decl: '
            + Debug2Str({+}string(TIFExternalProcRec(i.FProcs[t]).Decl){+.})
            + ' ' + {+}string(TIFExternalProcRec(i.FProcs[t]).Name){+.});
      end else begin
        if TPSInternalProcRec(i.FProcs[t]).ExportName <> '' then begin
          Writeln('Proc ['+IntToStr(t)+'] Export: '
            + {+}string(TPSInternalProcRec(i.FProcs[t]).ExportName)
            + ' '+string(TPSInternalProcRec(i.FProcs[t]).ExportDecl){+.});
        end else begin
          Writeln('Proc ['+IntToStr(t)+']');
        end;
        Writeproc(i.FProcs[t]);
      end;
    end; // for t
  end; // procedure WriteProcs

begin
  Result := False;
  I := TMyPSExec.Create;
  try
    try
      I.AddSpecialProcImport('', @SpecImportProc, nil);
      if I.LoadData(Input) then begin
        Output := '';
        WriteTypes;
        WriteVars;
        WriteProcs;
        //
        Result := True;
      end;
    except
      {$IFDEF _DCC_MSG_}
      {$if declared(dbg)}
      on e: Exception do begin
        dbg('ERROR: uPSDisasembly.IFPS3DataToText: '+e.Message);
      end;
      {$ifend}
      {$ENDIF}
    end;
  finally
    I.Free;
  end;
end; // function IFPS3DataToText

{ TMyIFPSExec }

function MyDummyProc({%H-}Caller: TPSExec; {%H-}p: TIFExternalProcRec; {%H-}Global, {%H-}Stack: TPSStack): Boolean;
begin
  Result := False;
end;

function TMyPSExec.ImportProc(const Name: ShortString; proc: TIFExternalProcRec): Boolean;
begin
  Proc.ProcPtr := MyDummyProc;
  Result := True;
end;

function SpecImportProc(Sender: TObject; p: TIFExternalProcRec): Boolean;
begin
  p.ProcPtr := MyDummyProc;
  Result := True;
end;

end.
