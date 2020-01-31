unit uPSR_dateutils;
{$I PascalScript.inc}
interface
uses
  SysUtils, uPSRuntime;

procedure RegisterDateTimeLibrary_R(S: TPSExec);

implementation

function TryEncodeDate(Year, Month, Day: Word; var Date: TDateTime): Boolean;
begin
  try
    Date := EncodeDate(Year, Month, Day);
    Result := true;
  except
    Result := false;
  end;
end;

function TryEncodeTime(Hour, Min, Sec, MSec: Word; var Time: TDateTime): Boolean;
begin
  try
    Time := EncodeTime(hour, Min, Sec, MSec);
    Result := true;
  except
    Result := false;
  end;
end;

function DateTimeToUnix(D: TDateTime): Int64;
begin
  Result := Round((D - 25569) * 86400);
end;

function UnixToDateTime(U: Int64): TDateTime;
begin
  Result := U / 86400 + 25569;
end;

function DateToStr_(D: TDateTime): string;
begin
  Result := DateToStr(D);
end;

function StrToDate_(const S: string): TDateTime;
begin
  Result := StrToDate(S);
end;

function DateTimeToStr_(D: TDateTime): string;
begin
  Result := DateTimeToStr(D);
end;

function FormatDateTime_(const Format: string; DateTime: TDateTime): string;
begin
  Result := FormatDateTime(Format, DateTime);
end;

procedure RegisterDateTimeLibrary_R(S: TPSExec);
begin
  S.RegisterDelphiFunction(@EncodeDate, 'EncodeDate', cdRegister);
  S.RegisterDelphiFunction(@EncodeTime, 'EncodeTime', cdRegister);
  S.RegisterDelphiFunction(@TryEncodeDate, 'TryEncodeDate', cdRegister);
  S.RegisterDelphiFunction(@TryEncodeTime, 'TryEncodeTime', cdRegister);
  S.RegisterDelphiFunction(@DecodeDate, 'DecodeDate', cdRegister);
  S.RegisterDelphiFunction(@DecodeTime, 'DecodeTime', cdRegister);
  S.RegisterDelphiFunction(@DayOfWeek, 'DayOfWeek', cdRegister);
  S.RegisterDelphiFunction(@Date, 'Date', cdRegister);
  S.RegisterDelphiFunction(@Time, 'Time', cdRegister);
  S.RegisterDelphiFunction(@Now, 'Now', cdRegister);
  S.RegisterDelphiFunction(@DateTimeToUnix, 'DateTimeToUnix', cdRegister);
  S.RegisterDelphiFunction(@UnixToDateTime, 'UnixToDateTime', cdRegister);
  S.RegisterDelphiFunction(@DateToStr_, 'DateToStr', cdRegister);
  S.RegisterDelphiFunction(@DateTimeToStr_, 'DateTimeToStr', cdRegister);
  S.RegisterDelphiFunction(@FormatDateTime_, 'FormatDateTime', cdRegister);
  S.RegisterDelphiFunction(@StrToDate_, 'StrToDate', cdRegister);
end;

end.
