program cmdimp;

{$APPTYPE CONSOLE}
{+}
{$i app_linker.inc}
{$i FxtVer.inc}
{+.}
uses
  SysUtils,
  Classes,
  //Windows,
  ParserU in 'ParserU.pas',
  ParserUtils in 'ParserUtils.pas';

{+}
{.$R *.res}
{$R imp.res}
{+.}

var
  s: TStringList;
  x: TUnitParser;
  {+}
  sFileName, sFilePath, sOutputUnitSufix: string;
  {+.}
begin
  if ParamCount = 0 then
  begin
    Writeln('Command Line Unit Importing');
    writeln;
    {+}
    writeln('cmdimp <filename> [<output-unit-sufix>]');
    {+.}
    Halt(1);
  end;
  try
    s := TStringList.Create;
    try
      {+}
      sFileName := Paramstr(1);
      if not FileExists(sFileName) then begin
        Writeln('ERROR: not exists file "'+sFileName+'"');
        Halt(2);
      end;
      sFilePath := ExtractFilePath(sFileName);
      sOutputUnitSufix := Trim(Paramstr(2));

      s.LoadFromFile(sFileName);
      x := TUnitParser.Create('');
      x.Unitname := ExtractFileName(sFileName);
      x.SingleUnit := True;
      {+}
      x.UnitFileSufix := sOutputUnitSufix;
      {+.}
      x.ParseUnit(s.Text);
      x.SaveToPath(sFilePath);
      Writeln('Successfully processed. Saved to file: "'+sFilePath+x.{+}{UnitNameCmp}UnitFileCmp{+.}+'"');
      {+.}
    finally
      s.Free;
    end;
  {+}
  except
    on e: Exception do begin
      Writeln('EXCEPTION: '+e.ClassName+': '+e.Message);
      Halt(3);
    end;
  end;
  {+.}
end.
