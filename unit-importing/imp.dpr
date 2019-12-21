program imp;
{+}
{$i app_linker.inc}
{$i FxtVer.inc}
{+.}
uses
  Forms,
  {+}
  //ExceptDlg,
  //ExceptDlg in '..\..\..\JCL\VCL\ExceptDlg.pas' {ExceptionDialog},
  {$IFDEF CONDITIONALEXPRESSIONS}
  {$IF CompilerVersion <= 18.50} // Delphi 2007 Down
    VCLFixPack, { optional }
  {$IFEND}
  {$ENDIF}
  {+.}
  Main in 'Main.pas' {frmMain},
  ParserU in 'ParserU.pas',
  ParserUtils in 'ParserUtils.pas',
  BigIni in 'BigIni.pas',
  FormSettings in 'FormSettings.pas' {frmSettings},
  UFrmGotoLine in 'UFrmGotoLine.pas' {frmGotoLine};

{$R *.res}
{+}
{.$R Main.dfm}
{+.}
begin
  Application.Initialize;
  {+}
  Application.ShowMainForm := True;
  {+.}
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TfrmGotoLine, frmGotoLine);
  Application.Run;
end.
