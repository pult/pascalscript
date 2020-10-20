{ uPSComponent_COM.pas } // version: 2020.1010.1010
{----------------------------------------------------------------------------}
{ RemObjects Pascal Script                                                   }
{----------------------------------------------------------------------------}
unit uPSComponent_COM;

interface
uses
  SysUtils, Classes, uPSComponent, {%H-}uPSCompiler, uPSRuntime;
type

  TPSImport_ComObj = class(TPSPlugin)
  private
  public
    procedure CompileImport1(CompExec: TPSScript); override;
    procedure ExecImport1(CompExec: TPSScript; const {%H-}ri: TPSRuntimeClassImporter); override;
  end;

  TIFPS3CE_ComObj = class(TPSImport_ComObj);

implementation
uses
  uPSC_comobj,
  uPSR_comobj;

{ TPSImport_ComObj }

procedure TPSImport_ComObj.CompileImport1(CompExec: TPSScript);
begin
  SIRegister_ComObj(CompExec.Comp);
end;

procedure TPSImport_ComObj.ExecImport1(CompExec: TPSScript;
  const ri: TPSRuntimeClassImporter);
begin
  RIRegister_ComObj(CompExec.Exec);
end;

end.
