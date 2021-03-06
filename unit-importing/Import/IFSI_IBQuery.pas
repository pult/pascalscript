unit IFSI_IBQuery;
{
This file has been generated by UnitParser v0.5, written by M. Knight
and updated by NP. v/d Spek.
Source Code from Carlo Kok has been used to implement various sections of
UnitParser. Components of ifps3 are used in the construction of UnitParser,
code implementing the class wrapper is taken from Carlo Kok''s conv unility

}
{$I ifps3_def.inc}
interface

uses
   SysUtils
  ,Classes
  ,IFPS3CompExec
  ,ifpscomp
  ,ifps3
  ;

type
(*----------------------------------------------------------------------------*)
  TIFPS3CE_IBQuery = class(TIFPS3Plugin)
  protected
    procedure CompOnUses(CompExec: TIFPS3CompExec); override;
    procedure ExecOnUses(CompExec: TIFPS3CompExec); override;
    procedure CompileImport1(CompExec: TIFPS3CompExec); override;
    procedure CompileImport2(CompExec: TIFPS3CompExec); override;
    procedure ExecImport1(CompExec: TIFPS3CompExec; const ri: TIFPSRuntimeClassImporter); override;
    procedure ExecImport2(CompExec: TIFPS3CompExec; const ri: TIFPSRuntimeClassImporter); override;
  end;


(*
{ compile-time registration functions }
procedure SIRegister_TIBQuery(CL: TIFPSPascalCompiler);
procedure SIRegister_IBQuery(CL: TIFPSPascalCompiler);

{ run-time registration functions }
procedure RIRegister_TIBQuery(CL: TIFPSRuntimeClassImporter);
procedure RIRegister_IBQuery(CL: TIFPSRuntimeClassImporter);
*)

implementation

uses
   DB
  ,IBHeader
  ,IB
  ,IBDatabase
  ,IBCustomDataSet
  ,IBSQL
  ,IBQuery
  ;


{ compile-time importer function }
(*----------------------------------------------------------------------------
 Sometimes the CL.AddClassN() fails to correctly register a class,
 for unknown (at least to me) reasons
 So, you may use the below RegClassS() replacing the CL.AddClassN()
 of the various SIRegister_XXXX calls
 ----------------------------------------------------------------------------*)
function RegClassS(CL: TIFPSPascalCompiler; const InheritsFrom, Classname: string): TIFPSCompileTimeClass;
begin
  Result := CL.FindClass(Classname);
  if Result = nil then
    Result := CL.AddClassN(CL.FindClass(InheritsFrom), Classname)
  else Result.ClassInheritsFrom := CL.FindClass(InheritsFrom);
end;


(* === compile-time registration functions === *)
(*----------------------------------------------------------------------------*)
procedure SIRegister_TIBQuery(CL: TIFPSPascalCompiler);
begin
  //with RegClassS(CL,'TIBCustomDataSet', 'TIBQuery') do
  with CL.AddClassN(CL.FindClass('TIBCustomDataSet'),'TIBQuery') do
  begin
    RegisterMethod('Procedure BatchInput( InputObject : TIBBatchInput)');
    RegisterMethod('Procedure BatchOutput( OutputObject : TIBBatchOutput)');
    RegisterMethod('Procedure ExecSQL');
    RegisterMethod('Function ParamByName( const Value : string) : TParam');
    RegisterMethod('Procedure Prepare');
    RegisterMethod('Procedure UnPrepare');
    RegisterProperty('Prepared', 'Boolean', iptrw);
    RegisterProperty('ParamCount', 'Word', iptr);
    RegisterProperty('StmtHandle', 'TISC_STMT_HANDLE', iptr);
    RegisterProperty('Text', 'string', iptr);
    RegisterProperty('RowsAffected', 'Integer', iptr);
    RegisterProperty('GenerateParamNames', 'Boolean', iptrw);
    RegisterProperty('SQL', 'TStrings', iptrw);
    RegisterProperty('Params', 'TParams', iptrw);
  end;
end;

(*----------------------------------------------------------------------------*)
procedure SIRegister_IBQuery(CL: TIFPSPascalCompiler);
begin
  SIRegister_TIBQuery(CL);
end;

(* === run-time registration functions === *)
(*----------------------------------------------------------------------------*)
procedure TIBQueryParams_W(Self: TIBQuery; const T: TParams);
begin Self.Params := T; end;

(*----------------------------------------------------------------------------*)
procedure TIBQueryParams_R(Self: TIBQuery; var T: TParams);
begin T := Self.Params; end;

(*----------------------------------------------------------------------------*)
procedure TIBQuerySQL_W(Self: TIBQuery; const T: TStrings);
begin Self.SQL := T; end;

(*----------------------------------------------------------------------------*)
procedure TIBQuerySQL_R(Self: TIBQuery; var T: TStrings);
begin T := Self.SQL; end;

(*----------------------------------------------------------------------------*)
procedure TIBQueryGenerateParamNames_W(Self: TIBQuery; const T: Boolean);
begin Self.GenerateParamNames := T; end;

(*----------------------------------------------------------------------------*)
procedure TIBQueryGenerateParamNames_R(Self: TIBQuery; var T: Boolean);
begin T := Self.GenerateParamNames; end;

(*----------------------------------------------------------------------------*)
procedure TIBQueryRowsAffected_R(Self: TIBQuery; var T: Integer);
begin T := Self.RowsAffected; end;

(*----------------------------------------------------------------------------*)
procedure TIBQueryText_R(Self: TIBQuery; var T: string);
begin T := Self.Text; end;

(*----------------------------------------------------------------------------*)
procedure TIBQueryStmtHandle_R(Self: TIBQuery; var T: TISC_STMT_HANDLE);
begin T := Self.StmtHandle; end;

(*----------------------------------------------------------------------------*)
procedure TIBQueryParamCount_R(Self: TIBQuery; var T: Word);
begin T := Self.ParamCount; end;

(*----------------------------------------------------------------------------*)
procedure TIBQueryPrepared_W(Self: TIBQuery; const T: Boolean);
begin Self.Prepared := T; end;

(*----------------------------------------------------------------------------*)
procedure TIBQueryPrepared_R(Self: TIBQuery; var T: Boolean);
begin T := Self.Prepared; end;

(*----------------------------------------------------------------------------*)
procedure RIRegister_TIBQuery(CL: TIFPSRuntimeClassImporter);
begin
  with CL.Add(TIBQuery) do
  begin
    RegisterMethod(@TIBQuery.BatchInput, 'BatchInput');
    RegisterMethod(@TIBQuery.BatchOutput, 'BatchOutput');
    RegisterMethod(@TIBQuery.ExecSQL, 'ExecSQL');
    RegisterMethod(@TIBQuery.ParamByName, 'ParamByName');
    RegisterMethod(@TIBQuery.Prepare, 'Prepare');
    RegisterMethod(@TIBQuery.UnPrepare, 'UnPrepare');
    RegisterPropertyHelper(@TIBQueryPrepared_R,@TIBQueryPrepared_W,'Prepared');
    RegisterPropertyHelper(@TIBQueryParamCount_R,nil,'ParamCount');
    RegisterPropertyHelper(@TIBQueryStmtHandle_R,nil,'StmtHandle');
    RegisterPropertyHelper(@TIBQueryText_R,nil,'Text');
    RegisterPropertyHelper(@TIBQueryRowsAffected_R,nil,'RowsAffected');
    RegisterPropertyHelper(@TIBQueryGenerateParamNames_R,@TIBQueryGenerateParamNames_W,'GenerateParamNames');
    RegisterPropertyHelper(@TIBQuerySQL_R,@TIBQuerySQL_W,'SQL');
    RegisterPropertyHelper(@TIBQueryParams_R,@TIBQueryParams_W,'Params');
  end;
end;

(*----------------------------------------------------------------------------*)
procedure RIRegister_IBQuery(CL: TIFPSRuntimeClassImporter);
begin
  RIRegister_TIBQuery(CL);
end;



{ TIFPS3CE_IBQuery }
(*----------------------------------------------------------------------------*)
procedure TIFPS3CE_IBQuery.CompOnUses(CompExec: TIFPS3CompExec);
begin
  { nothing }
end;
(*----------------------------------------------------------------------------*)
procedure TIFPS3CE_IBQuery.ExecOnUses(CompExec: TIFPS3CompExec);
begin
  { nothing }
end;
(*----------------------------------------------------------------------------*)
procedure TIFPS3CE_IBQuery.CompileImport1(CompExec: TIFPS3CompExec);
begin
  SIRegister_IBQuery(CompExec.Comp);
end;
(*----------------------------------------------------------------------------*)
procedure TIFPS3CE_IBQuery.CompileImport2(CompExec: TIFPS3CompExec);
begin
  { nothing }
end;
(*----------------------------------------------------------------------------*)
procedure TIFPS3CE_IBQuery.ExecImport1(CompExec: TIFPS3CompExec; const ri: TIFPSRuntimeClassImporter);
begin
  RIRegister_IBQuery(ri);
end;
(*----------------------------------------------------------------------------*)
procedure TIFPS3CE_IBQuery.ExecImport2(CompExec: TIFPS3CompExec; const ri: TIFPSRuntimeClassImporter);
begin
  { nothing }
end;


initialization
 (**)
{$IFDEF USEIMPORTER}
CIImporter.AddCallBack(@SIRegister_IBQuery,PT_ClassImport);
{$ENDIF}
finalization
 (**)

end.
