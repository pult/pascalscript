program rps_build_lib;
{$I PascalScript.inc}

uses
  Classes, SysUtils, CustApp
  { you can add units after this }
  {$IFDEF MSWINDOWS}
  ,Windows
  {$ENDIF}
  //
  ,uPSUtils
  ,uPSRuntime
  ,uPSCompiler
  ,uPSPreProcessor
  ,uPSDisassembly
  ,uPSDebugger

{
  ,uPSC_dll
  ,uPSR_dll

  ,uPSC_std
  ,uPSR_std
  ,uPSC_classes
  ,uPSR_classes
  ,uPSC_dateutils
  ,uPSR_dateutils

  ,uPSC_comobj
  ,uPSR_comobj
  ,uPSC_DB
  ,uPSR_DB

  ,uPSComponent
  ,uPSComponentExt
//}

{
  ,uPSComponent_Default
  ,uPSComponent_COM
  ,uPSComponent_DB
//}

{
  ,uPSC_graphics // FPC: required Lazarus "LCL" package!
  ,uPSR_graphics
//}

(*
  {$IFDEF FPC}
  ,Interfaces // this includes the LCL widgetset
  {$ENDIF}
  // FPC: required use module "Interfaces":
  ,uPSC_controls
  ,uPSR_controls
  ,uPSC_stdctrls
  ,uPSR_stdctrls
  ,uPSC_extctrls
  ,uPSR_extctrls
  ,uPSC_buttons
  ,uPSR_buttons
  ,uPSC_menus
  ,uPSR_menus
  ,uPSC_forms
  ,uPSR_forms

  ,uPSComponent_StdCtrls
  ,uPSComponent_Controls
  ,uPSComponent_Forms
//*)
  ;

{$if declared (TCustomApplication)}
type

  { TMyApp }

  TMyApp = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;
{$endif declared (TCustomApplication)}

procedure dbg(const {%H-}S: UnicodeString); inline;
begin
  {$if declared (OutputDebugStringW)}
  OutputDebugStringW(PWideChar(UnicodeString('rps-lib> ') + S));
  {$endif declared (OutputDebugStringW)}
end;

{$if declared (TCustomApplication)}

{ TMyApp }

procedure TMyApp.DoRun;
var
  ErrorMsg: String;
begin
  // quick check parameters
  ErrorMsg:=String(CheckOptions('h', 'help'));
  if ErrorMsg<>'' then begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;

  // parse parameters
  if HasOption('h', 'help') then begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  { add your program here }

  // stop program loop
  Terminate;
end;

constructor TMyApp.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TMyApp.Destroy;
begin
  inherited Destroy;
end;

procedure TMyApp.WriteHelp;
begin
  { add your help code here }
  writeln('Usage: ', ExeName, ' -h');
end;

var
  Application: TMyApp;
{$endif declared (TCustomApplication)}

begin
  dbg('Start:');
  {$if declared (TCustomApplication)}
  Application:=TMyApp.Create(nil);
  Application.Title:='My App';
  Application.Run;
  Application.Free;
  {$else}
  {$endif declared (TCustomApplication)}
  dbg('Done!');
end.
