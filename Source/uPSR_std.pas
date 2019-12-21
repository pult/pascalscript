unit uPSR_std;
{$I PascalScript.inc}
interface
uses
  uPSRuntime, uPSUtils;

procedure RIRegisterTObject(CL: TPSRuntimeClassImporter);
procedure RIRegisterTPersistent(Cl: TPSRuntimeClassImporter);
procedure RIRegisterTComponent(Cl: TPSRuntimeClassImporter);
procedure RIRegister_Std(Cl: TPSRuntimeClassImporter);

implementation
uses
  Classes;

{+}
{.$IFDEF DELPHI} // @@@ TODO: FPC Check ...
procedure TObject_Free(ASelf: TObject);
begin
  if (ASelf <> nil) {my fix for "system.pas":}and (ASelf.ClassType <> nil) then
  begin
    ASelf.Destroy;

    //
    // Clear Self ClssType pointer ( ASelf.ClassType := nil )
    //
    //  Effects:
    //    -  !!! disable call of any vmt methods for destroyed object
    //    - Double call of ".Free" no generate Exception "EInvalidPointer"
    //
    PPointer(ASelf)^ := nil; // !!! disable call of any vmt methods for destroyed object
    //
    // Sample:
    //
    {
// ------------------------
var
  obj: TObject;
begin
  obj := TObject.Create;
  obj.Free;
  obj.Free; // default: AV; now: like as nil !
end.
// ------------------------
    }
    //
  end;
end;
{.$ENDIF DELPHI}

function TObject_ClassName(ASelf: TObject): string;
begin
  Result := ASelf.ClassName;
end;
{+.}

procedure RIRegisterTObject(CL: TPSRuntimeClassImporter);
begin
  with cl.Add(TObject) do
  begin
    RegisterConstructor(@TObject.Create, 'Create');
  {+}
    {.$IFDEF DELPHI}
    RegisterMethod(@TObject_Free, 'Free'); // @@@ TODO: FPC Check ...
    {.$ELSE}
    //RegisterMethod(@TObject.Free, 'Free');
    {.$ENDIF}

    RegisterMethod(@TObject_ClassName, 'ClassName'); // class function allow call only for assigned object
  {+.}
  end;
end;

procedure RIRegisterTPersistent(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TPersistent) do
  begin
    RegisterVirtualMethod(@TPersistent.Assign, 'Assign');
  end;
end;

procedure TComponentOwnerR(Self: TComponent; var T: TComponent); begin T := Self.Owner; end;

procedure TCOMPONENTCOMPONENTS_R(Self: TCOMPONENT; var T: TCOMPONENT; t1: INTEGER); begin T := Self.COMPONENTS[t1]; end;
procedure TCOMPONENTCOMPONENTCOUNT_R(Self: TCOMPONENT; var T: INTEGER); begin t := Self.COMPONENTCOUNT; end;
procedure TCOMPONENTCOMPONENTINDEX_R(Self: TCOMPONENT; var T: INTEGER); begin t := Self.COMPONENTINDEX; end;
procedure TCOMPONENTCOMPONENTINDEX_W(Self: TCOMPONENT; T: INTEGER); begin Self.COMPONENTINDEX := t; end;
procedure TCOMPONENTCOMPONENTSTATE_R(Self: TCOMPONENT; var T: TCOMPONENTSTATE); begin t := Self.COMPONENTSTATE; end;
procedure TCOMPONENTDESIGNINFO_R(Self: TCOMPONENT; var T: LONGINT); begin t := Self.DESIGNINFO; end;
procedure TCOMPONENTDESIGNINFO_W(Self: TCOMPONENT; T: LONGINT); begin Self.DESIGNINFO := t; end;

procedure RIRegisterTComponent(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TComponent) do
  begin
    RegisterMethod(@TComponent.FindComponent, 'FindComponent');
    RegisterVirtualConstructor(@TComponent.Create, 'Create');
    RegisterPropertyHelper(@TComponentOwnerR, nil, 'Owner');

    RegisterMethod(@TCOMPONENT.DESTROYCOMPONENTS, 'DestroyComponents');
    RegisterPropertyHelper(@TCOMPONENTCOMPONENTS_R, nil, 'Components');
    RegisterPropertyHelper(@TCOMPONENTCOMPONENTCOUNT_R, nil, 'ComponentCount');
    RegisterPropertyHelper(@TCOMPONENTCOMPONENTINDEX_R, @TCOMPONENTCOMPONENTINDEX_W, 'ComponentIndex');
    RegisterPropertyHelper(@TCOMPONENTCOMPONENTSTATE_R, nil, 'ComponentState');
    RegisterPropertyHelper(@TCOMPONENTDESIGNINFO_R, @TCOMPONENTDESIGNINFO_W, 'DesignInfo');
  end;
end;

procedure RIRegister_Std(Cl: TPSRuntimeClassImporter);
begin
  RIRegisterTObject(CL);
  RIRegisterTPersistent(Cl);
  RIRegisterTComponent(Cl);
end;
// PS_MINIVCL changes by Martijn Laan (mlaan at wintax _dot_ nl)

end.
