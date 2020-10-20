{ PascalScriptFPC.inc } // version: 2020.1010.1010
{----------------------------------------------------------------------------}
{ RemObjects Pascal Script                                                   }
{----------------------------------------------------------------------------}

{--$DEFINE _INVOKECALL_IMPL_} // optional // TODO: currentry not all parameter types supported
{$if ( defined(cpuaarch64) or defined(cpuarm) )}
//{$if ( defined(cpuaarch64) or defined(cpuarm) ) or defined(cpu64) }
//{$if ( defined(cpuaarch64) or defined(cpuarm) ) or ( defined(cpu64) and not defined(mswindows) )}
  {$DEFINE _INVOKECALL_IMPL_}
  // view:"C:\fpcupdeluxe\fpcsrc\packages\rtl-objpas\src\inc\rtti.pp"
{$ifend}
{$IFDEF _INVOKECALL_IMPL_}{$IFNDEF PS_USECLASSICINVOKE}{$IFNDEF MSWINDOWS}
  {$DEFINE USEINVOKECALL} // optional
{$ENDIF}{$ENDIF}{$ENDIF}

{$DEFINE PS_HAVEVARIANT}
{$DEFINE PS_DYNARRAY}
{$IFNDEF MSWINDOWS}
  {$DEFINE PS_NOIDISPATCH}
{$ENDIF}
{$if (fpc_version>2) or ((fpc_version=2) and ((fpc_release=2) and (fpc_patch>=4)) or (fpc_release>2))}
  {$UNDEF FPC_OLD_FIX}
  {$DEFINE PS_STACKALIGN}
  {$UNDEF PS_FPCSTRINGWORKAROUND}
  {$DEFINE PS_RESBEFOREPARAMETERS}
  {$DEFINE x64_string_result_as_varparameter}
  {$IFDEF MSWINDOWS}
    {$if (fpc_version>2) or ((fpc_version=2) and (fpc_release>5))}
     {$DEFINE PS_FPC_HAS_COM}
    {$endif}
  {$ENDIF}
{FreePascal 2.3.1 and above has much Delphi compatibility bugs fixed}
{$else}
  {$DEFINE FPC_OLD_FIX}
  {$DEFINE PS_FPCSTRINGWORKAROUND}
{$ifend}
{$DEFINE DELPHI3UP}
{.$DEFINE DELPHI3UP_OR_FPC}
{$DEFINE DELPHI6UP}
{.$DEFINE DELPHI6UP_OR_FPC}

(*
{$IFDEF FPC}        // FPC: https://wiki.freepascal.org/Turn_warnings_and_hints_on_or_off
  {$warn 4015 off}  // FPC: Hint: Use DIV instead to get an integer result
  {$warn 4035 off}  // FPC: Hint: Mixing signed expressions and longwords gives a 64bit result
  {$warn 4055 off}  // FPC: Hint: Conversion between ordinals and pointers is not portable
  {$warn 4056 off}  // FPC: Warning: Conversion between ordinals and pointers is not portable
  {$warn 4081 off}  // FPC: Hint: Converting the operands to "$1" before doing the multiply could prevent overflow errors.
  {$warn 4082 off}  // FPC: Warning: Converting pointers to signed integers may result in wrong comparison results and range errors, use an unsigned type instead.
  {$warn 5037 off}  // FPC: Warning: Local variable "$1" does not seem to be initialized
  {$warn 5057 off}  // FPC: Hint: Local variable "$1" does not seem to be initialized
  {$warn 5060 off}  // FPC: Hint: Function result variable does not seem to be initialized
  {$warn 5091 off}  // FPC: Local variable "$1" of a managed type does not seem to be initialized
  {$warn 5093 off}  // FPC: Warning: Function result variable of a managed type does not seem to be initialized
  {$warn 5094 off}  // FPC: Hint: Function result variable of a managed type does not seem to be initialized
{$ENDIF}
//*)
