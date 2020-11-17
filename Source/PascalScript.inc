{ PascalScript.inc } // version: 2020.1117.1145
{----------------------------------------------------------------------------}
{ RemObjects Pascal Script                                                   }
{                                                                            }
{ Compiler: Delphi 2 and up, Kylix 3 and up                                  }
{ Platform: Win32, Linux                                                     }
{                                                                            }
{ (C)opyright RemObjects Software. All rights reserved.                      }
{                                                                            }
{----------------------------------------------------------------------------}

{$INCLUDE eDefines.inc}

{$IFDEF FPC}
  {$MODE DELPHI}{$H+}
  //{$MODE DelphiUnicode}{$H+}
  //{$MODE OBJFPC}{$H+}
{$ENDIF FPC}

{-$DEFINE _DCC_MSG_} // optional
{$IFDEF DEBUG}
  {-WARN MESSAGE_DIRECTIVE ON} // dcc+ fpc-
  {-HINT MESSAGE_DIRECTIVE ON} // dcc- fpc+
  {-NOTE MESSAGE_DIRECTIVE ON} // dcc- fpc+

  {$IFDEF DEBUG_IDE}
    {$DEFINE _DCC_MSG_}  // optional
  {$ELSE}
    {.$DEFINE _DCC_MSG_} // optional
  {$ENDIF}
{$ENDIF}
{$IFDEF _DCC_MSG_}
  {$IFDEF DELPHI}
    {$IFNDEF DELPHI11UP}
      {$UNDEF _DCC_MSG_} // when dcc unsupported directives: $MESSAGE $WARN
    {$ELSE}
      {-$WARN MESSAGE_DIRECTIVE ON} // optional // dcc+ fpc-
    {$ENDIF}
  {$ELSE}
    {-$HINT MESSAGE_DIRECTIVE ON} // optional // dcc- fpc+
    {-$NOTE MESSAGE_DIRECTIVE ON} // optional // dcc- fpc+
  {$ENDIF}
{$ENDIF}

{$IFNDEF FPC}
  {$IFDEF VER125}{C4}{$B-}{$X+}{$T-}{$H+}{$ENDIF}
  {$IFDEF VER110}{C3}{$B-}{$X+}{$T-}{$H+}{$ENDIF}
  {$IFDEF VER93}{C1}{$B-}{$X+}{$T-}{$H+}{$ENDIF}

  {$IFDEF DELPHI4UP}
    {$DEFINE PS_HAVEVARIANT}
    {$DEFINE PS_DYNARRAY}
  {$ENDIF}
{$ENDIF !FPC}

{$IFNDEF FPC}
  {$B-}{$X+}{$T-}{$H+}
{$ELSE}
  {$R-}{$Q-}
{$ENDIF}

{$IFNDEF FPC}
  {$IFNDEF DELPHI4UP}
  {$IFNDEF LINUX}
    {$DEFINE PS_NOINT64}
  {$ENDIF !LINUX}
  {$ENDIF !DELPHI4UP}

  {$IFDEF DELPHI2}
    {$DEFINE PS_NOINT64}
    {$DEFINE PS_NOWIDESTRING}
    {$B-}{$X+}{$T-}{$H+}
  {$ENDIF}

  {$IFDEF LINUX}{KYLIX}{$DEFINE CLX}{$DEFINE DELPHI3UP}{$DEFINE DELPHI6UP}{$ENDIF}
{$ENDIF !FPC}
{$R-}{$Q-}

{
Defines:
  PS_NOSMARTLIST    - Don't use the smart list option
  PS_NOIDISPATCH
  PS_NOWIDESTRING
  PS_NOINT64
  PS_DELPHIDIV
  PS_USESSUPPORT
  PS_PANSICHAR       ?
  //+:
  PS_BTSTRINGNATIVE - for unicode not use AnsiString type (tbtString mapped to string)
  //+.
}
//+:
{-$DEFINE PS_BTSTRINGNATIVE} { optional: default: not defined }
{-$DEFINE PS_USESSUPPORT}    { optional: default: not defined } // TODO: Need test it ...

{$UNDEF DEBUG} { optional }
//+.

{$IFDEF CLX}
  {$DEFINE PS_NOIDISPATCH} // not implemented
{$ENDIF}
{$IFNDEF MSWINDOWS}
  {$DEFINE PS_NOIDISPATCH} // not implemented
{$ENDIF}

{$IFDEF FPC}
  // FPC :
  {$I PascalScriptFPC.inc}
{+}
{$ELSE !FPC}
  // DELPHI :
  {$IFDEF DELPHI23UP}
    {$DEFINE PS_INVOK_RTTI_IMPL} // optional
  {$ENDIF DELPHI23UP}
  {$IFDEF PS_INVOK_RTTI_IMPL}{$IFNDEF PS_USECLASSICINVOKE}
    {--$DEFINE USEINVOKECALL} // optional
    {$if not ( defined(CPUX86) or defined(CPUX64) )}
      {$DEFINE USEINVOKECALL} // optional
    {$ifend}
  {$ENDIF}{$ENDIF}
{+.}
{$ENDIF !FPC}
{+}
{$IFDEF USEINVOKECALL}
  {$DEFINE PS_USE_INVOK_RTTI}
{$ENDIF}

{$IFDEF FPC}
  {$DEFINE UNICODE_OR_FPC}
{$ENDIF}

{$IFDEF CPU64}
  {$UNDEF PS_NOINT64}
{$ENDIF}

{$D+} { optional }
{$IFNDEF FPC}
  {$L+}
  {$IFDEF UNICODE}
    {$DEFINE UNICODE_OR_FPC}
  {$ENDIF}
  {$IFDEF DELPHI18UP} // DELPHIXE4 UP
    {$ZEROBASEDSTRINGS OFF}
  {$ENDIF}
{$ENDIF !FPC}
{+.}
