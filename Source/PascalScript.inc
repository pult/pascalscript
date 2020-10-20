{ PascalScript.inc } // version: 2020.0628.1520
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

{$IFDEF DEBUG}
  {$IFDEF DEBUG_IDE}
  {$ELSE}
  {$ENDIF}
  {$IFDEF DELPHI}
  {$ELSE}
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
}
//+:
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
{+.}
{$ENDIF !FPC}
{+}
{$IFDEF FPC}
  {$DEFINE UNICODE_OR_FPC}
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
