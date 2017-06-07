
    (*********************************************************************
     *  Utils.pas                                                        *
     *                                                                   *
     *  This unit is Part of the DC-DSP Audio Filter v1.0                *
     *                                                                   *
     *  author    : Milenko Mitrovic                                     *
     *  email     : dcoder@dsp-worx.de                                   *
     *  web       : http://dsp-worx.de                                   *
     *  date      : 24-07-2003                                           *
     *                                                                   *
     *  The contents of this file are used with permission, subject to   *
     *  the Mozilla Public License Version 1.1 (the "License"); you may  *
     *  not use this file except in compliance with the License. You may *
     *  obtain a copy of the License at                                  *
     *  http://www.mozilla.org/MPL/MPL-1.1.html                          *
     *                                                                   *
     *  Software distributed under the License is distributed on an      *
     *  "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or   *
     *  implied. See the License for the specific language governing     *
     *  rights and limitations under the License.                        *
     *                                                                   *
     *  (C) 2003, 2004 Milenko Mitrovic <dcoder@dsp-worx.de>             *
     *                                                                   *
     *********************************************************************)

unit Utils;

{$I Compiler.inc}

interface

uses
  Windows, DirectShow9, ActiveX, SysUtils, DCDSPTypes, Registry, Forms,
{$IFDEF WITH_PROPERTYPAGES}
  ComCtrls, 
{$ENDIF}
  Dialogs, BaseClass, Controls, CommCtrl, dspConst, dmoConst;

{$IFDEF DEBUG}
  procedure dbg(Instance : integer; Text : String);
{$ENDIF}
{$IFDEF WITH_PROPERTYPAGES}
  procedure UpdateTrackbars(Form: TForm);
{$ENDIF}
{$IFDEF WITH_WINAMP}
  function GetWinampDir : String;
  procedure ShowWinampDSPWindow(Name : String; Show : integer);
{$ENDIF}

implementation

{$IFDEF DEBUG}
procedure dbg(Instance : integer; Text : String);
begin
  OutputDebugString(PChar('DC-DSP Filter (' + inttostr(Instance) + ') : ' + Text));
end;
{$ENDIF}

{$IFDEF WITH_WINAMP}
function GetWinampDir : String;

  procedure delchar(var str : string;ch : char);
  var
    i : integer;
  begin
    i := 0;
    repeat
      inc(i);
      if str[i] = ch then delete(str,i,1);
    until i >= length(str);
  end;

var
  Reg : TRegistry;
begin
  Reg := TRegistry.Create;
  Result := '';
  Reg.RootKey := HKEY_LOCAL_MACHINE;
  if Reg.KeyExists('SOFTWARE\Microsoft\Windows\CurrentVersion\App Paths\Winamp.exe') then
  begin
    Reg.OpenKeyReadOnly('SOFTWARE\Microsoft\Windows\CurrentVersion\App Paths\Winamp.exe');
    Result := ExtractFilePath(Reg.ReadString('')) + 'Plugins\';
    if not DirectoryExists(Result) then Result := '';
    Reg.CloseKey;
    Reg.Free;
    Exit;
  end;

  if Reg.KeyExists('SOFTWARE\Classes\Winamp.File\shell\Play\command') then
  begin
    Reg.OpenKeyReadOnly('SOFTWARE\Classes\Winamp.File\shell\Play\command');
    Result := Reg.ReadString('');
    if Length(Result) > 6 then
    begin
      Delete(Result,Length(Result)-5,5);
      DelChar(result,'"');
      Result := ExtractFilePath(Result) + 'Plugins\';
    end;
    if not DirectoryExists(Result) then Result := '';
    Reg.CloseKey;
    Reg.Free;
    Exit;
  end;

  if Reg.KeyExists('SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\Winamp') then
  begin
    Reg.OpenKeyReadOnly('SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\Winamp');
    Result := Reg.ReadString('UninstallString');
    if Length(Result) > 10 then
    begin
      Delete(Result,Length(Result)-10,10);
      DelChar(result,'"');
      Result := ExtractFilePath(Result) + 'Plugins\';
    end;
    if not DirectoryExists(Result) then Result := '';
    Reg.CloseKey;
    Reg.Free;
    Exit;
  end;

  Reg.RootKey := HKEY_CLASSES_ROOT;
  if Reg.KeyExists('Applications\Winamp.File\shell\Play\command') then
  begin
    Reg.OpenKeyReadOnly('Applications\Winamp.File\shell\Play\command');
    Result := Reg.ReadString('');
    if Length(Result) > 6 then
    begin
      Delete(Result,Length(Result)-5,5);
      DelChar(result,'"');
      Result := ExtractFilePath(Result) + 'Plugins\';
    end;
    if not DirectoryExists(Result) then Result := '';
    Reg.CloseKey;
    Reg.Free;
    Exit;
  end;
end;

procedure ShowWinampDSPWindow(Name : String; Show : integer);

  function GetFirstToken (Value : String) : String;
  begin
    Result := System.copy(Value,1,Pos(',',Value)-1);
  end;

  function GetSecondToken (Value : String) : String;
  var
    z : integer;
  begin
    z := Pos(',',Value);
    Result := System.copy(Value,z+1,(Length(Value) - z));
  end;

var
  sClass, sWindow : String;
  pClass, pWindow : PChar;
  wnd : hWnd;
begin
  sClass := GetFirstToken(Name);
  sWindow := GetSecondToken(Name);
  if Length(sClass) = 0 then pClass := nil else pClass := PChar(sClass);
  if Length(sWindow) = 0 then pWindow := nil else pWindow := PChar(sWindow);
  wnd := FindWindow(pClass,pWindow);
  if wnd > 0 then ShowWindow(wnd,Show);
end;
{$ENDIF}
{$IFDEF WITH_PROPERTYPAGES}
procedure UpdateTrackbars(Form: TForm);
var
  i: integer;
begin
  for i := 0 to Form.ComponentCount -1 do
    if (Form.Components[i].InheritsFrom(TTrackBar)) then
      SetWindowLong(TTrackBar(Form.Components[i]).Handle, GWL_STYLE,
                    GetWindowLong(TTrackBar(Form.Components[i]).Handle,
                                  GWL_STYLE) and not TBS_ENABLESELRANGE);
end;
{$ENDIF}
end.
