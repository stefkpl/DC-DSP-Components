
    (*********************************************************************
     *  PropAbout.pas                                                    *
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

unit PropAbout;

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls, StdCtrls,
  ExtCtrls, Forms, BaseClass, ComObj, StdVcl, AxCtrls, DirectShow9, Dialogs,
  ShellAPI, Utils, DCDSPTypes, ControlDCDSPFilter, dspConst, dspUtils,
  pngimage, ComCtrls, FastMM4;

type
  TFormPropAbout = class(TFormPropertyPage)
    Timer1: TTimer;
    TabControl1: TTabControl;
    Label14: TLabel;
    GroupBox1: TGroupBox;
    Label2: TLabel;
    GroupBox2: TGroupBox;
    Label8: TLabel;
    Label9: TLabel;
    Label12: TLabel;
    GroupBox3: TGroupBox;
    Label10: TLabel;
    GroupBox4: TGroupBox;
    Label3: TLabel;
    GroupBox5: TGroupBox;
    Label11: TLabel;
    GroupBox6: TGroupBox;
    label6: TLabel;
    Label1: TLabel;
    Label5: TLabel;
    Label7: TLabel;
    label4: TLabel;
    statictext1: TLabel;
    Label13: TLabel;
    Image2: TImage;
    procedure Label4Click(Sender: TObject);
    procedure Label4MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure Label1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure StaticText1Click(Sender: TObject);
    procedure StaticText1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    fDCDSPFilter : TDCDSPFilterControl;
  public
    function OnConnect(Unknown: IUnknown): HRESULT; override;
  end;

implementation

{$R *.DFM}

function ReplaceChar(str : string; ch, ch2 : char) : String;
var
  i : integer;
begin
  i :=0;
  repeat
    inc(i);
    if str[i] = ch then str[i] := ch2;
  until i >= length(str);
  Result := str;
end;

function TFormPropAbout.OnConnect(Unknown: IUnKnown): HRESULT;
begin
  fDCDSPFilter := TDCDSPFilterControl.Create(nil);
  fDCDSPFilter.DCDSPFilter := Unknown;
  result := NOERROR;
end;

procedure TFormPropAbout.Label4Click(Sender: TObject);
var
  tmp : String;
  z : integer;
  str : String;
begin
  z := FilterVersion - (FilterVersion div 10 * 10);
  if (z > 0) and (z < 8) then tmp := ' Beta ' + inttostr(z)
  else if (z >= 8) then tmp := ' RC ' + inttostr(z - 7)
  else tmp := ' Final';
  str := 'mailto:dcoder@dsp-worx.de?subject=DC-DSP Filter v' + ReplaceChar(Format('%.2f', [(FilterVersion - z) / 1000]),',','.') + tmp + ' (' + FilterReleaseDate + ')';
  ShellExecute(0,'open',PChar(str),nil,nil,SW_SHOWNORMAL);
end;

procedure TFormPropAbout.Label4MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  StaticText1.Font := label5.Font;
  label4.Font.Color := clHotLight;
  label4.Font.Style := [fsUnderline];
end;

procedure TFormPropAbout.Label1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  StaticText1.Font := label5.Font;
  label4.Font := label5.Font;
end;

procedure TFormPropAbout.FormCreate(Sender: TObject);
begin
  ClientWidth := PPWidth;
  ClientHeight := PPHeight;
  if has3DNowExt then label13.Caption := 'Using enhanced 3DNow! Instructions'
  else if has3DNow then label13.Caption := 'Using 3DNow! Instructions'
  else if hasSSE then label13.Caption := 'Using SSE Instructions'
  else label13.Caption := 'Using FPU Instructions';
end;

procedure TFormPropAbout.FormShow(Sender: TObject);
var
  tmp : String;
  z : integer;
//  wnd : hWnd;
begin
//  wnd := GetWindowLong(ParentWindow,GWL_HWNDPARENT);
//  WndProcOld := Pointer(GetWindowLong(wnd,GWL_WNDPROC));
//  SetWindowLong(wnd,GWL_WNDPROC,integer(@WndProcHook));

  z := FilterVersion - (FilterVersion div 10 * 10);
  if (z > 0) and (z < 8) then tmp := ' Beta ' + inttostr(z)
  else if (z >= 8) then tmp := ' RC ' + inttostr(z - 7)
  else tmp := ' Final';
  label6.caption := 'DC-DSP Filter v' + ReplaceChar(Format('%.2f', [(FilterVersion - z) / 1000]),',','.') + tmp + ' (Build on ' + FilterReleaseDate + ')';
  Timer1Timer(Self);
end;

procedure TFormPropAbout.Timer1Timer(Sender: TObject);
var
  Stream : PDSStream;
  tmp : String;
  tmp2 : String;
  state: TMemoryManagerState;
  s: Int64;
  i: Integer;
begin
try
  GetMemoryManagerState(state);

  s := state.TotalAllocatedMediumBlockSize;
  s := s + state.TotalAllocatedLargeBlockSize;

  for i := 0 to high(state.SmallBlockTypeStates) do
  begin
    with state.SmallBlockTypeStates[i] do
    begin
      Inc(s, AllocatedBlockCount * UseableBlockSize);
    end;
  end;

  label3.caption := Format('%.2f MB', [s / 1024 / 1024]);
  label3.Repaint;
  label11.caption := Format('%.2f', [fDCDSPFilter.CPUUsage * 100]) + ' %';
  label11.Repaint;
  Stream := fDCDSPFilter.StreamInfo;
  if Stream.Channels > 1 then tmp := 's' else tmp := '';
  if Stream.Float then tmp2 := 'Float' else tmp2 := 'Integer';
  tmp2 := inttostr(Stream.Frequency) + 'Hz - ' + inttostr(Stream.Bits) + ' Bit (' +  tmp2 + ') - ' + inttostr(Stream.Channels) + ' Channel' + tmp;
  if Stream.SPDIF then tmp2 := tmp2 + ' - SPDIF (bypassed)';
  if Stream.DTS then tmp2 := tmp2 + ' - DTS (bypassed)';
  label10.Caption := tmp2;
  label10.Repaint;
except
  label2.Caption := DateTimeToStr(now);
end;  
end;

procedure TFormPropAbout.FormDestroy(Sender: TObject);
begin
  fDCDSPFilter.Free;
end;

procedure TFormPropAbout.StaticText1Click(Sender: TObject);
begin
  ShellExecute(0,'open','http://www.dsp-worx.de',nil,nil,SW_SHOWNORMAL);
end;

procedure TFormPropAbout.StaticText1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  label4.Font := label5.Font;
  StaticText1.Font.Color := clHotLight;
  StaticText1.Font.Style := [fsUnderline];
end;

procedure TFormPropAbout.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
 Timer1.Enabled := False;
end;

initialization
  TBCClassFactory.CreatePropertyPage(TFormPropAbout, CLSID_DCDSPFilterPropertyPageAbout);
end.
