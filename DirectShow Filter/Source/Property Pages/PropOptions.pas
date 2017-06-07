
    (*********************************************************************
     *  PropOptions.pas                                                  *
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

unit PropOptions;

interface

{$I Compiler.inc}

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls, StdCtrls,
  Forms, BaseClass, DirectShow9, ControlDCDSPFilter, ComCtrls, Buttons,
  ExtCtrls, Utils, Registry, DCDSPTypes, Dialogs, dspConst;

type
  TFormPropOptions = class(TFormPropertyPage)
    TabControl1: TTabControl;
    GroupBox1: TGroupBox;
    chkVisuals: TCheckBox;
    chkInstance: TCheckBox;
    chkTrayicon: TCheckBox;
    cbROT: TCheckBox;
    cbBalloonHint: TCheckBox;
    CheckBox1: TCheckBox;
    GroupBox3: TGroupBox;
    Label1: TLabel;
    Label3: TLabel;
    SpeedButton1: TSpeedButton;
    tbStreamDelay: TTrackBar;
    chkStreamDelay: TCheckBox;
    GroupBox2: TGroupBox;
    chkEnableStreamSwitching: TCheckBox;
    chkStreamInterface: TCheckBox;
    chkForceReconnect: TCheckBox;
    chkForceStopFilter: TCheckBox;
    GroupBox5: TGroupBox;
    cmbStreams: TComboBox;
    GroupBox4: TGroupBox;
    Label4: TLabel;
    chkEnableBitRate: TCheckBox;
    cmbBitRate: TComboBox;
    chkBitRateBeforeDSP: TCheckBox;
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure chkVisualsClick(Sender: TObject);
    procedure tbStreamDelayChange(Sender: TObject);
    procedure chkStreamDelayClick(Sender: TObject);
    procedure chkEnableStreamSwitchingClick(Sender: TObject);
    procedure cmbStreamsChange(Sender: TObject);
    procedure chkStreamInterfaceClick(Sender: TObject);
    procedure chkInstanceClick(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure chkEnableBitRateClick(Sender: TObject);
    procedure chkBitRateBeforeDSPClick(Sender: TObject);
    procedure cmbBitRateChange(Sender: TObject);
    procedure chkForceStopFilterClick(Sender: TObject);
    procedure chkForceReconnectClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure chkTrayiconClick(Sender: TObject);
    procedure cbROTClick(Sender: TObject);
    procedure cbBalloonHintClick(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
  private
    fDCDSPFilter : TDCDSPFilterControl;
    iSS : IAMStreamSelect;
    CanChange: Boolean;
  public
    function OnConnect(Unknown: IUnknown): HRESULT; override;
  end;

implementation

{$R *.DFM}

function TFormPropOptions.OnConnect(Unknown: IUnKnown): HRESULT;
var
  i : integer;
  c : Cardinal;
  pmt : PAMMediaType;
  Flags : Cardinal;
  lcid : Cardinal;
  Group : Cardinal;
  Name : PWideChar;
  unk1, unk2 : IUnknown;
  CurIndex : Cardinal;
begin
  fDCDSPFilter := TDCDSPFilterControl.Create(nil);
  Unknown.QueryInterface(IID_IAMStreamSelect,iSS);
  fDCDSPFilter.DCDSPFilter := Unknown;
  result := NOERROR;
  chkVisuals.Checked  := not fDCDSPFilter.VISafterDSP;
  CheckBox1.Checked := fDCDSPFilter.EnableVisualBuffering;
  tbStreamDelay.Position := fDCDSPFilter.AudioDelay div 2;
  tbStreamDelayChange(Self);
  chkStreamDelay.Checked := fDCDSPFilter.EnableAudioDelay;
  chkEnableStreamSwitching.Checked := fDCDSPFilter.EnableStreamSwitching;
  cmbStreams.Clear;
  chkStreamInterface.Checked := fDCDSPFilter.EnableStreamSwitchingInterface;
  chkInstance.Checked := fDCDSPFilter.EnableStreamLimitInstance;
  CanChange := False;
  if Assigned(iSS) then
  begin
    iSS.Count(c);
    if c > 0 then
    begin
      CurIndex := 0;
      for i := 0 to c -1 do
      begin
        iSS.Info(i,pmt,Flags,lcid,Group,Name,unk1,unk2);
        if Flags and AMSTREAMSELECTINFO_ENABLED <> 0 then CurIndex := i;
        cmbStreams.Items.Add(Name);
      end;
      if cmbStreams.Items.Count > 0 then cmbStreams.ItemIndex := CurIndex;
    end;
  end;
  chkEnableBitRate.Checked := fDCDSPFilter.EnableBitrateConversion;
  chkBitRateBeforeDSP.Checked := fDCDSPFilter.BitrateConversionBeforeDSP;
  cmbBitRate.ItemIndex := integer(fDCDSPFilter.BitrateConversionBits);
  chkForceStopFilter.Checked := fDCDSPFilter.EnableStreamSwitchingForceStopFilter;
  chkForceReconnect.Checked := fDCDSPFilter.EnableStreamSwitchingForceDisconnect;
  cbBalloonHint.Checked := fDCDSPFilter.EnableBalloonHint;
  chkTrayicon.Checked := fDCDSPFilter.TrayIconVisible;
  cbROT.Checked := fDCDSPFilter.EnableRunningObjectTable;
  CanChange := True;
end;

procedure TFormPropOptions.FormDestroy(Sender: TObject);
begin
  fDCDSPFilter.Free;
end;

procedure TFormPropOptions.FormCreate(Sender: TObject);
begin
  ClientWidth := PPWidth;
  ClientHeight := PPHeight;
{$IFNDEF WITH_TRAYICON}
  chkTrayicon.Visible := False;
{$ENDIF}
end;

procedure TFormPropOptions.chkVisualsClick(Sender: TObject);
begin
  fDCDSPFilter.VISafterDSP := not chkVisuals.Checked;
end;

procedure TFormPropOptions.tbStreamDelayChange(Sender: TObject);
begin
  label1.Caption := Format('%.3f sec', [tbStreamDelay.Position * 2 / 1000]);
  fDCDSPFilter.AudioDelay := tbStreamDelay.Position * 2;
end;

procedure TFormPropOptions.chkStreamDelayClick(Sender: TObject);
begin
  fDCDSPFilter.EnableAudioDelay := chkStreamDelay.Checked;
end;

procedure TFormPropOptions.chkEnableStreamSwitchingClick(Sender: TObject);
begin
  fDCDSPFilter.EnableStreamSwitching := chkEnableStreamSwitching.Checked;
end;

procedure TFormPropOptions.cmbStreamsChange(Sender: TObject);
begin
  if cmbStreams.ItemIndex < 0 then Exit;
  if Assigned(iSS) then iSS.Enable(cmbStreams.ItemIndex,AMSTREAMSELECTENABLE_ENABLE)
end;

procedure TFormPropOptions.chkStreamInterfaceClick(Sender: TObject);
begin
  fDCDSPFilter.EnableStreamSwitchingInterface := chkStreamInterface.Checked;
end;

procedure TFormPropOptions.chkInstanceClick(Sender: TObject);
begin
  fDCDSPFilter.EnableStreamLimitInstance := chkInstance.Checked;
end;

procedure TFormPropOptions.SpeedButton1Click(Sender: TObject);
begin
  tbStreamDelay.Position := 0;
  tbStreamDelayChange(Self);
end;

procedure TFormPropOptions.chkEnableBitRateClick(Sender: TObject);
begin
  if CanChange then fDCDSPFilter.EnableBitrateConversion := chkEnableBitRate.Checked;
end;

procedure TFormPropOptions.chkBitRateBeforeDSPClick(Sender: TObject);
begin
  if CanChange then fDCDSPFilter.BitrateConversionBeforeDSP := chkBitRateBeforeDSP.Checked;
end;

procedure TFormPropOptions.cmbBitRateChange(Sender: TObject);
begin
  if CanChange then fDCDSPFilter.BitrateConversionBits := TDCBitRate(cmbBitRate.ItemIndex);
end;

procedure TFormPropOptions.chkForceStopFilterClick(Sender: TObject);
begin
  if CanChange then fDCDSPFilter.EnableStreamSwitchingForceStopFilter := chkForceStopFilter.Checked;
end;

procedure TFormPropOptions.chkForceReconnectClick(Sender: TObject);
begin
  if CanChange then fDCDSPFilter.EnableStreamSwitchingForceDisconnect := chkForceReconnect.Checked;
end;

procedure TFormPropOptions.FormShow(Sender: TObject);
begin
  UpdateTrackbars(Self);
end;

procedure TFormPropOptions.chkTrayiconClick(Sender: TObject);
begin
  fDCDSPFilter.TrayIconVisible := chkTrayicon.Checked;
end;

procedure TFormPropOptions.cbROTClick(Sender: TObject);
begin
  fDCDSPFilter.EnableRunningObjectTable := cbROT.Checked;
end;

procedure TFormPropOptions.cbBalloonHintClick(Sender: TObject);
begin
  fDCDSPFilter.EnableBalloonHint := cbBalloonHint.Checked;
end;

procedure TFormPropOptions.CheckBox1Click(Sender: TObject);
begin
  fDCDSPFilter.EnableVisualBuffering := CheckBox1.Checked;
end;

initialization

  TBCClassFactory.CreatePropertyPage(TFormPropOptions, CLSID_DCDSPFilterPropertyPageOptions);

end.
