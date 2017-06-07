
    (*********************************************************************
     *  PropDSP.pas                                                      *
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

unit PropDSP;

{$I Compiler.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Math,
  Dialogs, Menus, StdCtrls, CheckLst, ExtCtrls, BaseClass, 
  ControlDCDSPFilter, DCDSPTypes, Utils, AlphaBlend, DirectShow9, dspConst, Registry,

  {$IFDEF WITH_INTERNAL_DSP} DynamicFilterList, {$ENDIF}

  formDSPAmplify, formDSPBandPass, formDSPChannelOrder, formDSPCompressor, formDSPDownMix, formDSPDynamicAmplify,
  formDSPEchoDelay, formDSPEqualizer, formDSPFlanger, formDSPHighPass, formDSPLowPass, formDSPNotch, formDSPPhaseInvert,
  formDSPPhaser, formDSPPitchScale, formDSPPitchShift, formDSPSound3D, formDSPTempo, formDSPTrebleEnhancer,
  formDSPTrueBass, formDSPParametricEQ,

  dspAmplify, dspBandPass, dspChannelOrder, dspCompressor, dspDownMix, dspDynamicAmplify,
  dspEchoDelay, dspEqualizer, dspFlanger, dspHighPass, dspLowPass, dspNotch, dspPhaseInvert,
  dspPhaser, dspPitchScale, dspPitchShift, dspSound3D, dspTempo, dspTrebleEnhancer,
  dspTrueBass,

  formDMOChorus, formDMOCompressor, formDMODistortion, formDMOEcho, formDMOFlanger, formDMOGargle, formDMOI3DL2Reverb,
  formDMOParamEQ, formDMOWavesReverb,

  dmoChorus, dmoCompressor, dmoDistortion, dmoEcho, dmoFlanger, dmoGargle, dmoI3DL2Reverb,
  dmoParamEQ, dmoWavesReverb, visSpectrum, ComCtrls, Buttons;

type
  TFormPropDSP = class(TFormPropertyPage)
    PopupMenu1: TPopupMenu;
    Add1: TMenuItem;
    Amplify1: TMenuItem;
    BandPass1: TMenuItem;
    ChannelOrder1: TMenuItem;
    Compressor1: TMenuItem;
    DownMix1: TMenuItem;
    DynamicAmplify1: TMenuItem;
    EchoDelay1: TMenuItem;
    Equalizer1: TMenuItem;
    Flanger2: TMenuItem;
    HighPass1: TMenuItem;
    LowPass1: TMenuItem;
    Notch1: TMenuItem;
    PhaseInvert1: TMenuItem;
    Phaser1: TMenuItem;
    PitchScale1: TMenuItem;
    PitchShift1: TMenuItem;
    Sound3D1: TMenuItem;
    empo1: TMenuItem;
    rebleEnhancer1: TMenuItem;
    rueBass1: TMenuItem;
    AddDMO1: TMenuItem;
    Chorus1: TMenuItem;
    Compressor2: TMenuItem;
    Distortion1: TMenuItem;
    Echo1: TMenuItem;
    Flanger1: TMenuItem;
    Gargle1: TMenuItem;
    I3DL2Reverb1: TMenuItem;
    ParamEQ1: TMenuItem;
    WavesReverb1: TMenuItem;
    N1: TMenuItem;
    Remove2: TMenuItem;
    Remove1: TMenuItem;
    RemoveAll1: TMenuItem;
    Move1: TMenuItem;
    MoveUp1: TMenuItem;
    MoveDown1: TMenuItem;
    N2: TMenuItem;
    Preset1: TMenuItem;
    Add2: TMenuItem;
    Delete1: TMenuItem;
    LoadAll1: TMenuItem;
    N3: TMenuItem;
    N4: TMenuItem;
    ShowControls1: TMenuItem;
    N5: TMenuItem;
    EnableAll1: TMenuItem;
    DisableAll1: TMenuItem;
    DCSpectrum: TDCSpectrum;
    PopupMenu2: TPopupMenu;
    SetStartColor1: TMenuItem;
    SetEndColor1: TMenuItem;
    ColorDialog1: TColorDialog;
    Bar1: TMenuItem;
    Back1: TMenuItem;
    SetBackColor1: TMenuItem;
    SetTopColor1: TMenuItem;
    SetBackgroundColor1: TMenuItem;
    N6: TMenuItem;
    SetDefaultStyle1: TMenuItem;
    Enabled1: TMenuItem;
    N7: TMenuItem;
    N8: TMenuItem;
    Equalizer21: TMenuItem;
    tmrVisual: TTimer;
    TabControl1: TTabControl;
    GroupBox2: TGroupBox;
    lbFilters: TCheckListBox;
    GroupBox3: TGroupBox;
    btnSavePreset: TSpeedButton;
    cmbPreset: TComboBox;
    pnlControlWindow: TPanel;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    GroupBox4: TGroupBox;
    tbFalloff: TTrackBar;
    pnlVisual: TPanel;
    Shape1: TShape;
    pbVisual: TPaintBox;
    tbVisual: TTrackBar;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure AddFilter(Sender: TObject);
    procedure Remove1Click(Sender: TObject);
    procedure PopupMenu1Popup(Sender: TObject);
    procedure lbFiltersClickCheck(Sender: TObject);
    procedure RemoveAll1Click(Sender: TObject);
    procedure MoveUp1Click(Sender: TObject);
    procedure MoveDown1Click(Sender: TObject);
    procedure lbFiltersMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure lbFiltersMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure lbFiltersMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure lbFiltersKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ShowControls1Click(Sender: TObject);
    procedure LoadAll1Click(Sender: TObject);
    procedure EnableAll1Click(Sender: TObject);
    procedure DisableAll1Click(Sender: TObject);
    procedure tmrVisualTimer(Sender: TObject);
    procedure DCSpectrumSpectrumData(Sender: TObject; Data: PVisualBuffer;
      MinY, MaxY, NumSamples, Channels: Integer);
    procedure pbVisualMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure tbVisualChange(Sender: TObject);
    procedure Add2Click(Sender: TObject);
    procedure SetStartColor1Click(Sender: TObject);
    procedure SetEndColor1Click(Sender: TObject);
    procedure SetBackgroundColor1Click(Sender: TObject);
    procedure SetTopColor1Click(Sender: TObject);
    procedure SetBackColor1Click(Sender: TObject);
    procedure SetDefaultStyle1Click(Sender: TObject);
    procedure PopupMenu2Popup(Sender: TObject);
    procedure Enabled1Click(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure lbFiltersClick(Sender: TObject);
    procedure cmbPresetChange(Sender: TObject);
    procedure btnSavePresetClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FBackBMP: TBitmap;
    FFadeBMP: TBitmap;

    Bar : TBitmap;
    Back : TBitmap;

    fDCDSPFilter : TDCDSPFilterControl;
    fMouseDown : Boolean;
    fIndex : integer;
    fStopped : Boolean;

    FFallOffs: array[0..37] of Single;
    FPeaks: array[0..37] of Single;
    FPeaksWait: array[0..37] of Single;

    fBarStart, fBarEnd,
    fBackStart, fBackEnd, fBack : TColor;
    fCanChange: Boolean;
    fPreset: String;
    procedure UpdateList;
    procedure CloseAllWindows;
    procedure SetVis(Mode : Boolean);
    procedure OnLoadPreset(Sender : TObject);
    procedure OnDeletePreset(Sender : TObject);
    procedure UpdateBar;
    procedure UpdatePresets(Preset: String);
  public
    function OnConnect(Unknown: IUnknown): HRESULT; override;
    function OnDisconnect: HRESULT; override;
  end;

implementation

{$R *.DFM}

function TFormPropDSP.OnConnect(Unknown: IUnKnown): HRESULT;
begin
  fDCDSPFilter := TDCDSPFilterControl.Create(nil);
  fDCDSPFilter.DCDSPFilter := Unknown;
  fMouseDown := False;
  fIndex := 0;
  UpdateList;
  result := NOERROR;
end;

function TFormPropDSP.OnDisconnect: HRESULT;
begin
  result := NOERROR;
end;

procedure TFormPropDSP.FormCreate(Sender: TObject);
begin
  lbFilters.DoubleBuffered := True;
  fCanChange := True;

  FBackBMP := TBitmap.Create;
  FBackBMP.PixelFormat := pf24bit;
  FBackBMP.Width := pbVisual.Width;
  FBackBMP.Height := pbVisual.Height;

  FFadeBMP := TBitmap.Create;
  FFadeBMP.PixelFormat := pf24bit;
  FFadeBMP.Width := pbVisual.Width;
  FFadeBMP.Height := pbVisual.Height;

  Back := TBitmap.Create;
  Back.Width := pbVisual.Width;
  Back.Height := pbVisual.Height;
  Back.PixelFormat := pf24bit;

  Bar := TBitmap.Create;
  Bar.Width := pbVisual.Width;
  Bar.Height := pbVisual.Height;
  Bar.PixelFormat := pf24bit;

  ClientWidth := PPWidth;
  ClientHeight := PPHeight;
  label1.Caption :=
  'To add Filters to the List, right click the Listbox on the Left and choose a' +
  ' Filter from the Filters Menu. You can load as many Filters you want. Note'+
  ' that Filters like Echo/Delay and Pitch uses a lot of Memory. Use the only' +
  ' if you need them or if you have enough RAM available. The Filters are also' +
  ' divided into 2 Categorys, DSP and DMO. The DSP Filters are internal DSP' +
  ' Filters, while the DMO Filters are Wrapper Filters for Microsofts DMO'+
  ' Filter.' + #13#10#13#10 +
  'You can save certain Filter configurations as a "Preset" to the Registry. Do'+
  ' this by right click on the left Listbox and then choose Preset -> Save. The' +
  ' last configuration will be automatically saved as a "last known" Configuration'+
  ' to the Registry.' + #13#10#13#10 +
  'You will find some other usefull Tools and Configurations under the "Options"'+
  ' Tab.' + #13#10#13#10 +
  'To use Winamp 2.x DSP or Visual Plugins click on the Tabs above and specify '+
  ' the Location. Note that not all Plugins are supposed to work!';
end;

procedure TFormPropDSP.FormShow(Sender: TObject);
var
  tmp: String;
begin
  SetDefaultStyle1Click(Self);
  with TRegistry.Create do
  begin
    Rootkey := HKEY_CURRENT_USER;
    if OpenKey('SOFTWARE\DSP-Worx\DC-DSP Filter',False) then
    begin
      if ValueExists('VisStopped') then fStopped := ReadBool('VisStopped')
                                   else fStopped := False;
      if ValueExists('VisMax') then
      begin
        tbVisual.Position := ReadInteger('VisMax');
        tbVisualChange(Self);
      end;
      if ValueExists('VisFalloff') then tbFalloff.Position := ReadInteger('VisFalloff');

      if ValueExists('BarStart') then fBarStart := ReadInteger('BarStart');
      if ValueExists('BarEnd') then fBarEnd := ReadInteger('BarEnd');
      if ValueExists('BackStart') then fBackStart := ReadInteger('BackStart');
      if ValueExists('BackEnd') then fBackEnd := ReadInteger('BackEnd');
      if ValueExists('Back') then fBack := ReadInteger('Back');

      if ValueExists('fLastPPPreset') then tmp := ReadString('fLastPPPreset')
                                      else tmp := '';
      if not fDCDSPFilter.PresetExist(tmp) then tmp := '';
    end;
    Free;
  end;
  UpdateBar;

  if lbFilters.Count > 0 then
  begin
//    lbfilters.ItemIndex := 0;
//    ShowControls1Click(Self);
  end;
  UpdatePresets(tmp);
  SetVis(fStopped);
  UpdateTrackbars(Self);
  // initialize the Bitmaps one time
  FFadeBMP.Canvas.Draw(0, 0, Back);
end;

procedure TFormPropDSP.FormDestroy(Sender: TObject);
begin
  with TRegistry.Create do
  begin
    Rootkey := HKEY_CURRENT_USER;
    if OpenKey('SOFTWARE\DSP-Worx\DC-DSP Filter',True) then
    begin
      WriteString('fLastPPPreset',fPreset);
      WriteBool('VisStopped',fStopped);
      WriteInteger('VisMax',tbVisual.Position);
      WriteInteger('VisFalloff',tbFalloff.Position);
      WriteInteger('BarStart',fBarStart);
      WriteInteger('BarEnd',fBarEnd);
      WriteInteger('BackStart',fBackStart);
      WriteInteger('BackEnd',fBackEnd);
      WriteInteger('Back',fBack);
    end;
    Free;
  end;
  if not fStopped then SetVis(True);
  CloseAllWindows;
  FFadeBMP.Free;
  FBackBMP.Free;
  Bar.Free;
  Back.Free;
  fDCDSPFilter.Free;
end;

procedure TFormPropDSP.AddFilter(Sender: TObject);
begin
  if lbFilters.ItemIndex > -1 then
  begin
    fDCDSPFilter.AddFilter(lbFilters.ItemIndex + 1,TDCFilterType((Sender as TMenuItem).Tag + 1));
    lbFilters.Items.Insert(lbFilters.ItemIndex + 1,fDCDSPFilter.FilterName(lbFilters.ItemIndex + 1));
  end else
  begin
    fDCDSPFilter.AddFilter(-1,TDCFilterType((Sender as TMenuItem).Tag + 1));
    lbFilters.Items.Add(fDCDSPFilter.FilterName(fDCDSPFilter.FilterCount -1));
  end;
  UpdatePresets('');
end;

procedure TFormPropDSP.Remove1Click(Sender: TObject);
var
  i : integer;
begin
  i := lbFilters.ItemIndex;
  if fDCDSPFilter.WindowShown[i] then CloseAllWindows;
  fDCDSPFilter.DeleteFilter(lbFilters.ItemIndex);
  lbFilters.Items.Delete(lbFilters.ItemIndex);
  if lbFilters.Count > 0 then
  begin
    if i = lbFilters.Count then dec(i);
    lbFilters.ItemIndex := i;
  end;
  UpdatePresets('');
end;

procedure TFormPropDSP.PopupMenu1Popup(Sender: TObject);
var
  En : Boolean;
  Dis : Boolean;
  i : integer;
  Item : TMenuItem;
begin
  Remove1.Enabled := (lbFilters.Count > 0) and (lbFilters.ItemIndex > -1);
  RemoveAll1.Enabled := fDCDSPFilter.FilterCount > 0;
  Remove2.Enabled := Remove1.Enabled or RemoveAll1.Enabled;

  MoveUp1.Enabled := (lbFilters.Count > 0) and (lbFilters.ItemIndex > 0);
  MoveDown1.Enabled := (lbFilters.Count > 0) and (lbFilters.ItemIndex >= 0) and (lbFilters.ItemIndex < lbFilters.Count -1);
  Move1.Enabled := MoveUp1.Enabled or MoveDown1.Enabled;

  ShowControls1.Enabled := (lbFilters.Count > 0) and (lbFilters.ItemIndex > -1) and not (fDCDSPFilter.WindowShown[lbFilters.ItemIndex]);

  En := False;
  Dis := False;

  for i := 0 to fDCDSPFilter.FilterCount -1 do
  begin
    if not fDCDSPFilter.FilterEnabled[i] and not En then En := True;
    if fDCDSPFilter.FilterEnabled[i] and not Dis then Dis := True;
  end;

  EnableAll1.Enabled := En;
  DisableAll1.Enabled := Dis;

  Delete1.Clear;
  while Preset1.Count > 5 do Preset1.Delete(Preset1.Count-1);

  if fDCDSPFilter.PresetCount > 0 then
  begin
    for i := 0 to fDCDSPFilter.PresetCount -1 do
    begin
      Item := TMenuItem.Create(Delete1);
      Item.Caption := fDCDSPFilter.PresetName(i);
      Item.OnClick := OnLoadPreset;
      Preset1.Add(Item);
      Item := TMenuItem.Create(Delete1);
      Item.Caption := fDCDSPFilter.PresetName(i);
      Item.OnClick := OnDeletePreset;
      Delete1.Add(Item);
    end;
  end;

  Add2.Enabled := lbFilters.Count > 0;
  Delete1.Enabled := fDCDSPFilter.PresetCount > 0;
end;

procedure TFormPropDSP.lbFiltersClickCheck(Sender: TObject);
begin
  fDCDSPFilter.FilterEnabled[lbFilters.ItemIndex] := lbFilters.Checked[lbFilters.ItemIndex];
//  cmbPreset.ItemIndex := -1;
  fPreset := '';
end;

procedure TFormPropDSP.UpdateList;
var
  i : integer;
begin
  lbFilters.Clear;
  for i := 0 to fDCDSPFilter.FilterCount -1 do
  begin
    lbFilters.Items.Add(fDCDSPFilter.FilterName(i));
    lbFilters.Checked[i] := fDCDSPFilter.FilterEnabled[i];
  end;
end;

procedure TFormPropDSP.RemoveAll1Click(Sender: TObject);
begin
  CloseAllWindows;
  fDCDSPFilter.RemoveAllFilters;
  lbFilters.Clear;
  UpdatePresets('');
end;

procedure TFormPropDSP.MoveUp1Click(Sender: TObject);
var
  i : integer;
begin
  i := lbFilters.ItemIndex;
  fDCDSPFilter.MoveFilter(i,i-1);
  lbFilters.Items.Move(i - 1,i);
  lbFilters.Selected[i - 1] := True;
//  cmbPreset.ItemIndex := -1;
  fPreset := '';
end;

procedure TFormPropDSP.MoveDown1Click(Sender: TObject);
var
  i : integer;
begin
  i := lbFilters.ItemIndex;
  fDCDSPFilter.MoveFilter(i,i+1);
  lbFilters.Items.Move(i + 1,i);
  lbFilters.Selected[i + 1] := True;
//  cmbPreset.ItemIndex := -1;
  fPreset := '';
end;

procedure TFormPropDSP.CloseAllWindows;
begin
  fDCDSPFilter.ResetShownWindows;
  if frmDSPAmplify  <> nil then FreeAndNil(frmDSPAmplify);
  if frmDSPBandPass <> nil then FreeAndNil(frmDSPBandPass);
  if frmDSPChannelOrder <> nil then FreeAndNil(frmDSPChannelOrder);
  if frmDSPCompressor <> nil then FreeAndNil(frmDSPCompressor);
  if frmDSPDownMix <> nil then FreeAndNil(frmDSPDownMix);
  if frmDSPDynamicAmplify <> nil then FreeAndNil(frmDSPDynamicAmplify);
  if frmDSPEchoDelay <> nil then FreeAndNil(frmDSPEchoDelay);
  if frmDSPEqualizer <> nil then FreeAndNil(frmDSPEqualizer);
  if frmDSPFlanger <> nil then FreeAndNil(frmDSPFlanger);
  if frmDSPHighPass <> nil then FreeAndNil(frmDSPHighPass);
  if frmDSPLowPass <> nil then FreeAndNil(frmDSPLowPass);
  if frmDSPNotch <> nil then FreeAndNil(frmDSPNotch);
  if frmDSPParametricEQ <> nil then FreeAndNil(frmDSPParametricEQ);
  if frmDSPPhaseInvert <> nil then FreeAndNil(frmDSPPhaseInvert);
  if frmDSPPhaser <> nil then FreeAndNil(frmDSPPhaser);
  if frmDSPPitchScale <> nil then FreeAndNil(frmDSPPitchScale);
  if frmDSPPitchShift <> nil then FreeAndNil(frmDSPPitchShift);
  if frmDSPSound3D <> nil then FreeAndNil(frmDSPSound3D);
  if frmDSPTempo <> nil then FreeAndNil(frmDSPTempo);
  if frmDSPTrebleEnhancer <> nil then FreeAndNil(frmDSPTrebleEnhancer);
  if frmDSPTrueBass <> nil then FreeAndNil(frmDSPTrueBass);
  if frmDMOChorus <> nil then FreeAndNil(frmDMOChorus);
  if frmDMOCompressor <> nil then FreeAndNil(frmDMOCompressor);
  if frmDMODistortion <> nil then FreeAndNil(frmDMODistortion);
  if frmDMOEcho <> nil then FreeAndNil(frmDMOEcho);
  if frmDMOFlanger <> nil then FreeAndNil(frmDMOFlanger);
  if frmDMOGargle <> nil then FreeAndNil(frmDMOGargle);
  if frmDMOI3DL2Reverb <> nil then FreeAndNil(frmDMOI3DL2Reverb);
  if frmDMOParamEQ <> nil then FreeAndNil(frmDMOParamEQ);
  if frmDMOWavesReverb <> nil then FreeAndNil(frmDMOWavesReverb);
end;

procedure TFormPropDSP.lbFiltersMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  i : integer;
begin
  if Button = mbRight then
  begin
    i := lbFilters.ItemAtPos(Point(X,Y),True);
    if i > -1 then
    begin
      lbFilters.ItemIndex := i;
    end;
  end else
  if Button = mbLeft then
  begin
    fIndex := lbFilters.ItemAtPos(Point(X,Y),True);
    fMouseDown := True;
  end;
end;

procedure TFormPropDSP.lbFiltersMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  fMouseDown := False;
end;

procedure TFormPropDSP.lbFiltersMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  i : integer;
begin
  if fMouseDown then
  begin
    i := lbFilters.ItemAtPos(Point(X,Y),True);
    if (i > -1) and (fIndex > -1) and (i <> fIndex) then
    begin
      lbFilters.Items.Move(fIndex,i);
      lbFilters.ItemIndex := i;
      fDCDSPFilter.MoveFilter(fIndex,i);
      fIndex := i;
//      cmbPreset.ItemIndex := -1;
      fPreset := '';
    end;
  end;
end;

procedure TFormPropDSP.lbFiltersKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  i : integer;
begin
  if ssCtrl in Shift then
  begin
    case Key of
      VK_UP:
      begin
        if (lbFilters.Count > 0) and (lbFilters.ItemIndex > 0) then
        begin
          i := lbFilters.ItemIndex;
          MoveUp1Click(Self);
          lbFilters.ItemIndex := i;
        end;
      end;
      VK_DOWN:
      begin
        if (lbFilters.Count > 0) and (lbFilters.ItemIndex >= 0) and (lbFilters.ItemIndex < lbFilters.Count -1) then
        begin
          i := lbFilters.ItemIndex;
          MoveDown1Click(Self);
          lbFilters.ItemIndex := i;
        end;
      end;
    end;
  end else
  begin
    case Key of
      VK_DELETE:
      begin
        if (lbFilters.ItemIndex > -1) then
        begin
          i := lbFilters.ItemIndex;
          Remove1Click(Self);
          if (i > lbFilters.Count -1)
            then i := lbFilters.Count -1;
          if (i < 0)
            then i := 0;
          if (i < lbFilters.Count)
            then lbFilters.ItemIndex := i;
        end;
      end;
      VK_INSERT:
      begin
        if (lbFilters.Count > 0) and (lbFilters.ItemIndex >= 0) then
        begin
          lbFiltersClick(Self);
        end;
      end;
    end;
  end;
end;

procedure TFormPropDSP.ShowControls1Click(Sender: TObject);
begin
  lbFiltersClick(Self);
end;

procedure TFormPropDSP.LoadAll1Click(Sender: TObject);
var
  i : integer;
begin
  RemoveAll1Click(Self);
  for i := 0 to 28 do
  begin
    fDCDSPFilter.AddFilter(-1,TDCFilterType(i+1));
    lbFilters.Items.Add(fDCDSPFilter.FilterName(fDCDSPFilter.FilterCount -1));
  end;
end;

procedure TFormPropDSP.EnableAll1Click(Sender: TObject);
var
  i : integer;
begin
  for i := 0 to fDCDSPFilter.FilterCount -1 do
  begin
    lbFilters.Checked[i] := True;
    fDCDSPFilter.FilterEnabled[i] := True;
  end;
//  cmbPreset.ItemIndex := -1;
  fPreset := '';
end;

procedure TFormPropDSP.DisableAll1Click(Sender: TObject);
var
  i : integer;
begin
  for i := 0 to fDCDSPFilter.FilterCount -1 do
  begin
    lbFilters.Checked[i] := False;
    fDCDSPFilter.FilterEnabled[i] := False;
  end;
//  cmbPreset.ItemIndex := -1;
  fPreset := '';
end;

procedure TFormPropDSP.tmrVisualTimer(Sender: TObject);
var
  Buffer : Pointer;
  Stream : PDSStream;
  Size : integer;
begin
  if fStopped then Exit;
  try
    if fDCDSPFilter.Get_VisualData(Buffer,Size,Stream) then
    begin
      DCSpectrum.Process(Buffer,Size,Stream.Bits,Stream.Channels,Stream.Float);
    end else
    begin
      DCSpectrum.Flush;
      DCSpectrumSpectrumData(Self, @DCSpectrum.Buffer, 0, 0, 0, 0);
    end;
  except
  end;
end;

procedure TFormPropDSP.DCSpectrumSpectrumData(Sender: TObject;
  Data: PVisualBuffer; MinY, MaxY, NumSamples, Channels : Integer);

  function GetBar(Val : integer) : integer;
  begin
    Result := Val - (Val mod 2) + 2;
  end;

const
  VisualData : array[0..38] of WORD = (
    1,   4,   9,   13,  18,  25,  30,  40,  50,  60,
    70,  80,  85,  90,  100, 105, 110, 115, 120, 130,
    140, 150, 160, 180, 200, 210, 230, 250, 260, 270,
    280, 290, 300, 340, 360, 400, 430, 460, 512
  );
var
  i, z : integer;
  value: integer;
begin
  FBackBMP.Canvas.Draw(0, 0, FFadeBMP);

  for i := 0 to 37 do
  begin
    value := 0;
    for z := VisualData[i] to VisualData[i+1]-1 do
    begin
      if (Data[0,z] > value)
        then value := Data[0,z];
    end;

    value := value + 2;

    if (value <= 0)
      then value := 0
    else if (value > pbVisual.Height -4)
      then value := pbVisual.Height -4;

    FFallOffs[i] := FFallOffs[i] - tbFalloff.Position / 2;

    if (value > FFallOffs[i])
      then FFallOffs[i] := value
      else value := Round(FFallOffs[i]);
    BitBlt(FBackBMP.Canvas.Handle, (i * 8) + 2, pbVisual.Height - value - 2,
           6, pbVisual.Height, Bar.Canvas.Handle, 2, pbVisual.Height - value - 2, SRCCOPY);

    FPeaksWait[i] := FPeaksWait[i] + 0.3;

    if (FPeaksWait[i] > 8.0)
      then FPeaks[i] := FPeaks[i] - tbFalloff.Position / 4;

    value := Round(FPeaks[i]);

    if (value <= 0)
      then value := 0
    else if (value > pbVisual.Height - 4)
      then value := pbVisual.Height - 4;

    if (FFallOffs[i] < FPeaks[i]) then
    begin
      BitBlt(FBackBMP.Canvas.Handle, (i * 8) + 2, pbVisual.Height - value - 2,
             6, 2, Bar.Canvas.Handle, 2, pbVisual.Height - value - 2, SRCCOPY);
    end else
    begin
      FPeaks[i] := FFallOffs[i];
      FPeaksWait[i] := 0;
    end;

  end;

  Blendit(FBackBMP, Back, FFadeBMP, 5);
  pbVisual.Canvas.Draw(0, 0, FBackBMP);
end;

procedure TFormPropDSP.pbVisualMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    fStopped := not fStopped;
    SetVis(fStopped);
  end;
end;

procedure TFormPropDSP.SetVis(Mode : Boolean);
begin
  if Mode then
  begin
    tmrVisual.Enabled := False;
    pbVisual.Visible := False;
  end else
  begin
    tmrVisual.Enabled := True;
    pbVisual.Visible := True;
  end;
end;

procedure TFormPropDSP.tbVisualChange(Sender: TObject);
begin
  DCSpectrum.MaxY := Round(Log10((tbVisual.Position+1) / 1000) * -333.34)+1;
end;

procedure TFormPropDSP.OnLoadPreset(Sender : TObject);
begin
  fDCDSPFilter.LoadPreset((Sender as TMenuItem).Caption);
  UpdatePresets((Sender as TMenuItem).Caption);
  UpdateList;
end;

procedure TFormPropDSP.OnDeletePreset(Sender : TObject);
begin
  if fDCDSPFilter.PresetExist((Sender as TMenuItem).Caption) then
  begin
    fDCDSPFilter.DeletePreset((Sender as TMenuItem).Caption);
    UpdatePresets('');
  end;
end;

procedure TFormPropDSP.Add2Click(Sender: TObject);
var
  str : String;
begin
  if InputQuery('Preset Saving.','Enter a Preset Name.',str) and (str <> '') then
  begin
    if fDCDSPFilter.PresetExist(str) then
    begin
      if MessageDlg('Preset already exists. Do you want to overwrite it?',mtConfirmation, [mbYes, mbNo], 0) = mrNo then Exit;
    end;
    fDCDSPFilter.SavePreset(str);
    UpdatePresets(str);
  end;
end;

procedure TFormPropDSP.UpdateBar;

  procedure DrawGradient(StartColor : TColor; EndColor : TColor; Bitmap : TBitmap);
  var
    j : Single;
    deltas : array[0..2] of Single;
    i : integer;
    r : TRect;
  begin
    Deltas[0] := (GetRValue(EndColor) - GetRValue(StartColor)) / Bitmap.Height;
    Deltas[1] := (GetGValue(EndColor) - GetGValue(StartColor)) / Bitmap.Height;
    Deltas[2] := (GetBValue(EndColor) - GetBValue(StartColor)) / Bitmap.Height;
    Bitmap.Canvas.Brush.Style := bsSolid;
    j := 1;
    for i := 0 to Bitmap.Height do
    begin
      r.Left := Width;
      r.Right := 0;
      r.Top := Round(i * j);
      r.Bottom := Round((i + 1) * j);
      Bitmap.Canvas.Brush.Color := RGB(Round(GetRValue(StartColor) + i * Deltas[0]), Round(GetGValue(StartColor) + i *
      Deltas[1]), Round(GetBValue(StartColor) + i * Deltas[2]));
      Bitmap.Canvas.FillRect(r);
    end;
  end;

(*  procedure DrawLinesBar(Bitmap : TBitmap; Color : TColor);
  var
    i : integer;
  begin
    Bitmap.Canvas.Pen.Color := Color;
    for i := 0 to Bitmap.Width div 2 do
    begin
      Bitmap.Canvas.MoveTo(i*2,0);
      Bitmap.Canvas.LineTo(i*2,Bitmap.Height);
    end;
    for i := 0 to Bitmap.Height div 2 do
    begin
      Bitmap.Canvas.MoveTo(0,i*2+1);
      Bitmap.Canvas.LineTo(Bitmap.Width,i*2+1);
    end;
  end;
*)
  procedure DrawLinesBack(Bitmap : TBitmap; Color : TColor);
  var
    i: integer;
  begin
    Bitmap.Canvas.Pen.Color := Color;

    Bitmap.Canvas.MoveTo(0,0);
    Bitmap.Canvas.LineTo(Bitmap.Width,0);
    Bitmap.Canvas.MoveTo(0,1);
    Bitmap.Canvas.LineTo(Bitmap.Width,1);
    for i := 0 to 34 do
    begin
      Bitmap.Canvas.MoveTo(0,i*2+3);
      Bitmap.Canvas.LineTo(Bitmap.Width,i*2+3);
    end;
    Bitmap.Canvas.MoveTo(0,72);
    Bitmap.Canvas.LineTo(Bitmap.Width,72);

    for i := 0 to 38 do
    begin
      Bitmap.Canvas.MoveTo(i*8, 0);
      Bitmap.Canvas.LineTo(i*8, Bitmap.Height);
      Bitmap.Canvas.MoveTo(i*8+1, 0);
      Bitmap.Canvas.LineTo(i*8+1, Bitmap.Height);
    end;
  end;

var
  bmp : TBitmap;
begin
  bmp := TBitmap.Create;
  bmp.PixelFormat := Bar.PixelFormat;

  bmp.Width := Bar.Width;
  bmp.Height := Bar.Height;
  DrawGradient(fBarStart,fBarEnd,bmp);
  DrawLinesBack(bmp,fBack);
  Bar.Canvas.Draw(0,0,bmp);

  bmp.Width := Back.Width;
  bmp.Height := Back.Height;
  DrawGradient(fBackStart,fBackEnd,bmp);
  DrawLinesBack(bmp,fBack);
  Back.Canvas.Draw(0,0,bmp);

  bmp.Free;
end;

procedure TFormPropDSP.SetStartColor1Click(Sender: TObject);
begin
  ColorDialog1.Color := fBarStart;
  if ColorDialog1.Execute then
  begin
    fBarStart := ColorDialog1.Color;
    UpdateBar;
  end;
end;

procedure TFormPropDSP.SetEndColor1Click(Sender: TObject);
begin
  ColorDialog1.Color := fBarEnd;
  if ColorDialog1.Execute then
  begin
    fBarEnd := ColorDialog1.Color;
    UpdateBar;
  end;
end;

procedure TFormPropDSP.SetBackgroundColor1Click(Sender: TObject);
begin
  ColorDialog1.Color := fBack;
  if ColorDialog1.Execute then
  begin
    fBack := ColorDialog1.Color;
    UpdateBar;
  end;
end;

procedure TFormPropDSP.SetTopColor1Click(Sender: TObject);
begin
  ColorDialog1.Color := fBackStart;
  if ColorDialog1.Execute then
  begin
    fBackStart := ColorDialog1.Color;
    UpdateBar;
  end;
end;

procedure TFormPropDSP.SetBackColor1Click(Sender: TObject);
begin
  ColorDialog1.Color := fBackEnd;
  if ColorDialog1.Execute then
  begin
    fBackEnd := ColorDialog1.Color;
    UpdateBar;
  end;
end;

procedure TFormPropDSP.SetDefaultStyle1Click(Sender: TObject);
begin
  fBarStart := clSkyBlue;
  fBarEnd := $00DB7D28;
  fBackStart := $00713F0D;
  fBackEnd := $00713F0D;
  fBack := $0;
  UpdateBar;
end;

procedure TFormPropDSP.PopupMenu2Popup(Sender: TObject);
begin
  Enabled1.Checked := not fStopped;
end;

procedure TFormPropDSP.Enabled1Click(Sender: TObject);
begin
  fStopped := Enabled1.Checked;
  SetVis(fStopped);
end;

procedure TFormPropDSP.FormPaint(Sender: TObject);
begin
  InvalidateRect(lbFilters.Handle,nil,True);
  SendMessage(lbFilters.Handle,WM_NCPAINT,0,0);
end;

procedure TFormPropDSP.lbFiltersClick(Sender: TObject);
var
  i : integer;
begin
  i := lbFilters.ItemIndex;
  if (i < 0) or fDCDSPFilter.WindowShown[i] then Exit;
  CloseAllWindows;
  fDCDSPFilter.WindowShown[i] := True;
{$IFDEF WITH_INTERNAL_DSP}
  case fDCDSPFilter.FilterType(i) of
    ftAmplify:
    begin
      frmDSPAmplify := TfrmDSPAmplify.CreateParented(pnlControlWindow.Handle,fDCDSPFilter.FilterItem(i), pnlControlWindow.Width, pnlControlWindow.Height);
      UpdateTrackbars(frmDSPAmplify);
    end;
    ftBandPass:
    begin
      frmDSPBandPass := TfrmDSPBandPass.CreateParented(pnlControlWindow.Handle,fDCDSPFilter.FilterItem(i), pnlControlWindow.Width, pnlControlWindow.Height);
      UpdateTrackbars(frmDSPBandPass);
    end;
    ftChannelOrder:
    begin
      frmDSPChannelOrder := TfrmDSPChannelOrder.CreateParented(pnlControlWindow.Handle,fDCDSPFilter.FilterItem(i), pnlControlWindow.Width, pnlControlWindow.Height);
      UpdateTrackbars(frmDSPChannelOrder);
    end;
    ftCompressor:
    begin
      frmDSPCompressor := TfrmDSPCompressor.CreateParented(pnlControlWindow.Handle,fDCDSPFilter.FilterItem(i), pnlControlWindow.Width, pnlControlWindow.Height);
      UpdateTrackbars(frmDSPCompressor);
    end;
    ftDownMix:
    begin
      frmDSPDownMix := TfrmDSPDownMix.CreateParented(pnlControlWindow.Handle,fDCDSPFilter.FilterItem(i), pnlControlWindow.Width, pnlControlWindow.Height);
      UpdateTrackbars(frmDSPDownMix);
    end;
    ftDynamicAmplify:
    begin
      frmDSPDynamicAmplify := TfrmDSPDynamicAmplify.CreateParented(pnlControlWindow.Handle,fDCDSPFilter.FilterItem(i), pnlControlWindow.Width, pnlControlWindow.Height);
      UpdateTrackbars(frmDSPDynamicAmplify);
    end;
    ftEchoDelay:
    begin
      frmDSPEchoDelay := TfrmDSPEchoDelay.CreateParented(pnlControlWindow.Handle,fDCDSPFilter.FilterItem(i), pnlControlWindow.Width, pnlControlWindow.Height);
      UpdateTrackbars(frmDSPEchoDelay);
    end;
    ftEqualizer:
    begin
      frmDSPEqualizer := TfrmDSPEqualizer.CreateParented(pnlControlWindow.Handle,fDCDSPFilter.FilterItem(i), pnlControlWindow.Width, pnlControlWindow.Height);
      UpdateTrackbars(frmDSPEqualizer);
    end;
    ftFlanger:
    begin
      frmDSPFlanger := TfrmDSPFlanger.CreateParented(pnlControlWindow.Handle,fDCDSPFilter.FilterItem(i), pnlControlWindow.Width, pnlControlWindow.Height);
      UpdateTrackbars(frmDSPFlanger);
    end;
    ftHighPass:
    begin
      frmDSPHighPass := TfrmDSPHighPass.CreateParented(pnlControlWindow.Handle,fDCDSPFilter.FilterItem(i), pnlControlWindow.Width, pnlControlWindow.Height);
      UpdateTrackbars(frmDSPHighPass);
    end;
    ftLowPass:
    begin
      frmDSPLowPass := TfrmDSPLowPass.CreateParented(pnlControlWindow.Handle,fDCDSPFilter.FilterItem(i), pnlControlWindow.Width, pnlControlWindow.Height);
      UpdateTrackbars(frmDSPLowPass);
    end;
    ftNotch:
    begin
      frmDSPNotch := TfrmDSPNotch.CreateParented(pnlControlWindow.Handle,fDCDSPFilter.FilterItem(i), pnlControlWindow.Width, pnlControlWindow.Height);
      UpdateTrackbars(frmDSPNotch);
    end;
    ftParametricEQ:
    begin
      frmDSPParametricEQ := TfrmDSPParametricEQ.CreateParented(pnlControlWindow.Handle,fDCDSPFilter.FilterItem(i), pnlControlWindow.Width, pnlControlWindow.Height);
      UpdateTrackbars(frmDSPParametricEQ);
    end;
    ftPhaseInvert:
    begin
      frmDSPPhaseInvert := TfrmDSPPhaseInvert.CreateParented(pnlControlWindow.Handle,fDCDSPFilter.FilterItem(i), pnlControlWindow.Width, pnlControlWindow.Height);
      UpdateTrackbars(frmDSPPhaseInvert);
    end;
    ftPhaser:
    begin
      frmDSPPhaser := TfrmDSPPhaser.CreateParented(pnlControlWindow.Handle,fDCDSPFilter.FilterItem(i), pnlControlWindow.Width, pnlControlWindow.Height);
      UpdateTrackbars(frmDSPPhaser);
    end;
    ftPitchScale:
    begin
      frmDSPPitchScale := TfrmDSPPitchScale.CreateParented(pnlControlWindow.Handle,fDCDSPFilter.FilterItem(i), pnlControlWindow.Width, pnlControlWindow.Height);
      UpdateTrackbars(frmDSPPitchScale);
    end;
    ftPitchShift:
    begin
      frmDSPPitchShift := TfrmDSPPitchShift.CreateParented(pnlControlWindow.Handle,fDCDSPFilter.FilterItem(i), pnlControlWindow.Width, pnlControlWindow.Height);
      UpdateTrackbars(frmDSPPitchShift);
    end;
    ftSound3D:
    begin
      frmDSPSound3D := TfrmDSPSound3D.CreateParented(pnlControlWindow.Handle,TDCSound3D(fDCDSPFilter.FilterClass(i)), pnlControlWindow.Width, pnlControlWindow.Height);
      UpdateTrackbars(frmDSPSound3D);
    end;
    ftTempo:
    begin
      frmDSPTempo := TfrmDSPTempo.CreateParented(pnlControlWindow.Handle,fDCDSPFilter.FilterItem(i), pnlControlWindow.Width, pnlControlWindow.Height);
      UpdateTrackbars(frmDSPTempo);
    end;
    ftTrebleEnhancer:
    begin
      frmDSPTrebleEnhancer := TfrmDSPTrebleEnhancer.CreateParented(pnlControlWindow.Handle,fDCDSPFilter.FilterItem(i), pnlControlWindow.Width, pnlControlWindow.Height);
      UpdateTrackbars(frmDSPTrebleEnhancer);
    end;
    ftTrueBass:
    begin
      frmDSPTrueBass := TfrmDSPTrueBass.CreateParented(pnlControlWindow.Handle,fDCDSPFilter.FilterItem(i), pnlControlWindow.Width, pnlControlWindow.Height);
      UpdateTrackbars(frmDSPTrueBass);
    end;
    ftDMOChorus:
    begin
      frmDMOChorus := TfrmDMOChorus.CreateParented(pnlControlWindow.Handle,fDCDSPFilter.FilterItem(i), pnlControlWindow.Width, pnlControlWindow.Height);
      UpdateTrackbars(frmDMOChorus);
    end;
    ftDMOCompressor:
    begin
      frmDMOCompressor := TfrmDMOCompressor.CreateParented(pnlControlWindow.Handle,fDCDSPFilter.FilterItem(i), pnlControlWindow.Width, pnlControlWindow.Height);
      UpdateTrackbars(frmDMOCompressor);
    end;
    ftDMODistortion:
    begin
      frmDMODistortion := TfrmDMODistortion.CreateParented(pnlControlWindow.Handle,fDCDSPFilter.FilterItem(i), pnlControlWindow.Width, pnlControlWindow.Height);
      UpdateTrackbars(frmDMODistortion);
    end;
    ftDMOEcho:
    begin
      frmDMOEcho := TfrmDMOEcho.CreateParented(pnlControlWindow.Handle,fDCDSPFilter.FilterItem(i), pnlControlWindow.Width, pnlControlWindow.Height);
      UpdateTrackbars(frmDMOEcho);
    end;
    ftDMOFlanger:
    begin
      frmDMOFlanger := TfrmDMOFlanger.CreateParented(pnlControlWindow.Handle,fDCDSPFilter.FilterItem(i), pnlControlWindow.Width, pnlControlWindow.Height);
      UpdateTrackbars(frmDMOFlanger);
    end;
    ftDMOGargle:
    begin
      frmDMOGargle := TfrmDMOGargle.CreateParented(pnlControlWindow.Handle,fDCDSPFilter.FilterItem(i), pnlControlWindow.Width, pnlControlWindow.Height);
      UpdateTrackbars(frmDMOGargle);
    end;
    ftDMOI3DL2Reverb:
    begin
      frmDMOI3DL2Reverb := TfrmDMOI3DL2Reverb.CreateParented(pnlControlWindow.Handle,fDCDSPFilter.FilterItem(i), pnlControlWindow.Width, pnlControlWindow.Height);
      UpdateTrackbars(frmDMOI3DL2Reverb);
    end;
    ftDMOParamEQ:
    begin
      frmDMOParamEQ := TfrmDMOParamEQ.CreateParented(pnlControlWindow.Handle,fDCDSPFilter.FilterItem(i), pnlControlWindow.Width, pnlControlWindow.Height);
      UpdateTrackbars(frmDMOParamEQ);
    end;
    ftDMOWavesReverb:
    begin
      frmDMOWavesReverb := TfrmDMOWavesReverb.CreateParented(pnlControlWindow.Handle,fDCDSPFilter.FilterItem(i), pnlControlWindow.Width, pnlControlWindow.Height);
      UpdateTrackbars(frmDMOWavesReverb);
    end;
  end;
{$ENDIF}
end;

procedure TFormPropDSP.UpdatePresets(Preset: String);
var
  i: integer;
begin
  cmbPreset.Clear;
  fCanChange := False;
  for i := 0 to fDCDSPFilter.PresetCount -1 do
    cmbPreset.Items.Add(fDCDSPFilter.PresetName(i));
  if Preset = '' then
  begin
    cmbPreset.ItemIndex := -1;
    fPreset := '';
  end else
  begin
    cmbPreset.ItemIndex := cmbPreset.Items.IndexOf(Preset);
    fPreset := Preset;
  end;
  fCanChange := True;
end;

procedure TFormPropDSP.cmbPresetChange(Sender: TObject);
begin
  if (cmbPreset.Itemindex < 0) or not fCanChange then Exit;
  fDCDSPFilter.LoadPreset(cmbPreset.Items.Strings[cmbPreset.Itemindex]);
  fPreset := cmbPreset.Items.Strings[cmbPreset.Itemindex];
  UpdateList;
end;

procedure TFormPropDSP.btnSavePresetClick(Sender: TObject);
var
  str: String;
begin
  if (cmbPreset.Itemindex < 0) then
  begin
    if InputQuery('Preset Saving.','Enter a Preset Name.',str) and (str <> '') then
    begin
      fDCDSPFilter.SavePreset(str);
      UpdatePresets(str);
    end;
  end else
  begin
    str := cmbPreset.Items[cmbPreset.ItemIndex];
    fDCDSPFilter.SavePreset(str);
    UpdatePresets(str);
  end;
end;

procedure TFormPropDSP.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  tmrVisual.Enabled := False;
end;

initialization

  TBCClassFactory.CreatePropertyPage(TFormPropDSP, CLSID_DCDSPFilterPropertyPageDSP);

end.
