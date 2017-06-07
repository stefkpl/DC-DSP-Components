unit formDMOI3DL2Reverb;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, dmoI3DL2Reverb, StdCtrls, ComCtrls, DynamicFilterList, dmoConst;

type
  TMySavings = record
    Preset : integer;
  end;
  PMySavings = ^TMySavings;

  TfrmDMOI3DL2Reverb = class(TForm)
    TabControl2: TTabControl;
    GroupBox1: TGroupBox;
    Label27: TLabel;
    cmbPreset: TComboBox;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    Label3: TLabel;
    Label42: TLabel;
    Label5: TLabel;
    Label44: TLabel;
    Label7: TLabel;
    Label46: TLabel;
    tbRoom: TTrackBar;
    tbRoomHF: TTrackBar;
    tbRoomRollof: TTrackBar;
    Label9: TLabel;
    Label48: TLabel;
    Label11: TLabel;
    Label50: TLabel;
    Label13: TLabel;
    Label52: TLabel;
    tbDecayTime: TTrackBar;
    tbDecayHF: TTrackBar;
    tbReflections: TTrackBar;
    TabSheet3: TTabSheet;
    Label14: TLabel;
    Label53: TLabel;
    Label16: TLabel;
    Label55: TLabel;
    Label18: TLabel;
    Label57: TLabel;
    tbReflectDelay: TTrackBar;
    tbReverb: TTrackBar;
    tbReverbDelay: TTrackBar;
    TabSheet4: TTabSheet;
    Label20: TLabel;
    Label59: TLabel;
    Label22: TLabel;
    Label61: TLabel;
    tbDiffusion: TTrackBar;
    tbDensity: TTrackBar;
    TabSheet5: TTabSheet;
    Label24: TLabel;
    Label63: TLabel;
    Label26: TLabel;
    lanel12: TLabel;
    tbHFReference: TTrackBar;
    tbQuality: TTrackBar;
    procedure cmbPresetChange(Sender: TObject);
    procedure tbRoomChange(Sender: TObject);
    procedure tbRoomHFChange(Sender: TObject);
    procedure tbRoomRollofChange(Sender: TObject);
    procedure tbDecayTimeChange(Sender: TObject);
    procedure tbDecayHFChange(Sender: TObject);
    procedure tbReflectionsChange(Sender: TObject);
    procedure tbReflectDelayChange(Sender: TObject);
    procedure tbReverbChange(Sender: TObject);
    procedure tbReverbDelayChange(Sender: TObject);
    procedure tbDiffusionChange(Sender: TObject);
    procedure tbDensityChange(Sender: TObject);
    procedure tbHFReferenceChange(Sender: TObject);
    procedure tbQualityChange(Sender: TObject);
  private
    fPlugin : TDCDMOI3DL2Reverb;
    fItem : TDCFilterItem;
    fSaving : PMySavings;
    fStarting : Boolean;
    procedure UpdateValues;
    procedure ResetPreset;
  public
    constructor CreateParented(ParentWindow: HWnd; Item : TDCFilterItem; Width : integer; Height : integer);
  end;

var
  frmDMOI3DL2Reverb: TfrmDMOI3DL2Reverb;

implementation

{$R *.dfm}

constructor TfrmDMOI3DL2Reverb.CreateParented(ParentWindow: HWnd; Item : TDCFilterItem; Width : integer; Height : integer);
var
  i : integer;
begin
  inherited CreateParented(ParentWindow);
  SetWindowPos(Handle,0,0,0,Width,Height,SWP_NOZORDER);
  fPlugin := TDCDMOI3DL2Reverb(Item.Filter);
  fItem := Item;
  fStarting := True;
  for i := 0 to fPlugin.PresetCount -1 do
    cmbPreset.Items.Add(fPlugin.PresetName[i]);
  UpdateValues;
  fSaving := PMySavings(fItem.ExtraBuffer);
  if fSaving <> nil then
  begin
    if fSaving.Preset > 0 then cmbPreset.ItemIndex := fSaving.Preset -1;
  end;
  fStarting := False;
  Show;
end;

procedure TfrmDMOI3DL2Reverb.ResetPreset;
begin
  if not fStarting then
  begin
    cmbPreset.ItemIndex := -1;
    if fSaving <> nil then fSaving.Preset := 0;
  end;
end;

procedure TfrmDMOI3DL2Reverb.UpdateValues;
begin
  Set8087CW(($133F and $FCFF) or (512));

  tbRoom.Position := fPlugin.Room;
  tbRoomHF.Position := fPlugin.RoomHF;
  tbRoomRollof.Position := Round(fPlugin.RoomRolloffFactor * 10);
  tbDecayTime.Position := Round(fPlugin.DecayTime * 10);
  tbDecayHF.Position := Round(fPlugin.DecayHFRatio * 100);
  tbReflections.Position := fPlugin.Reflections;
  tbReflectDelay.Position := Round(fPlugin.ReflectionsDelay * 10000);
  tbReverb.Position := fPlugin.Reverb;
  tbReverbDelay.Position := Round(fPlugin.ReverbDelay * 10000);
  tbDiffusion.Position := Round(fPlugin.Diffusion * 10);
  tbDensity.Position := Round(fPlugin.Density * 10);
  tbHFReference.Position := Round(fPlugin.HFReference * 10);
  tbQuality.Position := fPlugin.Quality;

  Set8087CW((DCCW and $FCFF) or (512));
end;

procedure TfrmDMOI3DL2Reverb.cmbPresetChange(Sender: TObject);
begin
  fStarting := True;
  if fSaving <> nil then fSaving.Preset := cmbPreset.ItemIndex + 1;
  fPlugin.Preset := TDCDMOI3DL2ReverbPreset(cmbPreset.ItemIndex);
  UpdateValues;
  fStarting := False;
end;

procedure TfrmDMOI3DL2Reverb.tbRoomChange(Sender: TObject);
begin
  fPlugin.Room := tbRoom.Position;
  label3.Caption := inttostr(tbRoom.Position);
  ResetPreset;
end;

procedure TfrmDMOI3DL2Reverb.tbRoomHFChange(Sender: TObject);
begin
  fPlugin.RoomHF := tbRoomHF.Position;
  label5.Caption := inttostr(tbRoomHF.Position);
  ResetPreset;
end;

procedure TfrmDMOI3DL2Reverb.tbRoomRollofChange(Sender: TObject);
begin
  fPlugin.RoomRolloffFactor := tbRoomRollof.Position / 10;
  label7.Caption := Format('%.1f', [tbRoomRollof.Position / 10]);
  ResetPreset;
end;

procedure TfrmDMOI3DL2Reverb.tbDecayTimeChange(Sender: TObject);
begin
  fPlugin.DecayTime := tbDecayTime.Position / 10;
  label9.Caption := Format('%.1f', [tbDecayTime.Position / 10]);
  ResetPreset;
end;

procedure TfrmDMOI3DL2Reverb.tbDecayHFChange(Sender: TObject);
begin
  fPlugin.DecayHFRatio := tbDecayHF.Position / 100;
  label11.Caption := Format('%.2f', [tbDecayHF.Position / 100]);
  ResetPreset;
end;

procedure TfrmDMOI3DL2Reverb.tbReflectionsChange(Sender: TObject);
begin
  fPlugin.Reflections := tbReflections.Position;
  label13.Caption := inttostr(tbReflections.Position);
  ResetPreset;
end;

procedure TfrmDMOI3DL2Reverb.tbReflectDelayChange(Sender: TObject);
begin
  fPlugin.ReflectionsDelay := tbReflectDelay.Position / 10000;
  label14.Caption := Format('%.3f', [tbReflectDelay.Position / 10000]);
  ResetPreset;
end;

procedure TfrmDMOI3DL2Reverb.tbReverbChange(Sender: TObject);
begin
  fPlugin.Reverb := tbReverb.Position;
  label16.Caption := inttostr(tbReverb.Position);
  ResetPreset;
end;

procedure TfrmDMOI3DL2Reverb.tbReverbDelayChange(Sender: TObject);
begin
  fPlugin.ReverbDelay := tbReverbDelay.Position / 10000;
  label18.Caption := Format('%.3f', [tbReverbDelay.Position / 10000]);
  ResetPreset;
end;

procedure TfrmDMOI3DL2Reverb.tbDiffusionChange(Sender: TObject);
begin
  fPlugin.Diffusion := tbDiffusion.Position / 10;
  label20.Caption := Format('%.1f', [tbDiffusion.Position / 10]);
  ResetPreset;
end;

procedure TfrmDMOI3DL2Reverb.tbDensityChange(Sender: TObject);
begin
  fPlugin.Density := tbDensity.Position / 10;
  label22.Caption := Format('%.1f', [tbDensity.Position / 10]);
  ResetPreset;
end;

procedure TfrmDMOI3DL2Reverb.tbHFReferenceChange(Sender: TObject);
begin
  fPlugin.HFReference := tbHFReference.Position / 10;
  label24.Caption := Format('%.1f', [tbHFReference.Position / 10]);
  ResetPreset;
end;

procedure TfrmDMOI3DL2Reverb.tbQualityChange(Sender: TObject);
begin
  fPlugin.Quality := tbQuality.Position;
  label26.Caption := inttostr(tbQuality.Position);
  ResetPreset;
end;

end.
