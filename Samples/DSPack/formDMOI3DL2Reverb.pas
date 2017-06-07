unit formDMOI3DL2Reverb;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, Buttons, DirectSound, dmoConst;

type
  TfrmDMOI3DL2Reverb = class(TForm)
    SpeedButton1: TSpeedButton;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    tbRoom: TTrackBar;
    tbRoomHF: TTrackBar;
    tbRoomRollof: TTrackBar;
    tbDecayTime: TTrackBar;
    tbDecayHF: TTrackBar;
    tbReflections: TTrackBar;
    chkEnabled: TCheckBox;
    Label1: TLabel;
    Label14: TLabel;
    tbReflectDelay: TTrackBar;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    Label23: TLabel;
    Label24: TLabel;
    Label25: TLabel;
    Label26: TLabel;
    tbReverb: TTrackBar;
    tbReverbDelay: TTrackBar;
    tbDiffusion: TTrackBar;
    tbDensity: TTrackBar;
    tbHFReference: TTrackBar;
    tbQuality: TTrackBar;
    Label27: TLabel;
    cmbPreset: TComboBox;
    procedure SpeedButton1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure cmbPresetChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure chkEnabledClick(Sender: TObject);
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
    { Private declarations }
    procedure UpdateValues;
  public
    { Public declarations }
  end;

var
  frmDMOI3DL2Reverb: TfrmDMOI3DL2Reverb;

implementation

uses formMain, dmoI3DL2Reverb;

{$R *.dfm}

procedure TfrmDMOI3DL2Reverb.SpeedButton1Click(Sender: TObject);
begin
  frmMain.DCDMOI3DL2Reverb.ShowPropertyPage(Handle);
  UpdateValues;
end;

procedure TfrmDMOI3DL2Reverb.UpdateValues;
begin
  tbRoom.Position := frmMain.DCDMOI3DL2Reverb.Room;
  tbRoomHF.Position := frmMain.DCDMOI3DL2Reverb.RoomHF;
  tbRoomRollof.Position := Round(frmMain.DCDMOI3DL2Reverb.RoomRolloffFactor * 10);
  tbDecayTime.Position := Round(frmMain.DCDMOI3DL2Reverb.DecayTime * 10);
  tbDecayHF.Position := Round(frmMain.DCDMOI3DL2Reverb.DecayHFRatio * 100);
  tbReflections.Position := frmMain.DCDMOI3DL2Reverb.Reflections;
  tbReflectDelay.Position := Round(frmMain.DCDMOI3DL2Reverb.ReflectionsDelay * 10000);
  tbReverb.Position := frmMain.DCDMOI3DL2Reverb.Reverb;
  tbReverbDelay.Position := Round(frmMain.DCDMOI3DL2Reverb.ReverbDelay * 10000);
  tbDiffusion.Position := Round(frmMain.DCDMOI3DL2Reverb.Diffusion * 10);
  tbDensity.Position := Round(frmMain.DCDMOI3DL2Reverb.Density * 10);
  tbHFReference.Position := Round(frmMain.DCDMOI3DL2Reverb.HFReference * 10);
  tbQuality.Position := frmMain.DCDMOI3DL2Reverb.Quality;
  chkEnabled.Checked := frmMain.DCDMOI3DL2Reverb.Enabled;
end;

procedure TfrmDMOI3DL2Reverb.FormCreate(Sender: TObject);
var
  i : integer;
begin
  for i := 0 to frmMain.DCDMOI3DL2Reverb.PresetCount -1 do
    cmbPreset.Items.Add(frmMain.DCDMOI3DL2Reverb.PresetName[i]);
end;

procedure TfrmDMOI3DL2Reverb.cmbPresetChange(Sender: TObject);
begin
  frmMain.DCDMOI3DL2Reverb.Preset := TDCDMOI3DL2ReverbPreset(cmbPreset.ItemIndex);
  UpdateValues;
end;

procedure TfrmDMOI3DL2Reverb.FormShow(Sender: TObject);
begin
  UpdateValues;
end;

procedure TfrmDMOI3DL2Reverb.chkEnabledClick(Sender: TObject);
begin
  frmMain.DCDMOI3DL2Reverb.Enabled := chkEnabled.Checked;
end;

procedure TfrmDMOI3DL2Reverb.tbRoomChange(Sender: TObject);
begin
  frmMain.DCDMOI3DL2Reverb.Room := tbRoom.Position;
  label3.Caption := inttostr(tbRoom.Position);
end;

procedure TfrmDMOI3DL2Reverb.tbRoomHFChange(Sender: TObject);
begin
  frmMain.DCDMOI3DL2Reverb.RoomHF := tbRoomHF.Position;
  label5.Caption := inttostr(tbRoomHF.Position);
end;

procedure TfrmDMOI3DL2Reverb.tbRoomRollofChange(Sender: TObject);
begin
  frmMain.DCDMOI3DL2Reverb.RoomRolloffFactor := tbRoomRollof.Position / 10;
  label7.Caption := Format('%.1f', [tbRoomRollof.Position / 10]);
end;

procedure TfrmDMOI3DL2Reverb.tbDecayTimeChange(Sender: TObject);
begin
  frmMain.DCDMOI3DL2Reverb.DecayTime := tbDecayTime.Position / 10;
  label9.Caption := Format('%.1f', [tbDecayTime.Position / 10]);
end;

procedure TfrmDMOI3DL2Reverb.tbDecayHFChange(Sender: TObject);
begin
  frmMain.DCDMOI3DL2Reverb.DecayHFRatio := tbDecayHF.Position / 100;
  label11.Caption := Format('%.2f', [tbDecayHF.Position / 100]);
end;

procedure TfrmDMOI3DL2Reverb.tbReflectionsChange(Sender: TObject);
begin
  frmMain.DCDMOI3DL2Reverb.Reflections := tbReflections.Position;
  label13.Caption := inttostr(tbReflections.Position);
end;

procedure TfrmDMOI3DL2Reverb.tbReflectDelayChange(Sender: TObject);
begin
  frmMain.DCDMOI3DL2Reverb.ReflectionsDelay := tbReflectDelay.Position / 10000;
  label14.Caption := Format('%.3f', [tbReflectDelay.Position / 10000]);
end;

procedure TfrmDMOI3DL2Reverb.tbReverbChange(Sender: TObject);
begin
  frmMain.DCDMOI3DL2Reverb.Reverb := tbReverb.Position;
  label16.Caption := inttostr(tbReverb.Position);
end;

procedure TfrmDMOI3DL2Reverb.tbReverbDelayChange(Sender: TObject);
begin
  frmMain.DCDMOI3DL2Reverb.ReverbDelay := tbReverbDelay.Position / 10000;
  label18.Caption := Format('%.3f', [tbReverbDelay.Position / 10000]);
end;

procedure TfrmDMOI3DL2Reverb.tbDiffusionChange(Sender: TObject);
begin
  frmMain.DCDMOI3DL2Reverb.Diffusion := tbDiffusion.Position / 10;
  label20.Caption := Format('%.1f', [tbDiffusion.Position / 10]);
end;

procedure TfrmDMOI3DL2Reverb.tbDensityChange(Sender: TObject);
begin
  frmMain.DCDMOI3DL2Reverb.Density := tbDensity.Position / 10;
  label22.Caption := Format('%.1f', [tbDensity.Position / 10]);
end;

procedure TfrmDMOI3DL2Reverb.tbHFReferenceChange(Sender: TObject);
begin
  frmMain.DCDMOI3DL2Reverb.HFReference := tbHFReference.Position / 10;
  label24.Caption := Format('%.1f', [tbHFReference.Position / 10]);
end;

procedure TfrmDMOI3DL2Reverb.tbQualityChange(Sender: TObject);
begin
  frmMain.DCDMOI3DL2Reverb.Quality := tbQuality.Position;
  label26.Caption := inttostr(tbQuality.Position);
end;

end.
