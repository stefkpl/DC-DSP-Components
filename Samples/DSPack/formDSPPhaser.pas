unit formDSPPhaser;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls;

type
  TfrmDSPPhaser = class(TForm)
    Label6: TLabel;
    Label7: TLabel;
    Label1: TLabel;
    Label8: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    tbDryWetMix: TTrackBar;
    tbDepth: TTrackBar;
    chkPhaserEnabled: TCheckBox;
    tbStages: TTrackBar;
    tbFeedback: TTrackBar;
    tbStartPhase: TTrackBar;
    tbFrequency: TTrackBar;
    cmbPhaserChannels: TComboBox;
    procedure tbDryWetMixChange(Sender: TObject);
    procedure chkPhaserEnabledClick(Sender: TObject);
    procedure cmbPhaserChannelsChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
    CanChange : Boolean;
    procedure SetupValues;
    procedure UpdateControls;
  public
    { Public declarations }
  end;

var
  frmDSPPhaser: TfrmDSPPhaser;

implementation

uses formMain;

{$R *.dfm}

procedure TfrmDSPPhaser.SetupValues;
var
  i : integer;
begin
  label6.Caption := inttostr(tbDryWetMix.Position * 100 div 255) + ' %';
  label8.Caption := inttostr(tbDepth.Position * 100 div 255) + ' %';
  label10.Caption := inttostr((tbFeedback.Position - 50) * 2) + ' %';
  label3.Caption := inttostr(tbStages.Position);
  label12.Caption := inttostr(tbStartPhase.Position);
  label14.Caption := Format('%.2f', [tbFrequency.Position / 100]);
  if CanChange then Exit;
  i := cmbPhaserChannels.ItemIndex;
  if i > 0 then dec(i);
  frmMain.DCPhaser.DryWetRatio[i] := tbDryWetMix.Position;
  frmMain.DCPhaser.Depth[i] := tbDepth.Position;
  frmMain.DCPhaser.Stages[i] := tbStages.Position;
  frmMain.DCPhaser.Feedback[i] := tbFeedback.Position;
  frmMain.DCPhaser.StartPhase[i] := tbStartPhase.Position;
  frmMain.DCPhaser.Frequency[i] := tbFrequency.Position / 100;
end;

procedure TfrmDSPPhaser.tbDryWetMixChange(Sender: TObject);
begin
  SetupValues;
end;

procedure TfrmDSPPhaser.chkPhaserEnabledClick(Sender: TObject);
begin
  frmMain.DCPhaser.Enabled := chkPhaserEnabled.Checked;
end;

procedure TfrmDSPPhaser.cmbPhaserChannelsChange(Sender: TObject);
begin
  frmMain.DCPhaser.Seperate := cmbPhaserChannels.ItemIndex > 0;
  UpdateControls;
end;

procedure TfrmDSPPhaser.UpdateControls;
var
  i : integer;
begin
  CanChange := True;
  i := cmbPhaserChannels.ItemIndex;
  if i > 0 then dec(i);
  tbDryWetMix.Position := frmMain.DCPhaser.DryWetRatio[i];
  tbDepth.Position := frmMain.DCPhaser.Depth[i];
  tbStages.Position := frmMain.DCPhaser.Stages[i];
  tbFeedback.Position := frmMain.DCPhaser.Feedback[i];
  tbStartPhase.Position := Round(frmMain.DCPhaser.StartPhase[i]);
  tbFrequency.Position := Round(frmMain.DCPhaser.Frequency[i] * 100);
  CanChange := False;
end;

procedure TfrmDSPPhaser.FormShow(Sender: TObject);
begin
  UpdateControls;
end;

end.
