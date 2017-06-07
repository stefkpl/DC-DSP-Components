unit formDMODistortion;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, Buttons, DirectSound;

type
  TfrmDMODistortion = class(TForm)
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
    tbGain: TTrackBar;
    tbEdge: TTrackBar;
    tbFrequency: TTrackBar;
    tbBandwidth: TTrackBar;
    tbCutoff: TTrackBar;
    chkEnabled: TCheckBox;
    procedure SpeedButton1Click(Sender: TObject);
    procedure tbGainChange(Sender: TObject);
    procedure chkEnabledClick(Sender: TObject);
    procedure tbEdgeChange(Sender: TObject);
    procedure tbFrequencyChange(Sender: TObject);
    procedure tbBandwidthChange(Sender: TObject);
    procedure tbCutoffChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
    procedure UpdateValues;
  public
    { Public declarations }
  end;

var
  frmDMODistortion: TfrmDMODistortion;

implementation

uses formMain;

{$R *.dfm}

procedure TfrmDMODistortion.SpeedButton1Click(Sender: TObject);
begin
  frmMain.DCDMODistortion.ShowPropertyPage(Handle);
  UpdateValues;
end;

procedure TfrmDMODistortion.UpdateValues;
begin
  tbGain.Position := Round(frmMain.DCDMODistortion.Gain * 10);
  tbEdge.Position := Round(frmMain.DCDMODistortion.Edge * 10);
  tbFrequency.Position := Round(frmMain.DCDMODistortion.PostEQCenterFrequency * 10);
  tbBandwidth.Position := Round(frmMain.DCDMODistortion.PostEQBandwidth * 10);
  tbCutoff.Position := Round(frmMain.DCDMODistortion.PreLowpassCutoff * 10);
  chkEnabled.Checked := frmMain.DCDMODistortion.Enabled;
end;

procedure TfrmDMODistortion.tbGainChange(Sender: TObject);
begin
  frmMain.DCDMODistortion.Gain := tbGain.Position / 10;
  label3.Caption := Format('%.1f', [tbGain.Position / 10]);
end;

procedure TfrmDMODistortion.chkEnabledClick(Sender: TObject);
begin
  frmMain.DCDMODistortion.Enabled := chkEnabled.Checked;
end;

procedure TfrmDMODistortion.tbEdgeChange(Sender: TObject);
begin
  frmMain.DCDMODistortion.Edge := tbEdge.Position / 10;
  label5.Caption := Format('%.1f', [tbEdge.Position / 10]);
end;

procedure TfrmDMODistortion.tbFrequencyChange(Sender: TObject);
begin
  frmMain.DCDMODistortion.PostEQCenterFrequency := tbFrequency.Position / 10;
  label7.Caption := Format('%.1f', [tbFrequency.Position / 10]);
end;

procedure TfrmDMODistortion.tbBandwidthChange(Sender: TObject);
begin
  frmMain.DCDMODistortion.PostEQBandwidth := tbBandwidth.Position / 10;
  label9.Caption := Format('%.1f', [tbBandwidth.Position / 10]);
end;

procedure TfrmDMODistortion.tbCutoffChange(Sender: TObject);
begin
  frmMain.DCDMODistortion.PreLowpassCutoff := tbCutoff.Position / 10;
  label11.Caption := Format('%.1f', [tbCutoff.Position / 10]);
end;

procedure TfrmDMODistortion.FormShow(Sender: TObject);
begin
  UpdateValues;
end;

end.
