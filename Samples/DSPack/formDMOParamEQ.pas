unit formDMOParamEQ;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, Buttons;

type
  TfrmDMOParamEQ = class(TForm)
    SpeedButton1: TSpeedButton;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    tbCenter: TTrackBar;
    tbBandwidth: TTrackBar;
    tbGain: TTrackBar;
    chkEnabled: TCheckBox;
    procedure SpeedButton1Click(Sender: TObject);
    procedure chkEnabledClick(Sender: TObject);
    procedure tbCenterChange(Sender: TObject);
    procedure tbBandwidthChange(Sender: TObject);
    procedure tbGainChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
    procedure UpdateValues;
  public
    { Public declarations }
  end;

var
  frmDMOParamEQ: TfrmDMOParamEQ;

implementation

uses formMain;

{$R *.dfm}

procedure TfrmDMOParamEQ.SpeedButton1Click(Sender: TObject);
begin
  frmMain.DCDMOParamEQ.ShowPropertyPage(Handle);
  UpdateValues;
end;

procedure TfrmDMOParamEQ.UpdateValues;
begin
  tbCenter.Position := Round(frmMain.DCDMOParamEQ.Center * 10);
  tbBandwidth.Position := Round(frmMain.DCDMOParamEQ.Bandwidth * 10);
  tbGain.Position := Round(frmMain.DCDMOParamEQ.Gain * 10);
  chkEnabled.Checked := frmMain.DCDMOParamEQ.Enabled;
end;

procedure TfrmDMOParamEQ.chkEnabledClick(Sender: TObject);
begin
  frmMain.DCDMOParamEQ.Enabled := chkEnabled.Checked;
end;

procedure TfrmDMOParamEQ.tbCenterChange(Sender: TObject);
begin
  frmMain.DCDMOParamEQ.Center := tbCenter.Position / 10;
  label3.Caption := Format('%.1f', [tbCenter.Position / 10]);
end;

procedure TfrmDMOParamEQ.tbBandwidthChange(Sender: TObject);
begin
  frmMain.DCDMOParamEQ.Bandwidth := tbBandwidth.Position / 10;
  label5.Caption := Format('%.1f', [tbBandwidth.Position / 10]);
end;

procedure TfrmDMOParamEQ.tbGainChange(Sender: TObject);
begin
  frmMain.DCDMOParamEQ.Gain := tbGain.Position / 10;
  label7.Caption := Format('%.1f', [tbGain.Position / 10]);
end;

procedure TfrmDMOParamEQ.FormShow(Sender: TObject);
begin
  UpdateValues;
end;

end.
