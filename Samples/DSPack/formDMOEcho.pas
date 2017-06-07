unit formDMOEcho;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, Buttons, DirectSound;

type
  TfrmDMOEcho = class(TForm)
    SpeedButton1: TSpeedButton;
    chkEnabled: TCheckBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    rbNormalPan: TRadioButton;
    rbSwapChannels: TRadioButton;
    tbWetDryMix: TTrackBar;
    tbFeedback: TTrackBar;
    tbLeftDelay: TTrackBar;
    tbRightDelay: TTrackBar;
    procedure SpeedButton1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure chkEnabledClick(Sender: TObject);
    procedure tbWetDryMixChange(Sender: TObject);
    procedure tbFeedbackChange(Sender: TObject);
    procedure tbLeftDelayChange(Sender: TObject);
    procedure tbRightDelayChange(Sender: TObject);
    procedure rbNormalPanClick(Sender: TObject);
    procedure rbSwapChannelsClick(Sender: TObject);
  private
    { Private declarations }
    procedure UpdateValues;
  public
    { Public declarations }
  end;

var
  frmDMOEcho: TfrmDMOEcho;

implementation

uses formMain, dmoEcho;

{$R *.dfm}

procedure TfrmDMOEcho.SpeedButton1Click(Sender: TObject);
begin
  frmMain.DCDMOEcho.ShowPropertyPage(Handle);
  UpdateValues;
end;

procedure TfrmDMOEcho.UpdateValues;
begin
  rbNormalPan.Checked := frmMain.DCDMOEcho.PanDelay = DSFXECHO_PANDELAY_MIN;
  rbSwapChannels.Checked := frmMain.DCDMOEcho.PanDelay = DSFXECHO_PANDELAY_MAX;
  tbWetDryMix.Position := Round(frmMain.DCDMOEcho.WetDryMix * 10);
  tbFeedback.Position := Round(frmMain.DCDMOEcho.Feedback * 10);
  tbLeftDelay.Position := Round(frmMain.DCDMOEcho.LeftDelay * 10);
  tbRightDelay.Position := Round(frmMain.DCDMOEcho.RightDelay * 10);
  chkEnabled.Checked := frmMain.DCDMOEcho.Enabled;
end;

procedure TfrmDMOEcho.FormShow(Sender: TObject);
begin
  UpdateValues;
end;

procedure TfrmDMOEcho.chkEnabledClick(Sender: TObject);
begin
  frmMain.DCDMOEcho.Enabled := chkEnabled.Checked;
end;

procedure TfrmDMOEcho.tbWetDryMixChange(Sender: TObject);
begin
  frmMain.DCDMOEcho.WetDryMix := tbWetDryMix.Position / 10;
  label3.Caption := Format('%.1f', [tbWetDryMix.Position / 10]);
end;

procedure TfrmDMOEcho.tbFeedbackChange(Sender: TObject);
begin
  frmMain.DCDMOEcho.Feedback := tbFeedback.Position / 10;
  label5.Caption := Format('%.1f', [tbFeedback.Position / 10]);
end;

procedure TfrmDMOEcho.tbLeftDelayChange(Sender: TObject);
begin
  frmMain.DCDMOEcho.LeftDelay := tbLeftDelay.Position / 10;
  label7.Caption := Format('%.1f', [tbLeftDelay.Position / 10]);
end;

procedure TfrmDMOEcho.tbRightDelayChange(Sender: TObject);
begin
  frmMain.DCDMOEcho.RightDelay := tbRightDelay.Position / 10;
  label9.Caption := Format('%.1f', [tbRightDelay.Position / 10]);
end;

procedure TfrmDMOEcho.rbNormalPanClick(Sender: TObject);
begin
  frmMain.DCDMOEcho.PanDelay := DSFXECHO_PANDELAY_MIN;
end;

procedure TfrmDMOEcho.rbSwapChannelsClick(Sender: TObject);
begin
  frmMain.DCDMOEcho.PanDelay := DSFXECHO_PANDELAY_MAX;
end;

end.
