unit formDSPCompressor;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ExtCtrls;

type
  TfrmDSPCompressor = class(TForm)
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
    Timer1: TTimer;   
    tbAttack: TTrackBar;
    tbDecay: TTrackBar;
    chkEnabled: TCheckBox;
    tbThreshold: TTrackBar;
    tbRatio: TTrackBar;
    tbGain: TTrackBar;
    procedure Timer1Timer(Sender: TObject);
    procedure chkEnabledClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure tbRatioChange(Sender: TObject);
    procedure tbGainChange(Sender: TObject);
    procedure tbThresholdChange(Sender: TObject);
    procedure tbAttackChange(Sender: TObject);
    procedure tbDecayChange(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmDSPCompressor: TfrmDSPCompressor;

implementation

uses formMain, dspCompressor;

{$R *.dfm}

procedure TfrmDSPCompressor.Timer1Timer(Sender: TObject);
begin
  if frmMain.DCCompressor.AboveThrs
    then label13.Caption := 'ABOVE'
    else label13.Caption := '';
  if frmMain.DCCompressor.InputLevel = 0
    then label14.Caption := 'N/A'
    else label14.Caption := Format('%.1fdb', [ln(frmMain.DCCompressor.InputLevel)/ln(10)*20]);
  if frmMain.DCCompressor.GainReduction = 0
    then label15.Caption := 'N/A'
    else label15.Caption := Format('%.1fdb', [ln(frmMain.DCCompressor.GainReduction)/ln(10)*20]);
end;

procedure TfrmDSPCompressor.chkEnabledClick(Sender: TObject);
begin
  frmMain.DCCompressor.Enabled := chkEnabled.Checked;
end;

procedure TfrmDSPCompressor.FormCreate(Sender: TObject);
begin
  chkEnabled.Checked := frmMain.DCCompressor.Enabled;
  tbRatio.Position := Round(frmMain.DCCompressor.Ratio * 100);
  tbRatioChange(Self);
  tbGain.Position := Round(frmMain.DCCompressor.GainDB * 10);
  tbGainChange(Self);
  tbThreshold.Position := Round(frmMain.DCCompressor.ThresholdDB * 100);
  tbThresholdChange(Self);
  tbAttack.Position := Round(frmMain.DCCompressor.AttackTime * 1000);
  tbAttackChange(Self);
  tbDecay.Position := Round(frmMain.DCCompressor.DecayTime * 1000);
  tbDecayChange(Self);
end;

procedure TfrmDSPCompressor.tbRatioChange(Sender: TObject);
begin
  frmMain.DCCompressor.Ratio := tbRatio.Position / 100;
  label10.Caption := Format('%.2f', [tbRatio.Position / 100]);
end;

procedure TfrmDSPCompressor.tbGainChange(Sender: TObject);
begin
  frmMain.DCCompressor.GainDB := tbGain.Position / 10;
  label12.Caption := Format('%.2f db', [tbGain.Position / 10]);
end;

procedure TfrmDSPCompressor.tbThresholdChange(Sender: TObject);
begin
  frmMain.DCCompressor.ThresholdDB := tbThreshold.Position / 100;
  label3.Caption := Format('%.2f db', [tbThreshold.Position / 100]);
end;

procedure TfrmDSPCompressor.tbAttackChange(Sender: TObject);
begin
  frmMain.DCCompressor.AttackTime := tbAttack.Position / 1000;
  label6.Caption := Format('%.3f s', [tbAttack.Position / 1000]);
end;

procedure TfrmDSPCompressor.tbDecayChange(Sender: TObject);
begin
  frmMain.DCCompressor.DecayTime := tbDecay.Position / 1000;
  Label8.Caption := Format('%.3f s', [tbDecay.Position / 1000]);
end;

end.
