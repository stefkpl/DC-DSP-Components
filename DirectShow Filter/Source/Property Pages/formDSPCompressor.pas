unit formDSPCompressor;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, dspCompressor, StdCtrls, ComCtrls, ExtCtrls, DynamicFilterList;

type
  TfrmDSPCompressor = class(TForm)
    Timer1: TTimer;
    TabControl1: TTabControl;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label4: TLabel;
    Label6: TLabel;
    Label13: TLabel;
    label8: TLabel;
    Label16: TLabel;
    label3: TLabel;
    Label18: TLabel;
    label10: TLabel;
    Label20: TLabel;
    label12: TLabel;
    Label22: TLabel;
    tbAttack: TTrackBar;
    tbDecay: TTrackBar;
    tbThreshold: TTrackBar;
    tbRatio: TTrackBar;
    tbGain: TTrackBar;
    procedure tbAttackChange(Sender: TObject);
    procedure tbDecayChange(Sender: TObject);
    procedure tbThresholdChange(Sender: TObject);
    procedure tbRatioChange(Sender: TObject);
    procedure tbGainChange(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    fPlugin : TDCCompressor;
    fItem : TDCFilterItem;
  public
    constructor CreateParented(ParentWindow: HWnd; Item : TDCFilterItem; Width : integer; Height : integer);
  end;

var
  frmDSPCompressor: TfrmDSPCompressor;

implementation

{$R *.dfm}

constructor TfrmDSPCompressor.CreateParented(ParentWindow: HWnd; Item : TDCFilterItem; Width : integer; Height : integer);
begin
  inherited CreateParented(ParentWindow);
  SetWindowPos(Handle,0,0,0,Width,Height,SWP_NOZORDER);
  fPlugin := TDCCompressor(Item.Filter);
  fItem := Item;
  tbRatio.Position := Round(fPlugin.Ratio * 10);
  tbRatioChange(Self);
  tbGain.Position := Round(fPlugin.GainDB * 10);
  tbGainChange(Self);
  tbThreshold.Position := Round(fPlugin.ThresholdDB * 100);
  tbThresholdChange(Self);
  tbAttack.Position := Round(fPlugin.AttackTime * 1000);
  tbAttackChange(Self);
  tbDecay.Position := Round(fPlugin.DecayTime * 1000);
  tbDecayChange(Self);
  Show;
end;

procedure TfrmDSPCompressor.tbAttackChange(Sender: TObject);
begin
  fPlugin.AttackTime := tbAttack.Position / 1000;
  label6.Caption := Format('%.3f s', [tbAttack.Position / 1000]);
end;

procedure TfrmDSPCompressor.tbDecayChange(Sender: TObject);
begin
  fPlugin.DecayTime := tbDecay.Position / 1000;
  Label8.Caption := Format('%.3f s', [tbDecay.Position / 1000]);
end;

procedure TfrmDSPCompressor.tbThresholdChange(Sender: TObject);
begin
  fPlugin.ThresholdDB := tbThreshold.Position / 100;
  label3.Caption := Format('%.2f db', [tbThreshold.Position / 100]);
end;

procedure TfrmDSPCompressor.tbRatioChange(Sender: TObject);
begin
  fPlugin.Ratio := tbRatio.Position / 10;
  label10.Caption := Format('%.2f', [tbRatio.Position / 10]);
end;

procedure TfrmDSPCompressor.tbGainChange(Sender: TObject);
begin
  fPlugin.GainDB := tbGain.Position / 10;
  label12.Caption := Format('%.2f db', [tbGain.Position / 10]);
end;

procedure TfrmDSPCompressor.Timer1Timer(Sender: TObject);
begin
  if fPlugin.AboveThrs
    then Label1.Caption := 'Threshold: Above'
    else Label1.Caption := 'Threshold: N/A';

  if fPlugin.InputLevel = 0
    then Label2.Caption := 'Input Level: 0'
    else Label2.Caption := 'Input Level: ' + Format('%.1f db', [ln(fPlugin.InputLevel)/ln(10)*20]);

  if fPlugin.GainReduction = 0
    then Label4.Caption := 'Gain Reduction: 0'
    else Label4.Caption := 'Gain Reduction: ' + Format('%.1f db', [ln(fPlugin.GainReduction)/ln(10)*20]);
end;

end.
