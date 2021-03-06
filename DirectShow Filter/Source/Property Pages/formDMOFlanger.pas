unit formDMOFlanger;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, dmoFlanger, StdCtrls, ComCtrls, DynamicFilterList, DirectSound;

type
  TfrmDMOFlanger = class(TForm)
    TabControl2: TTabControl;
    GroupBox1: TGroupBox;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    Label3: TLabel;
    Label15: TLabel;
    Label5: TLabel;
    Label17: TLabel;
    Label7: TLabel;
    Label19: TLabel;
    Label1: TLabel;
    tbWetDryMix: TTrackBar;
    tbDepth: TTrackBar;
    tbFeedback: TTrackBar;
    rbSine: TRadioButton;
    rbTriangle: TRadioButton;
    Label9: TLabel;
    Label21: TLabel;
    Label11: TLabel;
    Label23: TLabel;
    Label24: TLabel;
    Label13: TLabel;
    tbFrequency: TTrackBar;
    tbDelay: TTrackBar;
    tbLFOPhase: TTrackBar;
    procedure rbSineClick(Sender: TObject);
    procedure rbTriangleClick(Sender: TObject);
    procedure tbWetDryMixChange(Sender: TObject);
    procedure tbDepthChange(Sender: TObject);
    procedure tbFeedbackChange(Sender: TObject);
    procedure tbFrequencyChange(Sender: TObject);
    procedure tbDelayChange(Sender: TObject);
    procedure tbLFOPhaseChange(Sender: TObject);
  private
    fPlugin : TDCDMOFlanger;
    fItem : TDCFilterItem;
  public
    constructor CreateParented(ParentWindow: HWnd; Item : TDCFilterItem; Width : integer; Height : integer);
  end;

var
  frmDMOFlanger: TfrmDMOFlanger;

implementation

{$R *.dfm}

constructor TfrmDMOFlanger.CreateParented(ParentWindow: HWnd; Item : TDCFilterItem; Width : integer; Height : integer);
begin
  inherited CreateParented(ParentWindow);
  SetWindowPos(Handle,0,0,0,Width,Height,SWP_NOZORDER);
  fPlugin := TDCDMOFlanger(Item.Filter);
  fItem := Item;
  rbSine.Checked := fPlugin.Waveform = DSFXFLANGER_WAVE_SIN;
  rbTriangle.Checked := fPlugin.Waveform = DSFXFLANGER_WAVE_TRIANGLE;
  tbWetDryMix.Position := Round(fPlugin.WetDryMix * 10);
  tbDepth.Position := Round(fPlugin.Depth * 10);
  tbFeedback.Position := Round(fPlugin.Feedback * 10);
  tbFrequency.Position := Round(fPlugin.Frequency * 10);
  tbDelay.Position := Round(fPlugin.Delay * 10);
  tbLFOPhase.Position := fPlugin.Phase;
  Show;
end;

procedure TfrmDMOFlanger.rbSineClick(Sender: TObject);
begin
  fPlugin.Waveform := DSFXCHORUS_WAVE_SIN;
end;

procedure TfrmDMOFlanger.rbTriangleClick(Sender: TObject);
begin
  fPlugin.Waveform := DSFXCHORUS_WAVE_TRIANGLE;
end;

procedure TfrmDMOFlanger.tbWetDryMixChange(Sender: TObject);
begin
  fPlugin.WetDryMix := tbWetDryMix.Position / 10;
  label3.Caption := Format('%.1f', [tbWetDryMix.Position / 10]);
end;

procedure TfrmDMOFlanger.tbDepthChange(Sender: TObject);
begin
  fPlugin.Depth := tbDepth.Position / 10;
  label5.Caption := Format('%.1f', [tbDepth.Position / 10]);
end;

procedure TfrmDMOFlanger.tbFeedbackChange(Sender: TObject);
begin
  fPlugin.Feedback := tbFeedback.Position / 10;
  label7.Caption := Format('%.1f', [tbFeedback.Position / 10]);
end;

procedure TfrmDMOFlanger.tbFrequencyChange(Sender: TObject);
begin
  fPlugin.Frequency := tbFrequency.Position / 10;
  label9.Caption := Format('%.1f', [tbFrequency.Position / 10]);
end;

procedure TfrmDMOFlanger.tbDelayChange(Sender: TObject);
begin
  fPlugin.Delay := tbDelay.Position / 10;
  label11.Caption := Format('%.1f', [tbDelay.Position / 10]);
end;

procedure TfrmDMOFlanger.tbLFOPhaseChange(Sender: TObject);
begin
  fPlugin.Phase := tbLFOPhase.Position;
  label13.Caption := inttostr(-180 + (tbLFOPhase.Position * 90));
end;

end.
