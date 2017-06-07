unit formDMOWavesReverb;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, dmoWavesReverb, StdCtrls, ComCtrls, DynamicFilterList;

type
  TfrmDMOWavesReverb = class(TForm)
    TabControl2: TTabControl;
    GroupBox1: TGroupBox;
    Label3: TLabel;
    Label10: TLabel;
    Label5: TLabel;
    Label12: TLabel;
    Label7: TLabel;
    Label14: TLabel;
    Label9: TLabel;
    Label16: TLabel;
    tbInGain: TTrackBar;
    tbReverbMix: TTrackBar;
    tbReverbTime: TTrackBar;
    tbHFRatio: TTrackBar;
    procedure tbInGainChange(Sender: TObject);
    procedure tbReverbMixChange(Sender: TObject);
    procedure tbReverbTimeChange(Sender: TObject);
    procedure tbHFRatioChange(Sender: TObject);
  private
    fPlugin : TDCDMOWavesReverb;
    fItem : TDCFilterItem;
  public
    constructor CreateParented(ParentWindow: HWnd; Item : TDCFilterItem; Width : integer; Height : integer);
  end;

var
  frmDMOWavesReverb: TfrmDMOWavesReverb;

implementation

{$R *.dfm}

constructor TfrmDMOWavesReverb.CreateParented(ParentWindow: HWnd; Item : TDCFilterItem; Width : integer; Height : integer);
begin
  inherited CreateParented(ParentWindow);
  SetWindowPos(Handle,0,0,0,Width,Height,SWP_NOZORDER);
  fPlugin := TDCDMOWavesReverb(Item.Filter);
  fItem := Item;
  tbInGain.Position := Round(fPlugin.InGain * 10);
  tbInGainChange(Self);
  tbReverbMix.Position := Round(fPlugin.ReverbMix * 10);
  tbReverbMixChange(Self);
  tbReverbTime.Position := Round(fPlugin.ReverbTime * 1000);
  tbReverbTimeChange(Self);
  tbHFRatio.Position := Round(fPlugin.HighFreqRTRatio * 1000);
  tbHFRatioChange(Self);
  Show;
end;

procedure TfrmDMOWavesReverb.tbInGainChange(Sender: TObject);
begin
  fPlugin.InGain := tbInGain.Position / 10;
  label3.Caption := Format('%.1f', [tbInGain.Position / 10]);
end;

procedure TfrmDMOWavesReverb.tbReverbMixChange(Sender: TObject);
begin
  fPlugin.ReverbMix := tbReverbMix.Position / 10;
  label5.Caption := Format('%.1f', [tbReverbMix.Position / 10]);
end;

procedure TfrmDMOWavesReverb.tbReverbTimeChange(Sender: TObject);
begin
  fPlugin.ReverbTime := tbReverbTime.Position / 1000;
  label7.Caption := Format('%.3f', [tbReverbTime.Position / 1000]);
end;

procedure TfrmDMOWavesReverb.tbHFRatioChange(Sender: TObject);
begin
  fPlugin.HighFreqRTRatio := tbHFRatio.Position / 1000;
  label9.Caption := Format('%.3f', [tbHFRatio.Position / 1000]);
end;

end.
