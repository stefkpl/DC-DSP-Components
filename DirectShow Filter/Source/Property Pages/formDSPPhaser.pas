unit formDSPPhaser;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, dspPhaser, StdCtrls, ComCtrls, DynamicFilterList, Utils, Spin;

type
  TMySavings = record
    ChannelPos : integer;
  end;
  PMySavings = ^TMySavings;

  TfrmDSPPhaser = class(TForm)
    TabControl2: TTabControl;
    GroupBox1: TGroupBox;
    Label15: TLabel;
    cmbPhaserChannels: TComboBox;
    Label6: TLabel;
    Label5: TLabel;
    Label8: TLabel;
    Label17: TLabel;
    Label3: TLabel;
    Label19: TLabel;
    Label10: TLabel;
    Label21: TLabel;
    Label23: TLabel;
    Label14: TLabel;
    Label25: TLabel;
    tbDryWetMix: TTrackBar;
    tbDepth: TTrackBar;
    tbStages: TTrackBar;
    tbFeedback: TTrackBar;
    tbFrequency: TTrackBar;
    SpinEdit1: TSpinEdit;
    procedure tbDryWetMixChange(Sender: TObject);
    procedure cmbPhaserChannelsChange(Sender: TObject);
    procedure SpinEdit1Change(Sender: TObject);
  private
    fPlugin : TDCPhaser;
    fCanChange : Boolean;
    fSaving : PMySavings;
    fItem : TDCFilterItem;
    procedure SetupValues;
    procedure UpdateControls;
  public
    constructor CreateParented(ParentWindow: HWnd; Item : TDCFilterItem; Width : integer; Height : integer);
  end;

var
  frmDSPPhaser: TfrmDSPPhaser;

implementation

{$R *.dfm}

constructor TfrmDSPPhaser.CreateParented(ParentWindow: HWnd; Item : TDCFilterItem; Width : integer; Height : integer);
begin
  inherited CreateParented(ParentWindow);
  SetWindowPos(Handle,0,0,0,Width,Height,SWP_NOZORDER);
  fPlugin := TDCPhaser(Item.Filter);
  fItem := Item;
  fSaving := PMySavings(fItem.ExtraBuffer);
  if fSaving <> nil then
  begin
    cmbPhaserChannels.ItemIndex := fSaving.ChannelPos;
    cmbPhaserChannelsChange(Self);
  end;
  UpdateControls;
  Show;
end;

procedure TfrmDSPPhaser.SetupValues;
var
  i : integer;
begin
  label6.Caption := inttostr(tbDryWetMix.Position * 100 div 255) + ' %';
  label8.Caption := inttostr(tbDepth.Position * 100 div 255) + ' %';
  label10.Caption := inttostr((tbFeedback.Position - 50) * 2) + ' %';
  label3.Caption := inttostr(tbStages.Position);
  label14.Caption := Format('%.2f', [tbFrequency.Position / 100]);
  if not fCanChange then Exit;
  i := cmbPhaserChannels.ItemIndex;
  if i > 0 then dec(i);
  fPlugin.DryWetRatio[i] := tbDryWetMix.Position;
  fPlugin.Depth[i] := tbDepth.Position;
  fPlugin.Stages[i] := tbStages.Position;
  fPlugin.Feedback[i] := tbFeedback.Position;
  fPlugin.StartPhase[i] := SpinEdit1.Value;
  fPlugin.Frequency[i] := tbFrequency.Position / 100;
end;

procedure TfrmDSPPhaser.tbDryWetMixChange(Sender: TObject);
begin
  SetupValues;
end;

procedure TfrmDSPPhaser.cmbPhaserChannelsChange(Sender: TObject);
begin
  fPlugin.Seperate := cmbPhaserChannels.ItemIndex > 0;
  if fSaving <> nil then fSaving.ChannelPos := cmbPhaserChannels.ItemIndex;
  UpdateControls;
end;

procedure TfrmDSPPhaser.UpdateControls;
var
  i : integer;
begin
  fCanChange := False;
  i := cmbPhaserChannels.ItemIndex;
  if i > 0 then dec(i);
  tbDryWetMix.Position := fPlugin.DryWetRatio[i];
  tbDepth.Position := fPlugin.Depth[i];
  tbStages.Position := fPlugin.Stages[i];
  tbFeedback.Position := fPlugin.Feedback[i];
  SpinEdit1.Value := Round(fPlugin.StartPhase[i]);
  tbFrequency.Position := Round(fPlugin.Frequency[i] * 100);
  fCanChange := True;
end;

procedure TfrmDSPPhaser.SpinEdit1Change(Sender: TObject);
begin
  SetupValues;
end;

end.
