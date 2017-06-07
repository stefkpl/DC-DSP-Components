unit formDSPParametricEQ;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, dspParametricEQ, StdCtrls, ComCtrls, DynamicFilterList;

type
  TMySavings = record
    ChannelPos : integer;
  end;
  PMySavings = ^TMySavings;

  TfrmDSPParametricEQ = class(TForm)
    TabControl2: TTabControl;
    GroupBox1: TGroupBox;
    Label6: TLabel;
    Label3: TLabel;
    Label8: TLabel;
    Label5: TLabel;
    Label15: TLabel;
    Label1: TLabel;
    Label2: TLabel;
    tbFrequency: TTrackBar;
    tbGain: TTrackBar;
    cmbChannels: TComboBox;
    tbQ: TTrackBar;
    procedure tbFrequencyChange(Sender: TObject);
    procedure tbGainChange(Sender: TObject);
    procedure cmbChannelsChange(Sender: TObject);
    procedure tbQChange(Sender: TObject);
  private
    fCanChange: Boolean;
    fPlugin : TDCParametricEQ;
    fItem : TDCFilterItem;
    fSaving : PMySavings;
  public
    constructor CreateParented(ParentWindow: HWnd; Item : TDCFilterItem; Width : integer; Height : integer);
  end;

var
  frmDSPParametricEQ: TfrmDSPParametricEQ;

implementation

{$R *.dfm}

constructor TfrmDSPParametricEQ.CreateParented(ParentWindow: HWnd; Item : TDCFilterItem; Width : integer; Height : integer);
begin
  inherited CreateParented(ParentWindow);
  SetWindowPos(Handle,0,0,0,Width,Height,SWP_NOZORDER);
  fPlugin := TDCParametricEQ(Item.Filter);
  fItem := Item;
  fSaving := PMySavings(Item.ExtraBuffer);
  fCanChange := False;
  if fSaving <> nil then
  begin
    cmbChannels.ItemIndex := fSaving.ChannelPos;
    cmbChannelsChange(Self);
  end;
  fCanChange := True;
  Show;
end;

procedure TfrmDSPParametricEQ.tbFrequencyChange(Sender: TObject);
var
  i : integer;
begin
  i := cmbChannels.ItemIndex;
  if i > 0 then dec(i);
  if (fCanChange)
    then fPlugin.Frequency[i] := tbFrequency.Position / 1000;
  label6.Caption := Format('%.3f Hz', [tbFrequency.Position / 1000]);
end;

procedure TfrmDSPParametricEQ.tbGainChange(Sender: TObject);
var
  i : integer;
begin
  i := cmbChannels.ItemIndex;
  if i > 0 then dec(i);
  if (fCanChange)
    then fPlugin.GainDB[i] := tbGain.Position / 100;
  label8.Caption := Format('%.2f db', [tbGain.Position / 100]);
end;

procedure TfrmDSPParametricEQ.tbQChange(Sender: TObject);
var
  i : integer;
begin
  i := cmbChannels.ItemIndex;
  if i > 0 then dec(i);
  if (fCanChange)
    then fPlugin.Q[i] := tbQ.Position / 1000;
  label1.Caption := Format('%.3f', [tbQ.Position / 1000]);
end;

procedure TfrmDSPParametricEQ.cmbChannelsChange(Sender: TObject);
var
  i : integer;
begin
  fCanChange := False;
  i := cmbChannels.ItemIndex;
  fPlugin.Seperate := i <> 0;
  if fSaving <> nil then fSaving.ChannelPos := i;
  if i > 0 then dec(i);
  tbFrequency.Position := Round(fPlugin.Frequency[i] * 1000);
  tbFrequencyChange(Self);
  tbGain.Position := Round(fPlugin.GainDB[i] * 100);
  tbGainChange(Self);
  tbQ.Position := Round(fPlugin.Q[i] * 1000);
  tbQChange(Self);
  fCanChange := True;
end;

end.
