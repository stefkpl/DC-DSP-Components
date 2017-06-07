unit formDSPFlanger;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, dspFlanger, StdCtrls, ComCtrls, DynamicFilterList;

type
  TMySavings = record
    ChannelPos : integer;
  end;
  PMySavings = ^TMySavings;

  TfrmDSPFlanger = class(TForm)
    TabControl2: TTabControl;
    GroupBox1: TGroupBox;
    Label6: TLabel;
    Label3: TLabel;
    Label8: TLabel;
    Label5: TLabel;
    Label15: TLabel;
    tbFrequency: TTrackBar;
    tbDelay: TTrackBar;
    chkInvertEnabled: TCheckBox;
    cmbChannels: TComboBox;
    procedure tbFrequencyChange(Sender: TObject);
    procedure tbDelayChange(Sender: TObject);
    procedure chkInvertEnabledClick(Sender: TObject);
    procedure cmbChannelsChange(Sender: TObject);
  private
    fPlugin : TDCFlanger;
    fItem : TDCFilterItem;
    fSaving : PMySavings;
  public
    constructor CreateParented(ParentWindow: HWnd; Item : TDCFilterItem; Width : integer; Height : integer);
  end;

var
  frmDSPFlanger: TfrmDSPFlanger;

implementation

{$R *.dfm}

constructor TfrmDSPFlanger.CreateParented(ParentWindow: HWnd; Item : TDCFilterItem; Width : integer; Height : integer);
begin
  inherited CreateParented(ParentWindow);
  SetWindowPos(Handle,0,0,0,Width,Height,SWP_NOZORDER);
  fPlugin := TDCFlanger(Item.Filter);
  fItem := Item;
  fSaving := PMySavings(Item.ExtraBuffer);
  if fSaving <> nil then
  begin
    cmbChannels.ItemIndex := fSaving.ChannelPos;
    cmbChannelsChange(Self);
  end;  
  Show;
end;

procedure TfrmDSPFlanger.tbFrequencyChange(Sender: TObject);
var
  i : integer;
begin
  i := cmbChannels.ItemIndex;
  if i > 0 then dec(i);
  fPlugin.Frequency[i] := tbFrequency.Position / 1000;
  label6.Caption := Format('%.1f sec', [2000 / tbFrequency.Position]);
end;

procedure TfrmDSPFlanger.tbDelayChange(Sender: TObject);
var
  i : integer;
begin
  i := cmbChannels.ItemIndex;
  if i > 0 then dec(i);
  fPlugin.Delay[i] := tbDelay.Position / 1000;
  label8.Caption := inttostr(tbDelay.Position) + ' ms';
end;

procedure TfrmDSPFlanger.chkInvertEnabledClick(Sender: TObject);
var
  i : integer;
begin
  i := cmbChannels.ItemIndex;
  if i > 0 then dec(i);
  fPlugin.PhaseInvert[i] := chkInvertEnabled.Checked;
end;

procedure TfrmDSPFlanger.cmbChannelsChange(Sender: TObject);
var
  i : integer;
begin
  i := cmbChannels.ItemIndex;
  fPlugin.Seperate := i <> 0;
  if fSaving <> nil then fSaving.ChannelPos := i;
  if i > 0 then dec(i);
  chkInvertEnabled.Checked := fPlugin.PhaseInvert[i];
  tbFrequency.Position := Round(fPlugin.Frequency[i]* 1000);
  tbFrequencyChange(Self);
  tbDelay.Position := Round(fPlugin.Delay[i]* 1000);
  tbDelayChange(Self);
end;

end.
