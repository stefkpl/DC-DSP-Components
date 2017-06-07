unit formDSPNotch;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, dspNotch, StdCtrls, ComCtrls, DynamicFilterList;

type
  TMySavings = record
    ChannelPos : integer;
  end;
  PMySavings = ^TMySavings;

  TfrmDSPNotch = class(TForm)
    TabControl2: TTabControl;
    GroupBox1: TGroupBox;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    tbNotch: TTrackBar;
    cmbChannels: TComboBox;
    procedure tbNotchChange(Sender: TObject);
    procedure cmbChannelsChange(Sender: TObject);
  private
    fPlugin : TDCNotch;
    fItem : TDCFilterItem;
    fSaving : PMySavings;
    fStreamFrequency : integer;
  public
    constructor CreateParented(ParentWindow: HWnd; Item : TDCFilterItem; Width : integer; Height : integer);
  end;

var
  frmDSPNotch: TfrmDSPNotch;
  
implementation

{$R *.dfm}

constructor TfrmDSPNotch.CreateParented(ParentWindow: HWnd; Item : TDCFilterItem; Width : integer; Height : integer);
begin
  inherited CreateParented(ParentWindow);
  SetWindowPos(Handle,0,0,0,Width,Height,SWP_NOZORDER);
  fPlugin := TDCNotch(Item.Filter);
  fItem := Item;
  fSaving := PMySavings(fItem.ExtraBuffer);
  fStreamFrequency := fItem.Owner.Stream.Frequency div 2;
  if fSaving <> nil then
  begin
    cmbChannels.ItemIndex := fSaving.ChannelPos;
    cmbChannelsChange(Self);
  end;
  Show;
end;

procedure TfrmDSPNotch.tbNotchChange(Sender: TObject);
var
  i  :integer;
begin
  label2.Caption := inttostr((tbNotch.Position * fStreamFrequency) div 10000) + ' Hz';
  i := cmbChannels.ItemIndex;
  if i > 0 then dec(i);
  fPlugin.Cutoff[i] := tbNotch.Position;
end;

procedure TfrmDSPNotch.cmbChannelsChange(Sender: TObject);
var
  i : integer;
begin
  i := cmbChannels.ItemIndex;
  fPlugin.Seperate := i <> 0;
  if i > 0 then dec(i);
  tbNotch.Position := fPlugin.Cutoff[i];
  tbNotchChange(Self);
end;

end.
