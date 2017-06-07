unit formDSPLowPass;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, dspLowPass, StdCtrls, ComCtrls, DynamicFilterList;

type
  TMySavings = record
    ChannelPos : integer;
  end;
  PMySavings = ^TMySavings;

  TfrmDSPLowPass = class(TForm)
    TabControl2: TTabControl;
    GroupBox1: TGroupBox;
    Label2: TLabel;
    Label1: TLabel;
    Label3: TLabel;
    tbLowPass: TTrackBar;
    cmbLowPassChannels: TComboBox;
    procedure tbLowPassChange(Sender: TObject);
    procedure cmbLowPassChannelsChange(Sender: TObject);
  private
    fPlugin : TDCLowPass;
    fItem : TDCFilterItem;
    fSaving : PMySavings;
    fStreamFrequency : integer;
  public
    constructor CreateParented(ParentWindow: HWnd; Item : TDCFilterItem; Width : integer; Height : integer);
  end;

var
  frmDSPLowPass: TfrmDSPLowPass;
  
implementation

{$R *.dfm}

constructor TfrmDSPLowPass.CreateParented(ParentWindow: HWnd; Item : TDCFilterItem; Width : integer; Height : integer);
begin
  inherited CreateParented(ParentWindow);
  SetWindowPos(Handle,0,0,0,Width,Height,SWP_NOZORDER);
  fPlugin := TDCLowPass(Item.Filter);
  fItem := Item;
  fSaving := PMySavings(fItem.ExtraBuffer);
  fStreamFrequency := fItem.Owner.Stream.Frequency div 2;
  if fSaving <> nil then
  begin
    cmbLowPassChannels.ItemIndex := fSaving.ChannelPos;
    cmbLowPassChannelsChange(Self);
  end;
  Show;
end;

procedure TfrmDSPLowPass.tbLowPassChange(Sender: TObject);
var
  i  :integer;
begin
  label2.Caption := inttostr(0) + ' - ' + inttostr((tbLowPass.Position * fStreamFrequency) div 10000) + ' Hz';
  i := cmbLowPassChannels.ItemIndex;
  if i > 0 then dec(i);
  fPlugin.Cutoff[i] := tbLowPass.Position;
end;

procedure TfrmDSPLowPass.cmbLowPassChannelsChange(Sender: TObject);
var
  i : integer;
begin
  i := cmbLowPassChannels.ItemIndex;
  fPlugin.Seperate := i <> 0;
  if fSaving <> nil then fSaving.ChannelPos := cmbLowPassChannels.ItemIndex;
  if i > 0 then dec(i);
  tbLowPass.Position := fPlugin.Cutoff[i];
  tbLowPassChange(Self);
end;

end.
