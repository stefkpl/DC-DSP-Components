unit formDSPTrebleEnhancer;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, dspTrebleEnhancer, StdCtrls, ComCtrls, DynamicFilterList;

type
  TMySavings = record
    ChannelPos : integer;
  end;
  PMySavings = ^TMySavings;

  TfrmDSPTrebleEnhancer = class(TForm)
    TabControl2: TTabControl;
    GroupBox1: TGroupBox;
    Label13: TLabel;
    Label1: TLabel;
    Label3: TLabel;
    tbTrebleEnhancer: TTrackBar;
    cmbTrebleEnhancerChannels: TComboBox;
    procedure tbTrebleEnhancerChange(Sender: TObject);
    procedure cmbTrebleEnhancerChannelsChange(Sender: TObject);
  private
    fPlugin : TDCTrebleEnhancer;
    fItem : TDCFilterItem;
    fSaving : PMySavings;
  public
    constructor CreateParented(ParentWindow: HWnd; Item : TDCFilterItem; Width : integer; Height : integer);
  end;

var
  frmDSPTrebleEnhancer: TfrmDSPTrebleEnhancer;

implementation

{$R *.dfm}

constructor TfrmDSPTrebleEnhancer.CreateParented(ParentWindow: HWnd; Item : TDCFilterItem; Width : integer; Height : integer);
begin
  inherited CreateParented(ParentWindow);
  SetWindowPos(Handle,0,0,0,Width,Height,SWP_NOZORDER);
  fPlugin := TDCTrebleEnhancer(Item.Filter);
  fItem := Item;
  fSaving := PMySavings(fItem.ExtraBuffer);
  if fSaving <> nil then
  begin
    cmbTrebleEnhancerChannels.ItemIndex := fSaving.ChannelPos;
    cmbTrebleEnhancerChannelsChange(Self);
  end;
  Show;
end;

procedure TfrmDSPTrebleEnhancer.tbTrebleEnhancerChange(Sender: TObject);
var
  i : integer;
begin
  i := cmbTrebleEnhancerChannels.ItemIndex;
  if i > 0 then dec(i);
  label13.Caption := inttostr(tbTrebleEnhancer.Position div 20) + ' %';
  fPlugin.Volume[i] := tbTrebleEnhancer.Position;
end;

procedure TfrmDSPTrebleEnhancer.cmbTrebleEnhancerChannelsChange(
  Sender: TObject);
var
  i : integer;
begin
  i := cmbTrebleEnhancerChannels.ItemIndex;
  fPlugin.Seperate := i <> 0;
  if fSaving <> nil then fSaving.ChannelPos := i;
  if i > 0 then dec(i);
  tbTrebleEnhancer.Position := fPlugin.Volume[i];
  tbTrebleEnhancerChange(Self);
end;

end.
