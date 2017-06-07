unit formDSPTrueBass;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, dspTrueBass, StdCtrls, ComCtrls, DynamicFilterList;

type
  TMySavings = record
    ChannelPos : integer;
  end;
  PMySavings = ^TMySavings;

  TfrmDSPTrueBass = class(TForm)
    TabControl2: TTabControl;
    GroupBox1: TGroupBox;
    Label13: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    tbTrueBass: TTrackBar;
    cmbTrueBassChannels: TComboBox;
    procedure tbTrueBassChange(Sender: TObject);
    procedure cmbTrueBassChannelsChange(Sender: TObject);
  private
    fPlugin : TDCTrueBass;
    fItem : TDCFilterItem;
    fSaving : PMySavings;
  public
    constructor CreateParented(ParentWindow: HWnd; Item : TDCFilterItem; Width : integer; Height : integer);
  end;

var
  frmDSPTrueBass: TfrmDSPTrueBass;
  
implementation

{$R *.dfm}

constructor TfrmDSPTrueBass.CreateParented(ParentWindow: HWnd; Item : TDCFilterItem; Width : integer; Height : integer);
begin
  inherited CreateParented(ParentWindow);
  SetWindowPos(Handle,0,0,0,Width,Height,SWP_NOZORDER);
  fPlugin := TDCTrueBass(Item.Filter);
  fItem := Item;
  fSaving := PMySavings(fItem.ExtraBuffer);
  if fSaving <> nil then
  begin
    cmbTrueBassChannels.ItemIndex := fSaving.ChannelPos;
    cmbTrueBassChannelsChange(Self);
  end;
  Show;
end;

procedure TfrmDSPTrueBass.tbTrueBassChange(Sender: TObject);
var
  i : integer;
begin
  i := cmbTrueBassChannels.ItemIndex;
  if i > 0 then dec(i);
  label13.Caption := inttostr(tbTrueBass.Position div 100) + ' %';
  fPlugin.Volume[i] := tbTrueBass.Position;
end;

procedure TfrmDSPTrueBass.cmbTrueBassChannelsChange(Sender: TObject);
var
  i : integer;
begin
  i := cmbTrueBassChannels.ItemIndex;
  fPlugin.Seperate := i <> 0;
  if fSaving <> nil then fSaving.ChannelPos := i;
  if i > 0 then dec(i);
  tbTrueBass.Position := fPlugin.Volume[i];
  tbTrueBassChange(Self);
end;

end.
