unit formDSPHighPass;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, dspHighPass, StdCtrls, ComCtrls, DynamicFilterList;

type
  TMySavings = record
    ChannelPos : integer;
  end;
  PMySavings = ^TMySavings;

  TfrmDSPHighPass = class(TForm)
    TabControl2: TTabControl;
    GroupBox1: TGroupBox;
    Label4: TLabel;
    Label1: TLabel;
    Label2: TLabel;
    tbHighPass: TTrackBar;
    cmbHighPassChannels: TComboBox;
    procedure tbHighPassChange(Sender: TObject);
    procedure cmbHighPassChannelsChange(Sender: TObject);
  private
    fPlugin : TDCHighPass;
    fItem : TDCFilterItem;
    fSaving : PMySavings;
    fStreamFrequency : integer;
  public
    constructor CreateParented(ParentWindow: HWnd; Item : TDCFilterItem; Width : integer; Height : integer);
  end;

var
  frmDSPHighPass: TfrmDSPHighPass;

implementation

{$R *.dfm}

constructor TfrmDSPHighPass.CreateParented(ParentWindow: HWnd; Item : TDCFilterItem; Width : integer; Height : integer);
begin
  inherited CreateParented(ParentWindow);
  SetWindowPos(Handle,0,0,0,Width,Height,SWP_NOZORDER);
  fPlugin := TDCHighPass(Item.Filter);
  fItem := Item;
  fSaving := PMySavings(fItem.ExtraBuffer);
  fStreamFrequency := fItem.Owner.Stream.Frequency div 2;
  if fSaving <> nil then
  begin
    cmbHighPassChannels.ItemIndex := fSaving.ChannelPos;
    cmbHighPassChannelsChange(Self);
  end;
  Show;
end;

procedure TfrmDSPHighPass.tbHighPassChange(Sender: TObject);
var
  i : integer;
begin
  label4.Caption := inttostr((tbHighPass.Position * fStreamFrequency) div 10000) + ' - ' + inttostr(fStreamFrequency) + ' Hz';
  i := cmbHighPassChannels.ItemIndex;
  if i > 0 then dec(i);
  fPlugin.Cutoff[i] := tbHighPass.Position;
end;

procedure TfrmDSPHighPass.cmbHighPassChannelsChange(Sender: TObject);
var
  i : integer;
begin
  i := cmbHighPassChannels.ItemIndex;
  fPlugin.Seperate := i <> 0;
  if fSaving <> nil then fSaving.ChannelPos := cmbHighPassChannels.ItemIndex;
  if i > 0 then dec(i);
  tbHighPass.Position := fPlugin.Cutoff[i];
  tbHighPassChange(Self);
end;

end.
