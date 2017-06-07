unit formDSPBandPass;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, dspBandPass, StdCtrls, ComCtrls, DynamicFilterList;

type
  TMySavings = record
    ChannelPos : integer;
  end;
  PMySavings = ^TMySavings;

  TfrmDSPBandPass = class(TForm)
    TabControl2: TTabControl;
    GroupBox1: TGroupBox;
    Label10: TLabel;
    Label1: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label2: TLabel;
    tbLow: TTrackBar;
    cmbBandPassChannels: TComboBox;
    tbHigh: TTrackBar;
    procedure cmbBandPassChannelsChange(Sender: TObject);
    procedure tbLowChange(Sender: TObject);
  private
    fPlugin : TDCBandpass;
    fItem : TDCFilterItem;
    fSaving : PMySavings;
    fCanChange : Boolean;
  public
    constructor CreateParented(ParentWindow: HWnd; Item : TDCFilterItem; Width : integer; Height : integer);
  end;

var
  frmDSPBandPass: TfrmDSPBandPass;

implementation

{$R *.dfm}

constructor TfrmDSPBandPass.CreateParented(ParentWindow: HWnd; Item : TDCFilterItem; Width : integer; Height : integer);
begin
  inherited CreateParented(ParentWindow);
  fCanChange := False;
  SetWindowPos(Handle,0,0,0,Width,Height,SWP_NOZORDER);
  fPlugin := TDCBandpass(Item.Filter);
  fItem := Item;
  fSaving := PMySavings(fItem.ExtraBuffer);
  if fSaving <> nil then cmbBandPassChannels.ItemIndex := fSaving.ChannelPos;
  tbLow.Max := Item.Owner.Stream.Frequency div 2;
  tbHigh.Max := Item.Owner.Stream.Frequency div 2;
  fCanChange := True;
  cmbBandPassChannelsChange(Self);
  Show;
end;

procedure TfrmDSPBandPass.cmbBandPassChannelsChange(Sender: TObject);
var
  i : integer;
begin
  i := cmbBandPassChannels.ItemIndex;
  fPlugin.Seperate := i <> 0;
  if i > 0 then dec(i);
  fCanChange := False;
  tbLow.Position := Round(fPlugin.CutoffLow[i]);
  tbHigh.Position := Round(fPlugin.CutoffHigh[i]);
  if fSaving <> nil then fSaving.ChannelPos := cmbBandPassChannels.ItemIndex;
  tbLowChange(Self);
  fCanChange := True;
end;

procedure TfrmDSPBandPass.tbLowChange(Sender: TObject);
var
  i  :integer;
begin
  label3.Caption := inttostr(tbHigh.Position) + ' Hz';
  label2.Caption := inttostr(tbLow.Position) + ' Hz';
  if not fCanChange then Exit;
  i := cmbBandPassChannels.ItemIndex;
  if i > 0 then dec(i);
  fPlugin.CutoffLow[i] := tbLow.Position;
  fPlugin.CutoffHigh[i] := tbHigh.Position;
end;

end.
