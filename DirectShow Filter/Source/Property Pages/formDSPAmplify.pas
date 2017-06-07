unit formDSPAmplify;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, dspAmplify, StdCtrls, ComCtrls, DynamicFilterList;

type
  TMySavings = record
    ChannelPos : integer;
    Pitch : integer;
  end;
  PMySavings = ^TMySavings;

  TfrmDSPAmplify = class(TForm)
    TabControl2: TTabControl;
    GroupBox1: TGroupBox;
    Label12: TLabel;
    Label13: TLabel;
    Label1: TLabel;
    Label2: TLabel;
    PreAmp: TTrackBar;
    cmbAmpPitch: TComboBox;
    cmbAmpChannels: TComboBox;
    procedure PreAmpChange(Sender: TObject);
    procedure cmbAmpChannelsChange(Sender: TObject);
    procedure cmbAmpPitchChange(Sender: TObject);
  private
    fPlugin : TDCAmplify;
    fItem : TDCFilterItem;
    fSaving : PMySavings;
  public
    constructor CreateParented(ParentWindow: HWnd; Item : TDCFilterItem; Width : integer; Height : integer);
  end;

var
  frmDSPAmplify: TfrmDSPAmplify;

implementation

{$R *.dfm}

constructor TfrmDSPAmplify.CreateParented(ParentWindow: HWnd; Item : TDCFilterItem; Width : integer; Height : integer);
begin
  inherited CreateParented(ParentWindow);
  SetWindowPos(Handle,0,0,0,Width,Height,SWP_NOZORDER);
  fPlugin := TDCAmplify(Item.Filter);
  fItem := Item;
  fSaving := PMySavings(fItem.ExtraBuffer);
  if fSaving <> nil then
  begin
    cmbAmpPitch.ItemIndex := fSaving.ChannelPos;
    cmbAmpChannels.ItemIndex := fSaving.Pitch;
  end;
  cmbAmpPitchChange(Self);
  Show;
end;

procedure TfrmDSPAmplify.PreAmpChange(Sender: TObject);
var
  i : integer;
begin
  label12.Caption := inttostr(PreAmp.Position div 10) + ' %';
  i := cmbAmpChannels.ItemIndex;
  if i > 0 then dec(i);
  fPlugin.Volume[i] := PreAmp.Position * 10;
end;

procedure TfrmDSPAmplify.cmbAmpChannelsChange(Sender: TObject);
var
  i : integer;
begin
  i := cmbAmpChannels.ItemIndex;
  fPlugin.Seperate := i <> 0;
  if i > 0 then dec(i);
  if fSaving <> nil then fSaving.Pitch := cmbAmpChannels.ItemIndex;
  PreAmp.Position := fPlugin.Volume[i] div 10;
  PreAmpChange(Self);
end;

procedure TfrmDSPAmplify.cmbAmpPitchChange(Sender: TObject);
var
  i : integer;
  z : integer;
begin
  PreAmp.Max := (cmbAmpPitch.ItemIndex + 1) * 1000;
  PreAmp.Frequency := PreAmp.Max div 50;
  i := cmbAmpChannels.ItemIndex;
  if i > 0 then dec(i);
  z := fPlugin.Volume[i] div 10;
  if z > PreAmp.Max then z := PreAmp.Max;
  if fSaving <> nil then fSaving.ChannelPos := cmbAmpPitch.ItemIndex;
  PreAmp.Position := z;
  PreAmpChange(Self);
end;

end.
