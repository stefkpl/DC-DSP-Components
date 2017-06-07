unit formDSPChannelOrder;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, dspChannelOrder, StdCtrls, ComCtrls, DynamicFilterList, dspConst;

type
  TfrmDSPChannelOrder = class(TForm)
    TabControl2: TTabControl;
    GroupBox1: TGroupBox;
    Label8: TLabel;
    Label9: TLabel;
    cmbChannelSwitchIn: TComboBox;
    cmbChannelSwitchOut: TComboBox;
    sbDefaultChannel: TButton;
    GroupBox2: TGroupBox;
    Label1: TLabel;
    procedure cmbChannelSwitchInChange(Sender: TObject);
    procedure cmbChannelSwitchOutChange(Sender: TObject);
    procedure sbDefaultChannelClick(Sender: TObject);
    procedure UpdateChannelOrder;
  private
    fCurrentChannel : integer;
    fPlugin : TDCChannelOrder;
    fItem : TDCFilterItem;
  public
    constructor CreateParented(ParentWindow: HWnd; Item : TDCFilterItem; Width : integer; Height : integer);
  end;

var
  frmDSPChannelOrder: TfrmDSPChannelOrder;

implementation

{$R *.dfm}

constructor TfrmDSPChannelOrder.CreateParented(ParentWindow: HWnd; Item : TDCFilterItem; Width : integer; Height : integer);
begin
  inherited CreateParented(ParentWindow);
  SetWindowPos(Handle,0,0,0,Width,Height,SWP_NOZORDER);
  fPlugin := TDCChannelOrder(Item.Filter);
  fItem := Item;
  UpdateChannelOrder;
  Show;
end;

procedure TfrmDSPChannelOrder.cmbChannelSwitchInChange(Sender: TObject);
begin
  cmbChannelSwitchOut.ItemIndex := fPlugin.Order[cmbChannelSwitchIn.ItemIndex];
end;

procedure TfrmDSPChannelOrder.cmbChannelSwitchOutChange(Sender: TObject);
begin
  fCurrentChannel := cmbChannelSwitchIn.ItemIndex;
  fPlugin.Order[cmbChannelSwitchIn.ItemIndex] := cmbChannelSwitchOut.ItemIndex;
  UpdateChannelOrder;
end;

procedure TfrmDSPChannelOrder.sbDefaultChannelClick(Sender: TObject);
var
  i : integer;
begin
  for i := 0 to MaxChannels -1 do fPlugin.Order[i] := i;
  UpdateChannelOrder;
end;

procedure TfrmDSPChannelOrder.UpdateChannelOrder;
var
  i : integer;
  Str : String;
  tstr : String;
begin
  cmbChannelSwitchIn.Clear;
  cmbChannelSwitchOut.Clear;
  if fItem.Owner.Stream.Channels > 0 then
  begin
    Str := '';
    for i := 0 to fItem.Owner.Stream.Channels -1 do
    begin
      cmbChannelSwitchIn.Items.Add(inttostr(i+1));
      cmbChannelSwitchOut.Items.Add(inttostr(i+1));
      if i <> fItem.Owner.Stream.Channels -1 then tstr := ' - ' else tstr := '';
      if (i + 1) mod 4 <> 0 then
      begin
        Str := Str + ' Channel ' + inttostr(i+1) + ' -> ' + inttostr(fPlugin.Order[i] + 1) + tstr;
      end else
      begin
        Str := Str + ' Channel ' + inttostr(i+1) + ' -> ' + inttostr(fPlugin.Order[i] + 1) + #13#10;
      end;
    end;
    label1.Caption := Str;
    cmbChannelSwitchIn.ItemIndex := fCurrentChannel;
    cmbChannelSwitchInChange(Self);
  end;
end;

end.
