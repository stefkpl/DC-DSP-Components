unit formDSPChannelOrder;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, DSPackTDCDSPFilter, dspConst;

type
  TfrmDSPChannelOrder = class(TForm)
    chkEnableChannelOrder: TCheckBox;
    Label8: TLabel;
    cmbChannelSwitchIn: TComboBox;
    Label9: TLabel;
    cmbChannelSwitchOut: TComboBox;
    sbDefaultChannel: TButton;
    Label1: TLabel;
    procedure chkEnableChannelOrderClick(Sender: TObject);
    procedure cmbChannelSwitchInChange(Sender: TObject);
    procedure cmbChannelSwitchOutChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure sbDefaultChannelClick(Sender: TObject);
  private
    { Private declarations }
    fCurrentChannel : integer;
    procedure UpdateChannelOrder;
  public
    { Public declarations }
  end;

var
  frmDSPChannelOrder: TfrmDSPChannelOrder;

implementation

uses formMain, dspChannelOrder;

{$R *.dfm}

procedure TfrmDSPChannelOrder.UpdateChannelOrder;
var
  i : integer;
  Stream : TDSStream;
  Str : String;
begin
  cmbChannelSwitchIn.Clear;
  cmbChannelSwitchOut.Clear;
  Stream := frmMain.DSPackDCDSPFilter.StreamInfo;
  if Stream.Channels > 0 then
  begin
    Str := 'Current Order';
    for i := 0 to Stream.Channels -1 do
    begin
      cmbChannelSwitchIn.Items.Add(inttostr(i+1));
      cmbChannelSwitchOut.Items.Add(inttostr(i+1));

      if (i + 1) mod 2 <> 0 then
      begin
        Str := Str + #13#10 + 'Channel ' + inttostr(i+1) + ' -> ' + inttostr(frmMain.DCChannelOrder.Order[i] + 1);
      end else
      begin
        Str := Str + '       Channel ' + inttostr(i+1) + ' -> ' + inttostr(frmMain.DCChannelOrder.Order[i] + 1);
      end;
    end;
    label1.Caption := Str;
    cmbChannelSwitchIn.ItemIndex := fCurrentChannel;
    cmbChannelSwitchInChange(Self);
  end;
end;

procedure TfrmDSPChannelOrder.chkEnableChannelOrderClick(Sender: TObject);
begin
  frmMain.DCChannelOrder.Enabled := chkEnableChannelOrder.Checked;
end;

procedure TfrmDSPChannelOrder.cmbChannelSwitchInChange(Sender: TObject);
begin
  cmbChannelSwitchOut.ItemIndex := frmMain.DCChannelOrder.Order[cmbChannelSwitchIn.ItemIndex];
end;

procedure TfrmDSPChannelOrder.cmbChannelSwitchOutChange(Sender: TObject);
begin
  fCurrentChannel := cmbChannelSwitchIn.ItemIndex;
  frmMain.DCChannelOrder.Order[cmbChannelSwitchIn.ItemIndex] := cmbChannelSwitchOut.ItemIndex;
  UpdateChannelOrder;
end;

procedure TfrmDSPChannelOrder.FormShow(Sender: TObject);
begin
  UpdateChannelOrder;
end;

procedure TfrmDSPChannelOrder.sbDefaultChannelClick(Sender: TObject);
var
  i : integer;
begin
  for i := 0 to MaxChannels -1 do frmMain.DCChannelOrder.Order[i] := i;
  UpdateChannelOrder;
end;

end.
