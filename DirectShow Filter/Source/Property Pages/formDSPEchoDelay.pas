unit formDSPEchoDelay;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, dspEchoDelay, StdCtrls, ComCtrls, Spin, DynamicFilterList;

type
  TfrmDSPEchoDelay = class(TForm)
    TabControl2: TTabControl;
    GroupBox1: TGroupBox;
    label6: TLabel;
    Label2: TLabel;
    label8: TLabel;
    Label3: TLabel;
    Label9: TLabel;
    tbDelay: TTrackBar;
    tbDelayGain: TTrackBar;
    spNumEchos: TSpinEdit;
    chkEchoHighPass: TCheckBox;
    chkKillMain: TCheckBox;
    procedure tbDelayChange(Sender: TObject);
    procedure tbDelayGainChange(Sender: TObject);
    procedure chkKillMainClick(Sender: TObject);
    procedure chkEchoHighPassClick(Sender: TObject);
    procedure spNumEchosChange(Sender: TObject);
  private
    fCanChange : Boolean;
    fPlugin : TDCEchoDelay;
    fItem : TDCFilterItem;
  public
    constructor CreateParented(ParentWindow: HWnd; Item : TDCFilterItem; Width : integer; Height : integer);
  end;

var
  frmDSPEchoDelay: TfrmDSPEchoDelay;

implementation

{$R *.dfm}

constructor TfrmDSPEchoDelay.CreateParented(ParentWindow: HWnd; Item : TDCFilterItem; Width : integer; Height : integer);
begin
  inherited CreateParented(ParentWindow);
  SetWindowPos(Handle,0,0,0,Width,Height,SWP_NOZORDER);
  fPlugin := TDCEchoDelay(Item.Filter);
  fItem := Item;
  fCanChange := False;
  spNumEchos.Value := fPlugin.NumDelays;
  chkKillMain.Checked := fPlugin.KillMain;
  tbDelayGain.Position := fPlugin.DelayAmp;
  tbDelayGainChange(Self);
  tbDelay.Position := fPlugin.Delay;
  tbDelayChange(Self);
  chkEchoHighPass.Checked := fPlugin.Highpass;
  fCanChange := True;
  Show;
end;

procedure TfrmDSPEchoDelay.tbDelayChange(Sender: TObject);
begin
  if fCanChange then fPlugin.Delay := tbDelay.Position;
  label6.Caption := inttostr(tbDelay.position div 10) + ' ms';
end;

procedure TfrmDSPEchoDelay.tbDelayGainChange(Sender: TObject);
begin
  fPlugin.DelayAmp := tbDelayGain.Position;
  label8.Caption := inttostr(tbDelayGain.position div 100) + ' %';
end;

procedure TfrmDSPEchoDelay.chkKillMainClick(Sender: TObject);
begin
  fPlugin.KillMain := chkKillMain.Checked;
end;

procedure TfrmDSPEchoDelay.chkEchoHighPassClick(Sender: TObject);
begin
  fPlugin.Highpass := chkEchoHighPass.Checked;
end;

procedure TfrmDSPEchoDelay.spNumEchosChange(Sender: TObject);
begin
  fPlugin.NumDelays := spNumEchos.Value;
end;

end.
