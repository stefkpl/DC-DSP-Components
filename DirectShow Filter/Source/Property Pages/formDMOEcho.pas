unit formDMOEcho;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, dmoEcho, StdCtrls, ComCtrls, DynamicFilterList, DirectSound;

type
  TfrmDMOEcho = class(TForm)
    TabControl2: TTabControl;
    GroupBox1: TGroupBox;
    Label3: TLabel;
    Label15: TLabel;
    Label5: TLabel;
    Label17: TLabel;
    Label7: TLabel;
    Label19: TLabel;
    Label9: TLabel;
    Label21: TLabel;
    Label10: TLabel;
    tbWetDryMix: TTrackBar;
    tbFeedback: TTrackBar;
    tbLeftDelay: TTrackBar;
    tbRightDelay: TTrackBar;
    rbNormalPan: TRadioButton;
    rbSwapChannels: TRadioButton;
    procedure tbFeedbackChange(Sender: TObject);
    procedure rbNormalPanClick(Sender: TObject);
    procedure rbSwapChannelsClick(Sender: TObject);
    procedure tbWetDryMixChange(Sender: TObject);
    procedure tbLeftDelayChange(Sender: TObject);
    procedure tbRightDelayChange(Sender: TObject);
  private
    fPlugin : TDCDMOEcho;
    fItem : TDCFilterItem;
  public
    constructor CreateParented(ParentWindow: HWnd; Item : TDCFilterItem; Width : integer; Height : integer);
  end;

var
  frmDMOEcho: TfrmDMOEcho;

implementation

{$R *.dfm}

constructor TfrmDMOEcho.CreateParented(ParentWindow: HWnd; Item : TDCFilterItem; Width : integer; Height : integer);
begin
  inherited CreateParented(ParentWindow);
  SetWindowPos(Handle,0,0,0,Width,Height,SWP_NOZORDER);
  fPlugin := TDCDMOEcho(Item.Filter);
  fItem := Item;
  rbNormalPan.Checked := fPlugin.PanDelay = DSFXECHO_PANDELAY_MIN;
  rbSwapChannels.Checked := fPlugin.PanDelay = DSFXECHO_PANDELAY_MAX;
  tbWetDryMix.Position := Round(fPlugin.WetDryMix * 10);
  tbFeedback.Position := Round(fPlugin.Feedback * 10);
  tbLeftDelay.Position := Round(fPlugin.LeftDelay * 10);
  tbRightDelay.Position := Round(fPlugin.RightDelay * 10);
  Show;
end;

procedure TfrmDMOEcho.tbFeedbackChange(Sender: TObject);
begin
  fPlugin.Feedback := tbFeedback.Position / 10;
  label5.Caption := Format('%.1f', [tbFeedback.Position / 10]);
end;

procedure TfrmDMOEcho.rbNormalPanClick(Sender: TObject);
begin
  fPlugin.PanDelay := DSFXECHO_PANDELAY_MIN;
end;

procedure TfrmDMOEcho.rbSwapChannelsClick(Sender: TObject);
begin
  fPlugin.PanDelay := DSFXECHO_PANDELAY_MAX;
end;

procedure TfrmDMOEcho.tbWetDryMixChange(Sender: TObject);
begin
  fPlugin.WetDryMix := tbWetDryMix.Position / 10;
  label3.Caption := Format('%.1f', [tbWetDryMix.Position / 10]);
end;

procedure TfrmDMOEcho.tbLeftDelayChange(Sender: TObject);
begin
  fPlugin.LeftDelay := tbLeftDelay.Position / 10;
  label7.Caption := Format('%.1f', [tbLeftDelay.Position / 10]);
end;

procedure TfrmDMOEcho.tbRightDelayChange(Sender: TObject);
begin
  fPlugin.RightDelay := tbRightDelay.Position / 10;
  label9.Caption := Format('%.1f', [tbRightDelay.Position / 10]);
end;

end.
