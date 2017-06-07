unit formDMOCompressor;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, dmoCompressor, StdCtrls, ComCtrls, DynamicFilterList;

type
  TfrmDMOCompressor = class(TForm)
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
    Label11: TLabel;
    Label23: TLabel;
    Label24: TLabel;
    Label13: TLabel;
    tbGain: TTrackBar;
    tbAttack: TTrackBar;
    tbRelease: TTrackBar;
    tbThreshold: TTrackBar;
    tbRatio: TTrackBar;
    tbPreDelay: TTrackBar;
    procedure tbGainChange(Sender: TObject);
    procedure tbAttackChange(Sender: TObject);
    procedure tbReleaseChange(Sender: TObject);
    procedure tbThresholdChange(Sender: TObject);
    procedure tbRatioChange(Sender: TObject);
    procedure tbPreDelayChange(Sender: TObject);
  private
    fPlugin : TDCDMOCompressor;
    fItem : TDCFilterItem;
  public
    constructor CreateParented(ParentWindow: HWnd; Item : TDCFilterItem; Width : integer; Height : integer);
  end;

var
  frmDMOCompressor: TfrmDMOCompressor;

implementation

{$R *.dfm}

constructor TfrmDMOCompressor.CreateParented(ParentWindow: HWnd; Item : TDCFilterItem; Width : integer; Height : integer);
begin
  inherited CreateParented(ParentWindow);
  SetWindowPos(Handle,0,0,0,Width,Height,SWP_NOZORDER);
  fPlugin := TDCDMOCompressor(Item.Filter);
  fItem := Item;
  tbGain.Position := Round(fPlugin.Gain * 10);
  tbAttack.Position := Round(fPlugin.Attack * 100);
  tbRelease.Position := Round(fPlugin.Release * 10);
  tbThreshold.Position := Round(fPlugin.Threshold * 10);
  tbRatio.Position := Round(fPlugin.Ratio * 10);
  tbPreDelay.Position := Round(fPlugin.PreDelay * 10);
  Show;
end;

procedure TfrmDMOCompressor.tbGainChange(Sender: TObject);
begin
  fPlugin.Gain := tbGain.Position / 10;
  label3.Caption := Format('%.1f', [tbGain.Position / 10]);
end;

procedure TfrmDMOCompressor.tbAttackChange(Sender: TObject);
begin
  fPlugin.Attack := tbAttack.Position / 100;
  label5.Caption := Format('%.1f', [tbAttack.Position / 100]);
end;

procedure TfrmDMOCompressor.tbReleaseChange(Sender: TObject);
begin
  fPlugin.Release := tbRelease.Position / 10;
  label7.Caption := Format('%.1f', [tbRelease.Position / 10]);
end;

procedure TfrmDMOCompressor.tbThresholdChange(Sender: TObject);
begin
  fPlugin.Threshold := tbThreshold.Position / 10;
  label9.Caption := Format('%.1f', [tbThreshold.Position / 10]);
end;

procedure TfrmDMOCompressor.tbRatioChange(Sender: TObject);
begin
  fPlugin.Ratio := tbRatio.Position / 10;
  label11.Caption := Format('%.1f', [tbRatio.Position / 10]);
end;

procedure TfrmDMOCompressor.tbPreDelayChange(Sender: TObject);
begin
  fPlugin.PreDelay := tbPreDelay.Position / 10;
  label13.Caption := Format('%.1f', [tbPreDelay.Position / 10]);
end;

end.
