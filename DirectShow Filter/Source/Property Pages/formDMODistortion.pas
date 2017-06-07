unit formDMODistortion;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, dmoDistortion, StdCtrls, ComCtrls, DynamicFilterList;

type
  TfrmDMODistortion = class(TForm)
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
    tbGain: TTrackBar;
    tbEdge: TTrackBar;
    tbFrequency: TTrackBar;
    tbBandwidth: TTrackBar;
    tbCutoff: TTrackBar;
    procedure tbGainChange(Sender: TObject);
    procedure tbEdgeChange(Sender: TObject);
    procedure tbFrequencyChange(Sender: TObject);
    procedure tbBandwidthChange(Sender: TObject);
    procedure tbCutoffChange(Sender: TObject);
  private
    fPlugin : TDCDMODistortion;
    fItem : TDCFilterItem;
  public
    constructor CreateParented(ParentWindow: HWnd; Item : TDCFilterItem; Width : integer; Height : integer);
  end;

var
  frmDMODistortion: TfrmDMODistortion;

implementation

{$R *.dfm}

constructor TfrmDMODistortion.CreateParented(ParentWindow: HWnd; Item : TDCFilterItem; Width : integer; Height : integer);
begin
  inherited CreateParented(ParentWindow);
  SetWindowPos(Handle,0,0,0,Width,Height,SWP_NOZORDER);
  fPlugin := TDCDMODistortion(Item.Filter);
  fItem := Item;
  tbGain.Position := Round(fPlugin.Gain * 10);
  tbEdge.Position := Round(fPlugin.Edge * 10);
  tbFrequency.Position := Round(fPlugin.PostEQCenterFrequency * 10);
  tbBandwidth.Position := Round(fPlugin.PostEQBandwidth * 10);
  tbCutoff.Position := Round(fPlugin.PreLowpassCutoff * 10);
  Show;
end;

procedure TfrmDMODistortion.tbGainChange(Sender: TObject);
begin
  fPlugin.Gain := tbGain.Position / 10;
  label3.Caption := Format('%.1f', [tbGain.Position / 10]);
end;

procedure TfrmDMODistortion.tbEdgeChange(Sender: TObject);
begin
  fPlugin.Edge := tbEdge.Position / 10;
  label5.Caption := Format('%.1f', [tbEdge.Position / 10]);
end;

procedure TfrmDMODistortion.tbFrequencyChange(Sender: TObject);
begin
  fPlugin.PostEQCenterFrequency := tbFrequency.Position / 10;
  label7.Caption := Format('%.1f', [tbFrequency.Position / 10]);
end;

procedure TfrmDMODistortion.tbBandwidthChange(Sender: TObject);
begin
  fPlugin.PostEQBandwidth := tbBandwidth.Position / 10;
  label9.Caption := Format('%.1f', [tbBandwidth.Position / 10]);
end;

procedure TfrmDMODistortion.tbCutoffChange(Sender: TObject);
begin
  fPlugin.PreLowpassCutoff := tbCutoff.Position / 10;
  label11.Caption := Format('%.1f', [tbCutoff.Position / 10]);
end;

end.
