unit formDMOParamEQ;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, dmoParamEQ, StdCtrls, ComCtrls, DynamicFilterList;

type
  TfrmDMOParamEQ = class(TForm)
    TabControl2: TTabControl;
    GroupBox1: TGroupBox;
    Label3: TLabel;
    Label10: TLabel;
    Label5: TLabel;
    Label12: TLabel;
    Label7: TLabel;
    Label14: TLabel;
    tbCenter: TTrackBar;
    tbBandWidth: TTrackBar;
    tbGain: TTrackBar;
    procedure tbCenterChange(Sender: TObject);
    procedure tbBandWidthChange(Sender: TObject);
    procedure tbGainChange(Sender: TObject);
  private
    fPlugin : TDCDMOParamEQ;
    fItem : TDCFilterItem;
  public
    constructor CreateParented(ParentWindow: HWnd; Item : TDCFilterItem; Width : integer; Height : integer);
  end;

var
  frmDMOParamEQ: TfrmDMOParamEQ;

implementation

{$R *.dfm}

constructor TfrmDMOParamEQ.CreateParented(ParentWindow: HWnd; Item : TDCFilterItem; Width : integer; Height : integer);
begin
  inherited CreateParented(ParentWindow);
  SetWindowPos(Handle,0,0,0,Width,Height,SWP_NOZORDER);
  fPlugin := TDCDMOParamEQ(Item.Filter);
  fItem := Item;
  tbCenter.Position := Round(fPlugin.Center * 10);
  tbCenterChange(Self);
  tbBandwidth.Position := Round(fPlugin.Bandwidth * 10);
  tbBandWidthChange(Self);
  tbGain.Position := Round(fPlugin.Gain * 10);
  tbGainChange(Self);
  Show;
end;

procedure TfrmDMOParamEQ.tbCenterChange(Sender: TObject);
begin
  fPlugin.Center := tbCenter.Position / 10;
  label3.Caption := Format('%.1f Hz', [tbCenter.Position / 10]);
end;

procedure TfrmDMOParamEQ.tbBandWidthChange(Sender: TObject);
begin
  fPlugin.Bandwidth := tbBandwidth.Position / 10;
  label5.Caption := Format('%.1f Hz', [tbBandwidth.Position / 10]);
end;

procedure TfrmDMOParamEQ.tbGainChange(Sender: TObject);
begin
  fPlugin.Gain := tbGain.Position / 10;
  label7.Caption := Format('%.1f db', [tbGain.Position / 10]);
end;

end.
