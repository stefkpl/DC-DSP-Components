unit formDSPPitchScale;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, dspPitchScale, StdCtrls, ComCtrls, Spin, DynamicFilterList, dspConst;

type
  TfrmDSPPitchScale = class(TForm)
    TabControl2: TTabControl;
    GroupBox1: TGroupBox;
    Label13: TLabel;
    Label6: TLabel;
    Label1: TLabel;
    Label2: TLabel;
    tbPitchScale: TTrackBar;
    cmbFFT: TComboBox;
    spQuality: TSpinEdit;
    procedure tbPitchScaleChange(Sender: TObject);
    procedure cmbFFTChange(Sender: TObject);
    procedure spQualityChange(Sender: TObject);
  private
    fPlugin : TDCPitchScale;
    fItem : TDCFilterItem;
  public
    constructor CreateParented(ParentWindow: HWnd; Item : TDCFilterItem; Width : integer; Height : integer);
  end;

var
  frmDSPPitchScale: TfrmDSPPitchScale;

implementation

{$R *.dfm}

constructor TfrmDSPPitchScale.CreateParented(ParentWindow: HWnd; Item : TDCFilterItem; Width : integer; Height : integer);
begin
  inherited CreateParented(ParentWindow);
  SetWindowPos(Handle,0,0,0,Width,Height,SWP_NOZORDER);
  fPlugin := TDCPitchScale(Item.Filter);
  fItem := Item;
  spQuality.Value := fPlugin.Quality[0];
  cmbFFT.ItemIndex := integer(fPlugin.FFTSize);
  tbPitchScale.Position := fPlugin.Pitch[0];
  label13.Caption := Format('%.2fx', [tbPitchScale.Position / 1000]);
  Show;
end;

procedure TfrmDSPPitchScale.tbPitchScaleChange(Sender: TObject);
begin
  label13.Caption := Format('%.2fx', [tbPitchScale.Position / 1000]);
  fPlugin.Pitch[0] := tbPitchScale.Position;
end;

procedure TfrmDSPPitchScale.cmbFFTChange(Sender: TObject);
begin
  fPlugin.FFTSize := TDCFFTSize(cmbFFT.ItemIndex);
end;

procedure TfrmDSPPitchScale.spQualityChange(Sender: TObject);
begin
  fPlugin.Quality[0] := spQuality.Value;
end;

end.
