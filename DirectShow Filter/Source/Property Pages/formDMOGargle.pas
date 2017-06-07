unit formDMOGargle;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, dmoGargle, StdCtrls, ComCtrls, DynamicFilterList, DirectSound;

type
  TfrmDMOGargle = class(TForm)
    TabControl2: TTabControl;
    GroupBox1: TGroupBox;
    Label3: TLabel;
    Label5: TLabel;
    Label1: TLabel;
    tbRateHz: TTrackBar;
    rbSquare: TRadioButton;
    rbTriangle: TRadioButton;
    procedure rbSquareClick(Sender: TObject);
    procedure rbTriangleClick(Sender: TObject);
    procedure tbRateHzChange(Sender: TObject);
  private
    fPlugin : TDCDMOGargle;
    fItem : TDCFilterItem;
  public
    constructor CreateParented(ParentWindow: HWnd; Item : TDCFilterItem; Width : integer; Height : integer);
  end;

var
  frmDMOGargle: TfrmDMOGargle;

implementation

{$R *.dfm}

constructor TfrmDMOGargle.CreateParented(ParentWindow: HWnd; Item : TDCFilterItem; Width : integer; Height : integer);
begin
  inherited CreateParented(ParentWindow);
  SetWindowPos(Handle,0,0,0,Width,Height,SWP_NOZORDER);
  fPlugin := TDCDMOGargle(Item.Filter);
  fItem := Item;
  rbSquare.Checked := fPlugin.WaveShape = DSFXGARGLE_WAVE_SQUARE;
  rbTriangle.Checked := fPlugin.WaveShape = DSFXGARGLE_WAVE_TRIANGLE;
  tbRateHz.Position := fPlugin.RateHz;
  Show;
end;

procedure TfrmDMOGargle.rbSquareClick(Sender: TObject);
begin
  fPlugin.WaveShape := DSFXGARGLE_WAVE_SQUARE;
end;

procedure TfrmDMOGargle.rbTriangleClick(Sender: TObject);
begin
  fPlugin.WaveShape := DSFXGARGLE_WAVE_TRIANGLE;
end;

procedure TfrmDMOGargle.tbRateHzChange(Sender: TObject);
begin
  fPlugin.RateHz := tbRateHz.Position;
  label3.Caption := inttostr(tbRateHz.Position);
end;

end.
