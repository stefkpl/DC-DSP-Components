unit formDSPDownMix;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, dspDownMix, StdCtrls, ComCtrls, DynamicFilterList;

type
  TfrmDSPDownMix = class(TForm)
    TabControl2: TTabControl;
    GroupBox1: TGroupBox;
    Label1: TLabel;
  private
  public
    constructor CreateParented(ParentWindow: HWnd; Item : TDCFilterItem; Width : integer; Height : integer);
  end;

var
  frmDSPDownMix: TfrmDSPDownMix;

implementation

{$R *.dfm}

constructor TfrmDSPDownMix.CreateParented(ParentWindow: HWnd; Item : TDCFilterItem; Width : integer; Height : integer);
begin
  inherited CreateParented(ParentWindow);
  SetWindowPos(Handle,0,0,0,Width,Height,SWP_NOZORDER);
  Show;
end;

end.
