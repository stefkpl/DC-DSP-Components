unit formDSPSound3D;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, dspSound3D, StdCtrls, ComCtrls;

type
  TfrmDSPSound3D = class(TForm)
    TabControl2: TTabControl;
    GroupBox1: TGroupBox;
    Label13: TLabel;
    Label6: TLabel;
    tb3DSound: TTrackBar;
    procedure tb3DSoundChange(Sender: TObject);
  private
    fPlugin : TDCSound3D;
  public
    constructor CreateParented(ParentWindow: HWnd; Plugin : TDCSound3D; Width : integer; Height : integer);
  end;

var
  frmDSPSound3D: TfrmDSPSound3D;
  
implementation

{$R *.dfm}

constructor TfrmDSPSound3D.CreateParented(ParentWindow: HWnd; Plugin : TDCSound3D; Width : integer; Height : integer);
begin
  inherited CreateParented(ParentWindow);
  SetWindowPos(Handle,0,0,0,Width,Height,SWP_NOZORDER);
  fPlugin := Plugin;
  tb3DSound.Position := fPlugin.Volume;
  label13.Caption := inttostr((tb3DSound.Position) div 100) + ' %';
  Show;
end;

procedure TfrmDSPSound3D.tb3DSoundChange(Sender: TObject);
begin
  fPlugin.Volume := tb3DSound.Position;
  label13.Caption := inttostr((tb3DSound.Position) div 100) + ' %';
end;

end.
