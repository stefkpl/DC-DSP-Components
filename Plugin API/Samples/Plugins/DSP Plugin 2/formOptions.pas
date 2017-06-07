unit formOptions;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls;

type
  TfrmOptions = class(TForm)
    Label1: TLabel;
    TrackBar1: TTrackBar;
    procedure TrackBar1Change(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmOptions: TfrmOptions;

implementation

uses dsp1;

{$R *.dfm}

procedure TfrmOptions.TrackBar1Change(Sender: TObject);
begin
  fSound3D.Volume := TrackBar1.Position * 10;
end;

procedure TfrmOptions.FormShow(Sender: TObject);
begin
  TrackBar1.Position := fSound3D.Volume div 10;
end;

end.
