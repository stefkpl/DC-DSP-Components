unit formOptions;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

type
  TfrmOptions = class(TForm)
    Panel1: TPanel;
    Button1: TButton;
    ColorDialog1: TColorDialog;
    Shape1: TShape;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmOptions: TfrmOptions;

implementation

uses visual1;

{$R *.dfm}

procedure TfrmOptions.FormCreate(Sender: TObject);
begin
  shape1.Brush.Color := fVisualColor;
end;

procedure TfrmOptions.Button1Click(Sender: TObject);
begin
  ColorDialog1.Color := shape1.brush.Color;
  if ColorDialog1.Execute then
  begin
    shape1.brush.Color := ColorDialog1.Color;
    fVisualColor := ColorDialog1.Color;
  end;
end;

end.
