unit formVisual;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

type
  TfrmVisual = class(TForm)
    pb: TPaintBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    bmp : TBitmap;
  public
    procedure Render1;
    procedure Render2;
  end;

var
  frmVisual: TfrmVisual;
  TitleDraw : integer = 0;

implementation

uses
  visual1;

{$R *.dfm}

procedure TfrmVisual.FormCreate(Sender: TObject);
begin
  bmp := TBitmap.Create;
  bmp.PixelFormat := pf24bit;
  bmp.Width  := ClientWidth;
  bmp.Height := ClientHeight;
  bmp.Canvas.Pen.Color := clBlack;
  bmp.Canvas.Brush.Color := clBlack;
  bmp.Canvas.Rectangle(0,0,bmp.Width,bmp.Height);
  pb.Align := alClient;
end;

procedure TfrmVisual.Render1;
var
  i : integer;
  val : integer;
  cal : Single;
begin
  bmp.Width  := ClientWidth;
  bmp.Height := ClientHeight;
  bmp.Canvas.Pen.Color := clBlack;
  bmp.Canvas.Brush.Color := clBlack;
  cal := ClientWidth / 512;
  bmp.Canvas.Rectangle(bmp.Canvas.ClipRect);
  bmp.Canvas.Pen.Color := fVisualColor;
  bmp.Canvas.Brush.Color := clBlack;
  bmp.Canvas.Font.Color := fVisualColor;
  bmp.Canvas.Font.Size := 15;
  bmp.Canvas.TextOut(10,10,MediaName);
  for i := 0 to 511 do
  begin
    val := visual1.Plugin1.SpectrumData[0,i] * Clientheight div 65536;
    bmp.Canvas.MoveTo(Round(i * cal),Clientheight);
    bmp.Canvas.LineTo(Round(i * cal),Clientheight - val);
  end;
  pb.Canvas.Draw(0,0,bmp);
end;

procedure TfrmVisual.Render2;
var
  i : integer;
  val : integer;
  cal : Single;
begin
  bmp.Width  := ClientWidth;
  bmp.Height := ClientHeight;
  bmp.Canvas.Pen.Color := clBlack;
  bmp.Canvas.Brush.Color := clBlack;
  cal := ClientWidth / 512;
  bmp.Canvas.Rectangle(bmp.Canvas.ClipRect);
  bmp.Canvas.Pen.Color := fVisualColor - Random(255) * $FFF;
  bmp.Canvas.Brush.Color := clBlack;
  for i := 0 to 511 do
  begin
    val := visual1.Plugin2.SpectrumData[0,i] * Clientheight div 65536 div 2;
    bmp.Canvas.MoveTo(Round(i * cal),Clientheight);
    bmp.Canvas.LineTo(Round(i * cal),Clientheight - val);
  end;
  for i := 0 to 511 do
  begin
    val := (visual1.Plugin2.SpectrumData[1,i] * Clientheight div 65536 div 2) + Clientheight div 2;
    bmp.Canvas.MoveTo(Round(i * cal),Clientheight div 2);
    bmp.Canvas.LineTo(Round(i * cal),Clientheight - val);
  end;
  pb.Canvas.Draw(0,0,bmp);
end;

procedure TfrmVisual.FormDestroy(Sender: TObject);
begin
  bmp.Free;
end;

end.
