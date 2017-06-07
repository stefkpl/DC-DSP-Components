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
    procedure Render3;
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
  th, tw : integer;
begin
  bmp.Width  := ClientWidth;
  bmp.Height := ClientHeight;
  bmp.Canvas.Pen.Color := clBlack;
  bmp.Canvas.Brush.Color := clBlack;
  bmp.Canvas.Rectangle(bmp.Canvas.ClipRect);
  cal := ClientWidth / 512;
  bmp.Canvas.Pen.Color := fVisualColor;
  bmp.Canvas.Brush.Color := clBlack;
  for i := 0 to 511 do
  begin
    val := visual1.Plugin1.WaveformData[0,i] * Clientheight div 65536;
    bmp.Canvas.Ellipse(Round(cal * i) - 2,val-2,Round(cal * i) + 2,val +2);
  end;
  bmp.Canvas.Brush.Style := bsClear;
  bmp.Canvas.Font.Color := fVisualColor xor $00555555;
  bmp.Canvas.Font.Style := [fsBold];
  if TitleDraw < 0 then
  begin
    if Random(300) = 15 then
    begin
      tw := bmp.Canvas.TextWidth(MediaName);
      th := bmp.Canvas.TextHeight(MediaName);
      bmp.Canvas.Font.Size := 25;
      bmp.Canvas.TextOut((ClientWidth - tw) div 2,(ClientHeight - th) div 2,MediaName);
      TitleDraw := 80;
    end;
  end else
  begin
    bmp.Canvas.Font.Size := bmp.Canvas.Font.Size - 1;
    tw := bmp.Canvas.TextWidth(MediaName);
    th := bmp.Canvas.TextHeight(MediaName);
    bmp.Canvas.TextOut((ClientWidth - tw) div 2,(ClientHeight - th) div 2,MediaName);
    dec(TitleDraw)
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
  bmp.Canvas.Rectangle(0,0,bmp.Width,bmp.Height);
  bmp.Canvas.Pen.Color := fVisualColor;
  bmp.Canvas.Brush.Color := fVisualColor;
  cal := ClientWidth / 512;
  for i := 0 to 511 do
  begin
    val := visual1.Plugin2.WaveformData[0,i] * Clientheight div 65536;
    bmp.Canvas.Pixels[Round(i * cal),val] := fVisualColor;
  end;
  pb.Canvas.Draw(0,0,bmp);
end;

procedure TfrmVisual.Render3;
var
  i : integer;
  val : integer;
  cal : Single;
begin
  bmp.Width  := ClientWidth;
  bmp.Height := ClientHeight;
  bmp.Canvas.Pen.Color := clBlack;
  bmp.Canvas.Brush.Color := clBlack;
  bmp.Canvas.Rectangle(bmp.Canvas.ClipRect);
  cal := ClientWidth / 512;
  bmp.Canvas.Pen.Color := fVisualColor - Random(255) * $FFF;
  bmp.Canvas.Brush.Color := clBlack;
  for i := 0 to 511 do
  begin
    val := visual1.Plugin3.WaveformData[0,i] * Clientheight div 65536 div 2;
    if i = 0 then bmp.Canvas.MoveTo(0,val)
             else bmp.Canvas.LineTo(Round(i * cal),val);
  end;
  for i := 0 to 511 do
  begin
    val := (visual1.Plugin3.WaveformData[1,i] * Clientheight div 65536 div 2) + Clientheight div 2;
    if i = 0 then bmp.Canvas.MoveTo(0,val)
             else bmp.Canvas.LineTo(Round(i * cal),val);
  end;
  pb.Canvas.Draw(0,0,bmp);
end;

procedure TfrmVisual.FormDestroy(Sender: TObject);
begin
  bmp.Free;
end;

end.
