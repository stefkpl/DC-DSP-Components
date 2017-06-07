unit formOptions2;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

type
  TfrmOptions2 = class(TForm)
    CheckBox1: TCheckBox;
    procedure FormShow(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmOptions2: TfrmOptions2;

implementation

uses dsp1;

{$R *.dfm}

procedure TfrmOptions2.FormShow(Sender: TObject);
begin
  CheckBox1.Checked := fDownMix.Enabled;
end;

procedure TfrmOptions2.CheckBox1Click(Sender: TObject);
begin
  fDownMix.Enabled := CheckBox1.Checked;
end;

end.
