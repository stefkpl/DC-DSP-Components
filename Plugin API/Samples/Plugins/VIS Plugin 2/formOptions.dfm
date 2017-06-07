object frmOptions: TfrmOptions
  Left = 265
  Top = 228
  BorderStyle = bsNone
  Caption = 'frmOptions'
  ClientHeight = 65
  ClientWidth = 185
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 8
    Top = 8
    Width = 49
    Height = 25
    TabOrder = 0
    object Shape1: TShape
      Left = 1
      Top = 1
      Width = 47
      Height = 23
      Align = alClient
    end
  end
  object Button1: TButton
    Left = 64
    Top = 8
    Width = 81
    Height = 25
    Caption = 'Select Colour'
    TabOrder = 1
    OnClick = Button1Click
  end
  object ColorDialog1: TColorDialog
    Left = 152
    Top = 8
  end
end
