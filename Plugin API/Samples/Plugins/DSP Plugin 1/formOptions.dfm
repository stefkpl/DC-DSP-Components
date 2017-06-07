object frmOptions: TfrmOptions
  Left = 265
  Top = 228
  BorderStyle = bsNone
  Caption = 'frmOptions'
  ClientHeight = 68
  ClientWidth = 360
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 8
    Width = 35
    Height = 13
    Caption = 'Volume'
  end
  object TrackBar1: TTrackBar
    Left = 8
    Top = 24
    Width = 345
    Height = 32
    Max = 1000
    Frequency = 20
    Position = 1000
    TabOrder = 0
    OnChange = TrackBar1Change
  end
end
