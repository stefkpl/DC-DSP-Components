object frmDMOGargle: TfrmDMOGargle
  Left = 189
  Top = 160
  BorderStyle = bsToolWindow
  Caption = ' DMO Gargle'
  ClientHeight = 111
  ClientWidth = 366
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
  object SpeedButton1: TSpeedButton
    Left = 240
    Top = 80
    Width = 121
    Height = 22
    Caption = 'Show Property Page'
    OnClick = SpeedButton1Click
  end
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 58
    Height = 13
    Caption = 'Waveform : '
  end
  object Label2: TLabel
    Left = 8
    Top = 45
    Width = 39
    Height = 13
    Caption = 'Rate Hz'
  end
  object Label3: TLabel
    Left = 320
    Top = 45
    Width = 32
    Height = 13
    Caption = 'Label3'
  end
  object rbSquare: TRadioButton
    Left = 128
    Top = 8
    Width = 57
    Height = 17
    Caption = 'Square'
    TabOrder = 0
    OnClick = rbSquareClick
  end
  object rbTriangle: TRadioButton
    Left = 200
    Top = 8
    Width = 89
    Height = 17
    Caption = 'Triangle'
    TabOrder = 1
    OnClick = rbTriangleClick
  end
  object tbRateHz: TTrackBar
    Left = 80
    Top = 40
    Width = 225
    Height = 33
    Max = 1000
    Min = 1
    Frequency = 20
    Position = 1
    TabOrder = 2
    OnChange = tbRateHzChange
  end
  object chkEnabled: TCheckBox
    Left = 8
    Top = 88
    Width = 73
    Height = 17
    Caption = 'Enabled'
    TabOrder = 3
    OnClick = chkEnabledClick
  end
end
