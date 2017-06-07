object frmDMOGargle: TfrmDMOGargle
  Left = 365
  Top = 250
  BorderStyle = bsNone
  Caption = 'frmDMOGargle'
  ClientHeight = 481
  ClientWidth = 748
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object TabControl2: TTabControl
    Left = -10
    Top = -137
    Width = 587
    Height = 423
    TabOrder = 0
    object GroupBox1: TGroupBox
      Left = 15
      Top = 138
      Width = 383
      Height = 267
      Caption = ' DMO Gargle '
      TabOrder = 0
      object Label3: TLabel
        Left = 272
        Top = 39
        Width = 100
        Height = 13
        Alignment = taRightJustify
        AutoSize = False
        Caption = '0'
      end
      object Label5: TLabel
        Left = 10
        Top = 39
        Width = 46
        Height = 13
        Caption = 'Rate (Hz)'
      end
      object Label1: TLabel
        Left = 8
        Top = 20
        Width = 60
        Height = 13
        Caption = 'Waveform : '
      end
      object tbRateHz: TTrackBar
        Left = 3
        Top = 53
        Width = 374
        Height = 41
        Hint = '0 %'
        Max = 1000
        Min = 1
        ParentShowHint = False
        Frequency = 20
        Position = 1
        ShowHint = False
        TabOrder = 0
        ThumbLength = 22
        TickMarks = tmBoth
        OnChange = tbRateHzChange
      end
      object rbSquare: TRadioButton
        Left = 144
        Top = 20
        Width = 57
        Height = 17
        Caption = 'Square'
        TabOrder = 1
        OnClick = rbSquareClick
      end
      object rbTriangle: TRadioButton
        Left = 216
        Top = 20
        Width = 89
        Height = 17
        Caption = 'Triangle'
        TabOrder = 2
        OnClick = rbTriangleClick
      end
    end
  end
end
