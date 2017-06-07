object frmDSPSound3D: TfrmDSPSound3D
  Left = 365
  Top = 250
  BorderStyle = bsNone
  Caption = 'frmDSPSound3D'
  ClientHeight = 490
  ClientWidth = 764
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
      Caption = ' Sound 3D '
      TabOrder = 0
      object Label13: TLabel
        Left = 335
        Top = 15
        Width = 37
        Height = 13
        Alignment = taRightJustify
        AutoSize = False
        Caption = '0'
      end
      object Label6: TLabel
        Left = 10
        Top = 15
        Width = 76
        Height = 13
        Caption = '3D Amplification'
      end
      object tb3DSound: TTrackBar
        Left = 3
        Top = 29
        Width = 374
        Height = 41
        Hint = '0 %'
        Max = 10000
        ParentShowHint = False
        Frequency = 250
        Position = 500
        ShowHint = False
        TabOrder = 0
        ThumbLength = 22
        TickMarks = tmBoth
        OnChange = tb3DSoundChange
      end
    end
  end
end
