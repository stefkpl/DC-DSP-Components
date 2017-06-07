object frmDMOParamEQ: TfrmDMOParamEQ
  Left = 234
  Top = 247
  BorderStyle = bsNone
  Caption = 'frmDMOParamEQ'
  ClientHeight = 541
  ClientWidth = 914
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
      Caption = ' DMO Param EQ '
      TabOrder = 0
      object Label3: TLabel
        Left = 248
        Top = 15
        Width = 120
        Height = 13
        Alignment = taRightJustify
        AutoSize = False
        Caption = '0'
      end
      object Label10: TLabel
        Left = 10
        Top = 15
        Width = 33
        Height = 13
        Caption = 'Center'
      end
      object Label5: TLabel
        Left = 288
        Top = 71
        Width = 80
        Height = 13
        Alignment = taRightJustify
        AutoSize = False
        Caption = '0'
      end
      object Label12: TLabel
        Left = 10
        Top = 71
        Width = 50
        Height = 13
        Caption = 'Bandwidth'
      end
      object Label7: TLabel
        Left = 288
        Top = 127
        Width = 80
        Height = 13
        Alignment = taRightJustify
        AutoSize = False
        Caption = '0'
      end
      object Label14: TLabel
        Left = 10
        Top = 127
        Width = 21
        Height = 13
        Caption = 'Gain'
      end
      object tbCenter: TTrackBar
        Left = 3
        Top = 29
        Width = 374
        Height = 41
        Hint = '0 %'
        Max = 160000
        Min = 800
        ParentShowHint = False
        Frequency = 20
        Position = 800
        ShowHint = False
        TabOrder = 0
        ThumbLength = 22
        TickMarks = tmBoth
        OnChange = tbCenterChange
      end
      object tbBandWidth: TTrackBar
        Left = 3
        Top = 85
        Width = 374
        Height = 41
        Hint = '0 %'
        Max = 360
        Min = 10
        ParentShowHint = False
        Frequency = 8
        Position = 10
        ShowHint = False
        TabOrder = 1
        ThumbLength = 22
        TickMarks = tmBoth
        OnChange = tbBandWidthChange
      end
      object tbGain: TTrackBar
        Left = 3
        Top = 141
        Width = 374
        Height = 41
        Hint = '0 %'
        Max = 150
        Min = -150
        ParentShowHint = False
        Frequency = 7
        Position = 10
        ShowHint = False
        TabOrder = 2
        ThumbLength = 22
        TickMarks = tmBoth
        OnChange = tbGainChange
      end
    end
  end
end
