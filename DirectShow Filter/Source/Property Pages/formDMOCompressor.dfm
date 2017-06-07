object frmDMOCompressor: TfrmDMOCompressor
  Left = 280
  Top = 140
  BorderStyle = bsNone
  Caption = 'frmDMOCompressor'
  ClientHeight = 454
  ClientWidth = 709
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
      Caption = ' DMO Compressor '
      TabOrder = 0
      object Label3: TLabel
        Left = 314
        Top = 27
        Width = 52
        Height = 13
        Alignment = taRightJustify
        AutoSize = False
        Caption = '0'
      end
      object Label15: TLabel
        Left = 10
        Top = 27
        Width = 21
        Height = 13
        Caption = 'Gain'
      end
      object Label5: TLabel
        Left = 314
        Top = 67
        Width = 52
        Height = 13
        Alignment = taRightJustify
        AutoSize = False
        Caption = '0'
      end
      object Label17: TLabel
        Left = 10
        Top = 67
        Width = 31
        Height = 13
        Caption = 'Attack'
      end
      object Label7: TLabel
        Left = 314
        Top = 107
        Width = 52
        Height = 13
        Alignment = taRightJustify
        AutoSize = False
        Caption = '0'
      end
      object Label19: TLabel
        Left = 10
        Top = 107
        Width = 38
        Height = 13
        Caption = 'Release'
      end
      object Label9: TLabel
        Left = 314
        Top = 147
        Width = 52
        Height = 13
        Alignment = taRightJustify
        AutoSize = False
        Caption = '0'
      end
      object Label21: TLabel
        Left = 10
        Top = 147
        Width = 47
        Height = 13
        Caption = 'Threshold'
      end
      object Label11: TLabel
        Left = 314
        Top = 187
        Width = 52
        Height = 13
        Alignment = taRightJustify
        AutoSize = False
        Caption = '0'
      end
      object Label23: TLabel
        Left = 10
        Top = 187
        Width = 25
        Height = 13
        Caption = 'Ratio'
      end
      object Label24: TLabel
        Left = 10
        Top = 227
        Width = 46
        Height = 13
        Caption = 'Pre Delay'
      end
      object Label13: TLabel
        Left = 314
        Top = 227
        Width = 52
        Height = 13
        Alignment = taRightJustify
        AutoSize = False
        Caption = '0'
      end
      object tbGain: TTrackBar
        Left = 64
        Top = 13
        Width = 257
        Height = 41
        Hint = '0 %'
        Max = 600
        Min = -600
        ParentShowHint = False
        Frequency = 25
        Position = 10
        ShowHint = False
        TabOrder = 0
        ThumbLength = 22
        TickMarks = tmBoth
        OnChange = tbGainChange
      end
      object tbAttack: TTrackBar
        Left = 64
        Top = 53
        Width = 257
        Height = 41
        Hint = '0 %'
        Max = 50000
        Min = 1
        ParentShowHint = False
        Frequency = 500
        Position = 10
        ShowHint = False
        TabOrder = 1
        ThumbLength = 22
        TickMarks = tmBoth
        OnChange = tbAttackChange
      end
      object tbRelease: TTrackBar
        Left = 64
        Top = 93
        Width = 257
        Height = 41
        Hint = '0 %'
        Max = 30000
        Min = 500
        ParentShowHint = False
        Frequency = 40
        Position = 500
        ShowHint = False
        TabOrder = 2
        ThumbLength = 22
        TickMarks = tmBoth
        OnChange = tbReleaseChange
      end
      object tbThreshold: TTrackBar
        Left = 64
        Top = 133
        Width = 257
        Height = 41
        Hint = '0 %'
        Max = 0
        Min = -600
        ParentShowHint = False
        Frequency = 13
        ShowHint = False
        TabOrder = 3
        ThumbLength = 22
        TickMarks = tmBoth
        OnChange = tbThresholdChange
      end
      object tbRatio: TTrackBar
        Left = 64
        Top = 173
        Width = 257
        Height = 41
        Hint = '0 %'
        Max = 1000
        Min = 10
        ParentShowHint = False
        Frequency = 20
        Position = 10
        ShowHint = False
        TabOrder = 4
        ThumbLength = 22
        TickMarks = tmBoth
        OnChange = tbRatioChange
      end
      object tbPreDelay: TTrackBar
        Left = 64
        Top = 213
        Width = 257
        Height = 41
        Hint = '0 %'
        Max = 40
        ParentShowHint = False
        ShowHint = False
        TabOrder = 5
        ThumbLength = 22
        TickMarks = tmBoth
        OnChange = tbPreDelayChange
      end
    end
  end
end
