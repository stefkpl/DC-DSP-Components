object frmDSPCompressor: TfrmDSPCompressor
  Left = 367
  Top = 142
  BorderStyle = bsNone
  Caption = 'frmDSPCompressor'
  ClientHeight = 496
  ClientWidth = 658
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
      Caption = ' Compressor '
      TabOrder = 0
      object Label1: TLabel
        Left = 10
        Top = 242
        Width = 72
        Height = 13
        Caption = 'Threshold: N/A'
      end
      object Label2: TLabel
        Left = 166
        Top = 242
        Width = 67
        Height = 13
        Alignment = taRightJustify
        Caption = 'Input Level: 0'
      end
      object Label4: TLabel
        Left = 281
        Top = 242
        Width = 85
        Height = 13
        Alignment = taRightJustify
        Caption = 'Gain Reduction: 0'
      end
      object Label6: TLabel
        Left = 320
        Top = 22
        Width = 46
        Height = 13
        Alignment = taRightJustify
        AutoSize = False
        Caption = '0'
      end
      object Label13: TLabel
        Left = 10
        Top = 22
        Width = 56
        Height = 13
        Caption = 'Attack Time'
      end
      object label8: TLabel
        Left = 320
        Top = 69
        Width = 46
        Height = 13
        Alignment = taRightJustify
        AutoSize = False
        Caption = '0'
      end
      object Label16: TLabel
        Left = 10
        Top = 69
        Width = 55
        Height = 13
        Caption = 'Decay Time'
      end
      object label3: TLabel
        Left = 320
        Top = 116
        Width = 46
        Height = 13
        Alignment = taRightJustify
        AutoSize = False
        Caption = '0'
      end
      object Label18: TLabel
        Left = 10
        Top = 116
        Width = 47
        Height = 13
        Caption = 'Threshold'
      end
      object label10: TLabel
        Left = 320
        Top = 163
        Width = 46
        Height = 13
        Alignment = taRightJustify
        AutoSize = False
        Caption = '0'
      end
      object Label20: TLabel
        Left = 10
        Top = 163
        Width = 25
        Height = 13
        Caption = 'Ratio'
      end
      object label12: TLabel
        Left = 320
        Top = 210
        Width = 46
        Height = 13
        Alignment = taRightJustify
        AutoSize = False
        Caption = '0'
      end
      object Label22: TLabel
        Left = 10
        Top = 210
        Width = 21
        Height = 13
        Caption = 'Gain'
      end
      object tbAttack: TTrackBar
        Left = 72
        Top = 8
        Width = 241
        Height = 41
        Hint = '0 %'
        Max = 3000
        Min = 50
        ParentShowHint = False
        Frequency = 100
        Position = 50
        ShowHint = False
        TabOrder = 0
        ThumbLength = 22
        TickMarks = tmBoth
        OnChange = tbAttackChange
      end
      object tbDecay: TTrackBar
        Left = 72
        Top = 55
        Width = 241
        Height = 41
        Hint = '0 %'
        Max = 3000
        Min = 10
        ParentShowHint = False
        Frequency = 100
        Position = 10
        ShowHint = False
        TabOrder = 1
        ThumbLength = 22
        TickMarks = tmBoth
        OnChange = tbDecayChange
      end
      object tbThreshold: TTrackBar
        Left = 72
        Top = 102
        Width = 241
        Height = 41
        Hint = '0 %'
        Max = 0
        Min = -10000
        ParentShowHint = False
        Frequency = 300
        ShowHint = False
        TabOrder = 2
        ThumbLength = 22
        TickMarks = tmBoth
        OnChange = tbThresholdChange
      end
      object tbRatio: TTrackBar
        Left = 72
        Top = 149
        Width = 241
        Height = 41
        Hint = '0 %'
        Max = 200
        Min = 10
        ParentShowHint = False
        Frequency = 6
        Position = 10
        ShowHint = False
        TabOrder = 3
        ThumbLength = 22
        TickMarks = tmBoth
        OnChange = tbRatioChange
      end
      object tbGain: TTrackBar
        Left = 72
        Top = 196
        Width = 241
        Height = 41
        Hint = '0 %'
        Max = 1000
        ParentShowHint = False
        Frequency = 35
        ShowHint = False
        TabOrder = 4
        ThumbLength = 22
        TickMarks = tmBoth
        OnChange = tbGainChange
      end
    end
  end
  object Timer1: TTimer
    Interval = 200
    OnTimer = Timer1Timer
    Left = 88
    Top = 306
  end
end
