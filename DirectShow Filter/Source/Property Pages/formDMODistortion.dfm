object frmDMODistortion: TfrmDMODistortion
  Left = 187
  Top = 83
  BorderStyle = bsNone
  Caption = 'frmDMODistortion'
  ClientHeight = 484
  ClientWidth = 778
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
      Caption = ' DMO Distortion '
      TabOrder = 0
      object Label3: TLabel
        Left = 322
        Top = 25
        Width = 52
        Height = 13
        Alignment = taRightJustify
        AutoSize = False
        Caption = '0'
      end
      object Label15: TLabel
        Left = 10
        Top = 25
        Width = 21
        Height = 13
        Caption = 'Gain'
      end
      object Label5: TLabel
        Left = 322
        Top = 65
        Width = 52
        Height = 13
        Alignment = taRightJustify
        AutoSize = False
        Caption = '0'
      end
      object Label17: TLabel
        Left = 10
        Top = 65
        Width = 24
        Height = 13
        Caption = 'Edge'
      end
      object Label7: TLabel
        Left = 322
        Top = 97
        Width = 52
        Height = 13
        Alignment = taRightJustify
        AutoSize = False
        Caption = '0'
      end
      object Label19: TLabel
        Left = 10
        Top = 97
        Width = 128
        Height = 13
        Caption = 'Post EQ Center Frequency'
      end
      object Label9: TLabel
        Left = 322
        Top = 153
        Width = 52
        Height = 13
        Alignment = taRightJustify
        AutoSize = False
        Caption = '0'
      end
      object Label21: TLabel
        Left = 10
        Top = 153
        Width = 91
        Height = 13
        Caption = 'Post EQ Bandwidth'
      end
      object Label11: TLabel
        Left = 322
        Top = 209
        Width = 52
        Height = 13
        Alignment = taRightJustify
        AutoSize = False
        Caption = '0'
      end
      object Label23: TLabel
        Left = 10
        Top = 209
        Width = 94
        Height = 13
        Caption = 'Pre Lowpass Cutoff'
      end
      object tbGain: TTrackBar
        Left = 56
        Top = 11
        Width = 273
        Height = 41
        Hint = '0 %'
        Max = 0
        Min = -600
        ParentShowHint = False
        Frequency = 20
        ShowHint = False
        TabOrder = 0
        ThumbLength = 22
        TickMarks = tmBoth
        OnChange = tbGainChange
      end
      object tbEdge: TTrackBar
        Left = 56
        Top = 51
        Width = 273
        Height = 41
        Hint = '0 %'
        Max = 1000
        ParentShowHint = False
        Frequency = 30
        Position = 10
        ShowHint = False
        TabOrder = 1
        ThumbLength = 22
        TickMarks = tmBoth
        OnChange = tbEdgeChange
      end
      object tbFrequency: TTrackBar
        Left = 3
        Top = 111
        Width = 374
        Height = 41
        Hint = '0 %'
        Max = 80000
        Min = 1000
        ParentShowHint = False
        Frequency = 40
        Position = 1000
        ShowHint = False
        TabOrder = 2
        ThumbLength = 22
        TickMarks = tmBoth
        OnChange = tbFrequencyChange
      end
      object tbBandwidth: TTrackBar
        Left = 3
        Top = 167
        Width = 374
        Height = 41
        Hint = '0 %'
        Max = 80000
        Min = 1000
        ParentShowHint = False
        Frequency = 2
        Position = 1000
        ShowHint = False
        TabOrder = 3
        ThumbLength = 22
        TickMarks = tmBoth
        OnChange = tbBandwidthChange
      end
      object tbCutoff: TTrackBar
        Left = 3
        Top = 223
        Width = 374
        Height = 41
        Hint = '0 %'
        Max = 80000
        Min = 1000
        ParentShowHint = False
        Frequency = 4
        Position = 1000
        ShowHint = False
        TabOrder = 4
        ThumbLength = 22
        TickMarks = tmBoth
        OnChange = tbCutoffChange
      end
    end
  end
end
